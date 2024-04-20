import { CharStream, CommonTokenStream, FileStream, ParseTree, RuleContext } from 'antlr4'
import { Program, SVMFunction } from '../vm/svml-compiler'
import { assemble } from '../vm/svml-assembler'
import { runWithProgram } from '../vm/svml-machine'
import createContext from '../createContext'
import { OpCodes } from '../vm/opcodes'
import { stringifyProgram } from '../vm/util'
import GoParser, {
  AssignmentContext,
  BlockContext,
  ExpressionContext,
  ForStmtContext,
  FunctionDeclContext,
  FunctionLitContext,
  GoStmtContext,
  IfStmtContext,
  IntegerContext,
  OperandContext,
  OperandNameContext,
  PrimaryExprContext,
  ReturnStmtContext,
  SendStmtContext,
  SourceFileContext,
  String_Context,
  VarSpecContext
} from './generated/GoParser'
import GoParserVisitor from './generated/GoParserVisitor'
import GoLexer from './generated/GoLexer'

declare module './generated/GoParser' {
  export interface FunctionDeclContext {
    env: string[] // index is in table
  }

  export interface FunctionLitContext {
    env: string[] // index is in table
  }

  export interface SourceFileContext {
    env: string[] // index is in table
  }

  export interface IfStmtContext {
    env: string[] // index is in table
  }
}

declare module 'antlr4' {
  export interface RuleContext {
    getSym(sym: string): [index: number, envDiff: number]
    allocSym(sym: string): number
  }
}

const nextCtx = (
  ctx: RuleContext
): FunctionDeclContext | FunctionLitContext | SourceFileContext => {
  while (
    !(
      ctx instanceof FunctionLitContext ||
      ctx instanceof FunctionDeclContext ||
      ctx instanceof SourceFileContext
    )
  ) {
    if (!ctx?.parentCtx) {
      throw new Error(`No parent`)
    } else {
      ctx = ctx.parentCtx
    }
  }
  return ctx
}

// TODO NEWENV POPENV

RuleContext.prototype.getSym = function (sym: string): [index: number, envDiff: number] {
  // return ctx if current ctx is already
  let ctx = nextCtx(this)
  for (let envDiff = 0; ; ctx = nextCtx(ctx.parentCtx!), envDiff++) {
    const idx = ctx.env.indexOf(sym)
    if (idx == -1) continue
    return [idx, envDiff]
  }
}

RuleContext.prototype.allocSym = function (sym: string): number {
  // also store the global address if function
  if (!nextCtx(this).env) nextCtx(this).env = []
  return nextCtx(this).env.push(sym) - 1
}

/*
const envConsFac = (cons: Function): Function => (...args: any[]) => {
  console.log(this)
  console.assert((this as any) instanceof File); // FIXME
  (this as any).env = []
  return cons(...args)
}

FunctionLitContext.prototype.constructor = envConsFac(FunctionLitContext.prototype.constructor)
FunctionDeclContext.prototype.constructor = envConsFac(FunctionDeclContext.prototype.constructor)
 */

// expression: result push to stack
// function: compile function, load onto stack, if named get slot and store in symbol map

class GolangFuncVisitor extends GoParserVisitor<SVMFunction | null> {
  get EMPTY_FUNC(): SVMFunction {
    return [0, 0, 0, []]
  }

  // map from SVMFunction object to global addr
  funcs: SVMFunction[] = [] // index is global addr

  // we need this function: visit(ctx, instr, ctx, ...)

  visitVarSpec = (ctx: VarSpecContext): SVMFunction => {
    const init_expr = ctx.expressionList()?.expression_list()
    // for each of the identifier, allocate a slot, LGC the init value and STL to it
    const funcs = ctx
      .identifierList()
      .IDENTIFIER_list()
      .map((ident, index): SVMFunction => {
        const name = ident.getText()
        const idx = ctx.allocSym(name)
        if (init_expr?.[index]) {
          let expr_ins = this.visit(init_expr[index])
          expr_ins[3].push([OpCodes.STLG, idx])
          return expr_ins
        }
        return [...this.EMPTY_FUNC]
      })
    return this.combineSVMFunctions(...funcs)
  }

  visitIfStmt = (ctx: IfStmtContext) => {
    const pred = this.visit([ctx.simpleStmt(), ctx.expression()])
    const cons = this.visit(ctx.block(0))
    const alt = this.visit(ctx.block(1))
    if (!alt)
      return this.combineSVMFunctions(pred, [0, 0, 0, [[OpCodes.BRF, cons[3].length + 1]]], cons)
    // pred, BRF Alt, cons, BR End, Alt: alt, End:
    return this.combineSVMFunctions(
      pred,
      [0, 0, 0, [[OpCodes.BRF, cons[3].length + 2]]],
      cons,
      [0, 0, 0, [[OpCodes.BR, alt[3].length + 1]]],
      alt
    )
  }

  visitFunctionLit = (ctx: FunctionLitContext): SVMFunction => {
    // NEWC <addr> and STLG <slot> bind function literal to the name
    ctx.env ??= []
    let arg_cnt = 0
    ctx
      .signature()
      .parameters()
      .parameterDecl_list()
      .forEach(p =>
        p
          .identifierList()
          ?.IDENTIFIER_list()
          ?.forEach(i => ctx.allocSym(i.getText()), arg_cnt++)
      )
    let blk = this.visit(ctx.block()) // visit the body
    blk[0] = 20
    blk[1] = ctx.env.length
    blk[2] = arg_cnt
    blk[3].push([OpCodes.RETG])
    const addr = this.funcs.length
    this.funcs.push(blk)
    return [0, 0, 0, [[OpCodes.NEWC, addr]]]
  }

  // use the name if named function, otherwise
  visitFunctionDecl = (ctx: FunctionDeclContext): SVMFunction => {
    const l = this.visitFunctionLit(ctx)
    l[3].push([OpCodes.STLG, ctx.parentCtx!.allocSym(ctx.IDENTIFIER().getText())])
    return l
  }

  visitOperand = (ctx: OperandContext): SVMFunction | null => {
    if (
      ctx.parentCtx instanceof PrimaryExprContext &&
      ctx.parentCtx.parentCtx instanceof PrimaryExprContext &&
      ctx.parentCtx.parentCtx.arguments()
    ) {
      // this operand is called, may be function literal or variable
      const name = ctx.operandName()?.getText()
      if (!name) return this.visit(ctx.literal())
      if (name === 'display') {
        // T: tail; P: primitive V: internal
        // for now
        return [...this.EMPTY_FUNC]
      } else {
        return [
          0,
          0,
          0,
          [
            // (OpCodes.LDPG, index, envLevel) to get the closure
            [OpCodes.LDPG, ...ctx.getSym(name)]
          ]
        ]
      }
    }
    return this.visitChildren(ctx)
  }

  visitPrimaryExpr = (ctx: PrimaryExprContext): SVMFunction => {
    // first check if
    const name = ctx.primaryExpr()?.operand()?.operandName()?.getText()
    if (ctx.arguments()?.expressionList() || ctx.arguments()?.getChildCount() == 2) {
      // function call
      let res = this.visit(
        ctx?.parentCtx?.parentCtx instanceof GoStmtContext
          ? [ctx.arguments(), ctx.primaryExpr()]
          : [ctx.primaryExpr(), ctx.arguments()]
      ) // push function first
      if (name === 'display') {
        res[3].push([OpCodes.DISPLAY], [OpCodes.POPG])
      } else {
        res[3].push([OpCodes.CALL, ctx.arguments().expressionList()?.expression_list().length ?? 0])
      }
      return res
    } else if (name === 'make') {
      // make(chan something)
      if (!ctx?.arguments()?.type_()?.typeLit()?.channelType()) {
        throw new Error('make for non channel type')
      }
      return [0, 0, 0, [[OpCodes.LGCC]]]
    } else {
      // not invocation
      return this.visitChildren(ctx)!
    }
  }

  visitSendStmt = (ctx: SendStmtContext): SVMFunction => {
    let ins = this.visit(ctx.expression_list().slice().reverse())
    ins[3].push([OpCodes.CHAN, 0])
    return ins
  }

  visitExpression = (ctx: ExpressionContext) => {
    // TODO eval the operands first then the operator
    const UN_OP_MAP = new Map<number, OpCodes>([
      [GoLexer.EXCLAMATION, OpCodes.NOTG],
      [GoLexer.MINUS, OpCodes.NEGG],
      [GoLexer.RECEIVE, OpCodes.CHAN]
    ])
    const BIN_OP_MAP = new Map<number, OpCodes>([
      [GoLexer.MINUS, OpCodes.SUBG],
      [GoLexer.PLUS, OpCodes.ADDG],
      [GoLexer.MINUS, OpCodes.SUBG],
      [GoLexer.STAR, OpCodes.MULG],
      [GoLexer.DIV, OpCodes.DIVG],
      [GoLexer.MOD, OpCodes.MODG],
      [GoLexer.LESS, OpCodes.LTG],
      [GoLexer.GREATER, OpCodes.GTG],
      [GoLexer.LESS_OR_EQUALS, OpCodes.LEG],
      [GoLexer.GREATER_OR_EQUALS, OpCodes.GEG],
      [GoLexer.EQUALS, OpCodes.EQG],
      [GoLexer.NOT_EQUALS, OpCodes.NEQG]
    ])
    let res: SVMFunction
    const un_op = ctx._unary_op
    const bi_op = ctx._mul_op ?? ctx._add_op ?? ctx._rel_op ?? ctx.LOGICAL_AND() ?? ctx.LOGICAL_OR()
    const op = un_op ?? bi_op
    let opc: OpCodes | undefined
    if (op) {
      res = this.visit(ctx.expression_list())
      opc = (un_op ? UN_OP_MAP : BIN_OP_MAP).get(op.type)
      if (opc === undefined) throw new Error(`Unsupported operator ${op}`)
      res[3].push(opc === OpCodes.CHAN ? [opc, 1] : [opc])
    } else {
      res = this.visit(ctx.primaryExpr())
    }
    if (ctx.parentCtx instanceof BlockContext) res[3].push([OpCodes.POPG])
    return res
  }

  visitInteger = (ctx: IntegerContext): SVMFunction => {
    const str = ctx.getText()
    const num = parseInt(str)
    if (Number.isNaN(num)) {
      throw new Error(`${str} is not an integer.`)
    }
    return [0, 0, 0, [[OpCodes.LGCI, num]]]
  }

  visitString_ = (ctx: String_Context): SVMFunction => {
    return [
      0,
      0,
      0,
      [
        [
          OpCodes.LGCS,
          ctx.RAW_STRING_LIT()?.getText() ?? JSON.parse(ctx.INTERPRETED_STRING_LIT().getText())
        ]
      ]
    ]
  }

  visitOperandName = (ctx: OperandNameContext): SVMFunction => {
    // variable name used in expression
    const ident = ctx.IDENTIFIER().getText()
    return [0, 0, 0, [[OpCodes.LDPG, ...ctx.getSym(ident)]]]
  }

  visitGoStmt = (ctx: GoStmtContext): SVMFunction => {
    let ret = this.visit(ctx.expression())
    if (!ctx.expression()?.primaryExpr()?.arguments()) throw new Error('go statement not called')
    ret[3][ret[3].length - 1][0] = OpCodes.GO
    return ret
  }

  visitForStmt = (ctx: ForStmtContext): SVMFunction => {
    // cond, BRF, body, BR
    let cond = this.visit(ctx.expression() ?? ctx.forClause())
    let body = this.visit(ctx.block())
    return this.combineSVMFunctions(cond, [0, 0, 0, [[OpCodes.BRF, body[3].length + 2]]], body, [
      0,
      0,
      0,
      [[OpCodes.BR, 0 - body[3].length - cond[3].length - 1]]
    ])
  }

  visitAssignment = (ctx: AssignmentContext): SVMFunction => {
    const lhs = ctx.expressionList(0).expression_list() // lhs
    const rhs = ctx.expressionList(1).expression_list() // rhs
    return rhs.reduce(
      (prev, curr, idx) => {
        let ins = this.visitExpression(curr)
        ins[3].push([OpCodes.STPG, ...ctx.getSym(lhs[idx].getText())])
        return this.combineSVMFunctions(prev, ins)
      },
      [...this.EMPTY_FUNC]
    )
  }

  visitSourceFile = (ctx: SourceFileContext) => {
    ctx.env ??= []
    const glbl = this.visitChildren(ctx)!
    glbl[0] = 20 // TODO
    glbl[1] = ctx.env.length
    glbl[2] = 0 // no args
    glbl[3].push([OpCodes.LDLG, ctx.getSym('main')[0]], [OpCodes.CALL, 0], [OpCodes.RETG])
    this.funcs.push(glbl)
    return null
  }

  visitReturnStmt = (ctx: ReturnStmtContext): SVMFunction => {
    let res = this.visitChildren(ctx) ?? this.EMPTY_FUNC
    res[3].push([OpCodes.RETG])
    return res
  }

  visitChildren(node: any) {
    if (!node.children) return null
    return this.visit(node.children)
  }

  visit(tree: ParseTree | ParseTree[] | any): SVMFunction {
    // remove nullish values (e.g. from keyword nodes) and empty arrays (e.g. empty block)
    // for array of functions we aggregate them into one
    if (Array.isArray(tree)) {
      const res = tree.map(i => this.visit(i)).filter(i => i ?? null)
      return this.combineSVMFunctions(...res) // empty scenario is handled inside
    } else {
      return (tree as any)?.accept(this)
    }
  }

  protected combineSVMFunctions(...funcs: SVMFunction[]): SVMFunction {
    return funcs.reduce(
      (prev, curr): SVMFunction => {
        prev[1] += curr[1]
        prev[2] += curr[2]
        prev[3] = prev[3].concat(curr[3])
        return prev
      },
      [...this.EMPTY_FUNC]
    ) // otherwise the object is modified
  }
}

export function compile(program: SourceFileContext): Program {
  const visitor = new GolangFuncVisitor()
  program.accept(visitor)
  return [visitor.funcs.length - 1, visitor.funcs]
}

let input = `
package main

var i = 0;

func loop() {
  for 1 {
    i = i + 1
    display(i, "")
  }
}

func main() {
  go loop()
  go loop()
}
`

const stream = process.argv[2] ? new FileStream(process.argv[2]) : new CharStream(input)
const lexer = new GoLexer(stream)
const tokens = new CommonTokenStream(lexer)
const parser = new GoParser(tokens)
const tree = parser.sourceFile()
const ins = compile(tree)
// console.dir(ins, {depth: null})
console.log(stringifyProgram(ins))
// assemble(ins)
assemble
const ctx = createContext()
runWithProgram(ins, ctx)
