import { CharStream, CommonTokenStream, ParseTree, RuleContext } from 'antlr4'
import { Program, SVMFunction } from '../vm/svml-compiler'
import { assemble } from '../vm/svml-assembler'
import { runWithProgram } from '../vm/svml-machine'
import createContext from '../createContext'
import OpCodes from '../vm/opcodes'
import { stringifyProgram } from '../vm/util'
import GoParser, {
  ExpressionContext,
  FunctionDeclContext,
  FunctionLitContext,
  IntegerContext,
  OperandContext,
  OperandNameContext,
  PrimaryExprContext,
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
    if (!ctx.parentCtx) {
      throw new Error('No parent')
    } else {
      ctx = ctx.parentCtx
    }
  }
  return ctx
}

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
  readonly EMPTY_FUNC: SVMFunction = [0, 0, 0, []]
  // map from SVMFunction object to global addr
  funcs: SVMFunction[] = [] // index is global addr

  // we need this function: visit(ctx, instr, ctx, ...)

  visitVarSpec = (ctx: VarSpecContext) => {
    const init_expr = ctx.expressionList().expression_list()
    // for each of the identifier, allocate a slot, LGC the init value and STL to it
    const funcs = ctx
      .identifierList()
      .IDENTIFIER_list()
      .map((ident, index): SVMFunction => {
        const name = ident.getText()
        const idx = ctx.allocSym(name)
        if (init_expr[index]) {
          let expr_ins = this.visit(init_expr[index])
          expr_ins[3].push([OpCodes.STLG, idx])
          return expr_ins
        }
        return [...this.EMPTY_FUNC]
      })
    return this.combineSVMFunctions(...funcs)
  }

  // use the name if named function, otherwise
  visitFunctionDecl = (ctx: FunctionDeclContext): SVMFunction => {
    // NEWC <addr> and STLG <slot> bind function literal to the name
    const name = ctx.IDENTIFIER().getText()
    let arg_cnt = 0
    ctx
      .signature()
      .parameters()
      .parameterDecl_list()
      .forEach(p =>
        p
          .identifierList()
          .IDENTIFIER_list()
          .forEach(i => ctx.allocSym(i.getText()), arg_cnt++)
      )
    const idx = ctx.parentCtx!.allocSym(name) // index in symbol table in parent
    let blk = this.visit(ctx.block()) // visit the body
    blk[0] = 20
    blk[1] = 20
    blk[2] = arg_cnt
    blk[3].push([OpCodes.RETG])
    const addr = this.funcs.length
    this.funcs.push(blk)
    return [
      0,
      0,
      arg_cnt,
      [
        [OpCodes.NEWC, addr],
        [OpCodes.STLG, idx]
      ]
    ]
  }

  visitOperand = (ctx: OperandContext): SVMFunction | null => {
    if (
      ctx.parentCtx instanceof PrimaryExprContext &&
      ctx.parentCtx.parentCtx instanceof PrimaryExprContext &&
      ctx.parentCtx.parentCtx.arguments()
    ) {
      // this operand is called
      const name = ctx.operandName().getText()
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

  visitPrimaryExpr = (ctx: PrimaryExprContext) => {
    // first check if
    if (ctx.arguments()) {
      // function call
      // TODO combine the two and return
      let res = this.visit([ctx.primaryExpr(), ctx.arguments()]) // push function first
      if (ctx.primaryExpr()?.operand().operandName().getText() === 'display') {
        res[3].push([OpCodes.DISPLAY])
      } else {
        res[3].push([OpCodes.CALL, ctx.arguments().expressionList().expression_list().length])
      }
      return res
    } else {
      return this.visitChildren(ctx)
    }
  }

  visitExpression = (ctx: ExpressionContext) => {
    // TODO eval the operands first then the operator
    const UN_OP_MAP = new Map<number, OpCodes>([
      [GoLexer.EXCLAMATION, OpCodes.NOTG],
      [GoLexer.MINUS, OpCodes.NEGG]
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
    const un_op = ctx._unary_op
    const bi_op = ctx._mul_op ?? ctx._add_op ?? ctx._rel_op ?? ctx.LOGICAL_AND() ?? ctx.LOGICAL_OR()
    const op = un_op ?? bi_op
    let opc: OpCodes | undefined
    if (op) {
      let res = this.visit(ctx.expression_list())
      opc = (un_op ? UN_OP_MAP : BIN_OP_MAP).get(op.type)
      if (opc === undefined) throw new Error(`Unsupported operator ${op}`)
      res[3].push([opc])
      return res
    } else {
      return this.visit(ctx.primaryExpr())
    }
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

  visitSourceFile = (ctx: SourceFileContext) => {
    const glbl = this.visitChildren(ctx)!
    glbl[0] = 20 // TODO
    glbl[1] = 20
    glbl[2] = 0 // no args
    glbl[3].push([OpCodes.LDLG, ctx.getSym('main')[0]], [OpCodes.CALL, 0], [OpCodes.RETG])
    this.funcs.push(glbl)
    return null
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
      return (tree as any).accept(this)
    }
  }

  // if a node returns a list of functions, all functions except for the first is to be propagated
  // a node will never return a list of children results. either SVMFunction[] or undefined
  // for childrens, flatten the propagation part and aggregate the firsts
  // assignment of func literal to identifier is done at the varspec level
  // (visit expression, get a function declaration and obtain its index, assign it to the variable slot)

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

const input = `
package main
func another(str string) {
    var s string = "world"
    display(str + ", " + s, "")
}
func main() {
    display("hello?", "eh")
    var h = "hello"
    var i = -3 + 12 * 17
    another(h)
}
`
const chars = new CharStream(input) // replace this with a FileStream as required
const lexer = new GoLexer(chars)
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
