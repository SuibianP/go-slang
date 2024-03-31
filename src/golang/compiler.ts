import { CharStream, CommonTokenStream } from 'antlr4'

import { Program } from '../vm/svml-compiler'
import GoLexer from './generated/GoLexer'
import GoParser, {
  FunctionDeclContext,
  SourceFileContext,
  VarSpecContext
} from './generated/GoParser'
import GoParserVisitor from './generated/GoParserVisitor'

class GolangVisitor extends GoParserVisitor<void> {
  visitVarSpec = (ctx: VarSpecContext) => {
    console.log(
      `Declaring variables ${ctx
        .identifierList()
        .IDENTIFIER_list()
        .map(x => x.getText())}`
    )
  }
  visitFunctionDecl = (ctx: FunctionDeclContext) => {
    console.log(`Declaring function ${ctx.IDENTIFIER().getText()}`)
    this.visit(ctx.block())
  }
}

export function compileToIns(program: SourceFileContext): Program | void {
  return program.accept(new GolangVisitor())
}

const input = `
package main
func main() {
    var h, w string = "hello", "world"
    Println(h + " " + w) // builtin
}
`
const chars = new CharStream(input) // replace this with a FileStream as required
const lexer = new GoLexer(chars)
const tokens = new CommonTokenStream(lexer)
const parser = new GoParser(tokens)
const tree = parser.sourceFile()
compileToIns(tree)
