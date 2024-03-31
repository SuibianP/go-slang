import { Parser, TokenStream } from 'antlr4'

import GoParser from './GoParser'

export default abstract class GoParserBase extends Parser {
  protected constructor(input: TokenStream) {
    super(input)
  }

  protected closingBracket(): boolean {
    const la = this._input.LA(1)
    return la === GoParser.R_PAREN || la === GoParser.R_CURLY
  }
}
