#!/bin/sh

GRAMMARS="https://github.com/antlr/grammars-v4/raw/c7883a594bf622200c561db181848d277fcd49a7/golang/GoLexer.g4
          https://github.com/antlr/grammars-v4/raw/c7883a594bf622200c561db181848d277fcd49a7/golang/GoParser.g4"

# shellcheck disable=SC2086
wget $GRAMMARS -N -P ./generated/
antlr -o ./generated/ -Xexact-output-dir -Dlanguage=TypeScript -listener -visitor ./generated/GoLexer.g4 ./generated/GoParser.g4
sed -i.old "1s;^;\
/* eslint-disable */\n\
// @ts-nocheck\n\
;" ./generated/GoParser.ts ./generated/GoLexer.ts