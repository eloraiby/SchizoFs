// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | DOT
  | RIGHT_PAREN of (TokenData)
  | LEFT_PAREN of (TokenData)
  | RIGHT_BRACK of (TokenData)
  | LEFT_BRACK of (TokenData)
  | RIGHT_BRACE of (TokenData)
  | LEFT_BRACE of (TokenData)
  | FALSE of (Node)
  | TRUE of (Node)
  | OPERATOR of (Node)
  | SYMBOL of (Node)
  | STRING of (Node)
  | REAL64 of (Node)
  | INT64 of (Node)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_DOT
    | TOKEN_RIGHT_PAREN
    | TOKEN_LEFT_PAREN
    | TOKEN_RIGHT_BRACK
    | TOKEN_LEFT_BRACK
    | TOKEN_RIGHT_BRACE
    | TOKEN_LEFT_BRACE
    | TOKEN_FALSE
    | TOKEN_TRUE
    | TOKEN_OPERATOR
    | TOKEN_SYMBOL
    | TOKEN_STRING
    | TOKEN_REAL64
    | TOKEN_INT64
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_prog
    | NONTERM_atom
    | NONTERM_exp
    | NONTERM_exp_member
    | NONTERM_exp_member_list
    | NONTERM_exp_list
    | NONTERM_paren_exp
/// This function maps integers indexes to symbolic token ids
val tagOfToken: token -> int

/// This function maps integers indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Node option) 