signature Tiger_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val RIGHT_B:  'a * 'a -> (svalue,'a) token
val LEFT_B:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val LE:  'a * 'a -> (svalue,'a) token
val GE:  'a * 'a -> (svalue,'a) token
val L:  'a * 'a -> (svalue,'a) token
val G:  'a * 'a -> (svalue,'a) token
val NE:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val SUB:  'a * 'a -> (svalue,'a) token
val ADD:  'a * 'a -> (svalue,'a) token
val NIL:  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val STR: (string) *  'a * 'a -> (svalue,'a) token
val INT: (string) *  'a * 'a -> (svalue,'a) token
end
signature Tiger_LRVALS=
sig
structure Tokens : Tiger_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
