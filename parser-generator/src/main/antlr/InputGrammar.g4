grammar InputGrammar;

start : 'grammar' word SEMICOLON rules_section tokens_section;

word : RULE_NAME | TOKEN_NAME | TYPE_OR_NAME;

tokens_section : token+;

token : TOKEN_NAME COLON token_expr SEMICOLON;

token_expr : TOKEN_DEF | REGEX;

rules_section : just_rule+;

just_rule : RULE_NAME definition COLON rule_body SEMICOLON;

definition : input_attrs return_expr | return_expr;

input_attrs : '[' attr (',' attr)* ']';

return_expr : 'returns' input_attrs |;

attr : word word;

rule_body : rule_expr ('|' rule_expr)*;

rule_expr : expr_name+;

expr_name : RULE_NAME expr_attrs expr_code
          | TOKEN_NAME expr_code
          | EPS expr_code;

expr_attrs : EXPR_ATTR |;

expr_code : CODE |;

COLON : ':';
SEMICOLON : ';';
EPS : '#';

RULE_NAME : [a-z] [a-z0-9_]*;
TOKEN_NAME : [A-Z] [A-Z0-9_]*;
TYPE_OR_NAME : [a-zA-Z] [a-zA-Z0-9_]*;

CODE : '{' ~('\r' | '\n')* '}';
TOKEN_DEF : '\'' ~('\r' | '\n')* '\'';
REGEX : '`' ~('\r' | '\n')* '`';
EXPR_ATTR : '(' ~('\r' | '\n')* ')';

WS : [ \t\r\n]+ -> skip;