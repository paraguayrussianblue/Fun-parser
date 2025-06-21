%{
  module A = Absyn
  module S = Symbol

  let start_pos = Parsing.symbol_start
  let end_pos   = Parsing.symbol_end
%}

%start prog

%token COMMA SEMICOLON COLON
%token LPAREN RPAREN
%token PLUS MINUS TIMES
%token LT EQ GT
%token AND NOT OR
%token WHILE DO REF BANG ASSIGN
%token IF THEN ELSE
%token LET IN FUN ARROW UMINUS
%token EOF
%token <string> ID
%token <int>    NUM PROJ

%type <Absyn.prog> prog
%type <Absyn.exp> exp
%type <Absyn.tp>  tp

%right LET IN
%nonassoc ELSE DO
%left SEMICOLON
%left ASSIGN
%left COLON
%left AND OR
%right NOT
%left EQ LT GT
%left PLUS MINUS
%left TIMES
%left COMMA
%left GROUP
%left UMINUS REF BANG APP
%right ARROW

%%

prog:
  | fundecl_list EOF   
      { $1 }
;

fundecl_list:
  | fundecl fundecl_list   
      { $1 :: $2 }
  | fundecl                
      { [$1] }
;

fundecl:
  | FUN ID LPAREN ID COLON tp RPAREN COLON tp EQ exp
      {
        ((start_pos(), end_pos()),
        ((S.symbol $2, S.symbol $4, $6, $9, $11)))
      }
;

tp:
  | ID                    
      { 
        if $1 = "int" then A.Inttp
        else failwith ("Unknown type: " ^ $1)}
  | LT tp_list GT  
      { A.Tupletp($2) }
  | tp ARROW tp          
      { A.Arrowtp($1, $3) }
  | tp REF               
      { A.Reftp($1) }
  | LPAREN tp RPAREN     
      { $2 }
;

tp_list:
  | tp COMMA tp_list     
      { $1 :: $3 }
  | tp                   
      { [$1] }
  | /* empty */          
      { [] }
;

exp:
  | exp_seq %prec ELSE                 
      { $1 }

exp_seq:
  | exp_seq SEMICOLON exp_ctrl  
      {
        A.Pos((start_pos(), end_pos()),
              A.Let(S.symbol "_", $1, $3))
      }
  | exp_ctrl                     
      { $1 }

exp_ctrl:
  | IF exp THEN exp 
      { A.Pos((start_pos(), end_pos()), A.If($2, $4, A.Tuple [])) }
  | IF exp THEN exp ELSE exp
      { A.Pos((start_pos(), end_pos()), A.If($2, $4, $6)) }
  | LET ID EQ exp IN exp_ctrl 
    { A.Pos((start_pos(), end_pos()), A.Let(S.symbol $2, $4, $6)) }
  | WHILE exp DO exp
      { A.Pos((start_pos(), end_pos()), A.While($2, $4))}
  | exp_assign
      { $1 }
;
  
exp_assign:
  | exp_assign ASSIGN exp_ascr   
      {
        A.Pos((start_pos(), end_pos()),
              A.Op(A.Set, [$1; $3]))
      }
  | exp_ascr                    
      { $1 }

exp_ascr:
  | exp_ascr COLON tp    
      {
        A.Pos((start_pos(), end_pos()),
              A.Constrain($1, $3))
      }
  | exp_logic            
      { $1 }

exp_logic:
  | exp_logic AND exp_logic_not   
      {
        A.Pos((start_pos(), end_pos()),
              A.If($1, $3, A.Int 0))
      }
  | exp_logic OR exp_logic_not    
      {
        A.Pos((start_pos(), end_pos()),
              A.If($1, A.Int 1, $3))
      }
  | exp_logic_not                
      { $1 }

exp_logic_not:
  | NOT exp_logic_not   
      {
        A.Pos((start_pos(), end_pos()),
              A.If($2, A.Int 0, A.Int 1))
      }
  | exp_cmp %prec EQ             
      { $1 }

exp_cmp:
  | exp_cmp EQ exp_sum   
      {
        A.Pos((start_pos(), end_pos()),
              A.Op(A.Eq, [$1; $3]))
      }
  | exp_cmp LT exp_sum   
      {
        A.Pos((start_pos(), end_pos()),
              A.Op(A.LT, [$1; $3]))
      }
  | exp_cmp GT exp_sum   
      {
        A.Pos((start_pos(), end_pos()),
              A.Op(A.LT, [$3; $1]))
      }
  | exp_sum             
      { $1 }

exp_sum:
  | exp_sum PLUS exp_term   
      {
        A.Pos((start_pos(), end_pos()),
              A.Op(A.Add, [$1; $3]))
      }
  | exp_sum MINUS exp_term  
      {
        A.Pos((start_pos(), end_pos()),
              A.Op(A.Sub, [$1; $3]))
      }
  | exp_term               
      { $1 }

exp_term:
  | exp_term TIMES exp_factor   
      {
        A.Pos((start_pos(), end_pos()),
              A.Op(A.Mul, [$1; $3]))
      }
  | exp_factor                 
      { $1 }

exp_factor:
  | MINUS exp_factor %prec UMINUS  
      {
        A.Pos((start_pos(), end_pos()),
              A.Op(A.Sub, [A.Int 0; $2]))
      }
  | BANG exp_factor               
      {
        A.Pos((start_pos(), end_pos()),
              A.Op(A.Get, [$2]))
      }
  | REF exp_factor                
      {
        A.Pos((start_pos(), end_pos()),
              A.Op(A.Ref, [$2]))
      }
  | PROJ exp_factor               
      {
        A.Pos((start_pos(), end_pos()),
              A.Proj($1, $2))
      }
  | primary                      
      { $1 }

tuple_cont:
  | exp COMMA tuple_cont   
      { $1 :: $3 }
  | exp                    
      { [$1] }

primary:
  | primary LPAREN exp RPAREN %prec APP  
      {
        A.Pos((start_pos(), end_pos()),
              A.Call($1, $3))
      }
  | ID                         
      { A.Pos((start_pos(), end_pos()), A.Id(S.symbol $1)) }
  | NUM                        
      { A.Pos((start_pos(), end_pos()), A.Int $1) }
  | LT GT                      
      { A.Pos((start_pos(), end_pos()), A.Tuple []) }
  | LT exp COMMA tuple_cont GT %prec COMMA
      { A.Pos((start_pos(), end_pos()), A.Tuple($2 :: $4)) }
  | LPAREN exp RPAREN %prec GROUP
      { $2 }
;
%%

