Fun is a new programming language. The details of the language is as described below.

Previous Specifications:

https://github.com/paraguayrussianblue/fun-lexer/blob/main/README.md

Program Structure A program is a sequence of (mutually recursive) function and type declarations. The program structure is given by the following Backus–Naur form (BNF).

prog ::= decl ... decl
                                 
decl ::= fundecl

Function Declaration Structure:

A function declaration specifies the name of the function, its formal pa- rameter, parameter type, return type, and expression body. Functions are interpreted in call-by-value evaluation order. All functions must have different names (different functions may have the same formal parameter names).

fundecl ::= fun id ( id : tp ) : tp = exp 

A program must contain a declaration of the function main:
                           
fun main ( id : tp1 ) : tp2 = exp

where tp1 and tp2 are both int.

There is one predeclared (library) function printint of type int -> <>.

Type Structure:

Fun has the types of integer, n-ary tuples, single-argument/single-return function and ref- erence.

tp ::= int | <tp, ..., tp> | tp -> tp | tp ref | (tp)

The name “int” is not a reserved word, it is an ordinary identifier. The ref constructor has higher precedence (binds tighter) than the function type constructor (->). The function type constructor is right associative and the ref type constructor is left associative.

Expression Structure Expressions are evaluated in left-to-right order. 
un ::= - | not | ! | #i

bin ::= + | - | * | & | || | = | < | :=

exp ::= (exp) | id | num | exp; exp | un exp | exp bin exp | <exp, ..., exp> |
         exp ( exp ) | exp : tp | if exp then exp else exp | if exp then exp |
                    while exp do exp | let id = exp in exp | ref exp
                    
• Any expression may be parenthesized: (exp)

• Basic values include identifiers, <> (the unit value) and numbers. Sometimes numbers are interpreted as Boolean values (e.g., in logical operations, if statements, while loops). In this case, 0 should be interpreted as "false" and non-zero should be interpreted as "true."

• Binary arithmetic and logical operations include &, ||, =, <, +, -, and *. The & operator is short-circuit conjunction, and || is short-circuit disjunction. Equality (=) operates exclusively over integers. The assignment operator is :=.

• Unary operations include - (unary minus) and not (logical negation: 0 → 1; non-zero → 0). The unary operator #i extracts the i-th field from a tuple. Tuples are indexed starting with 0. The i in #i must a non-negative integer and #-0 is not legal.

• The operators ref, :=, and ! creates a reference, assigns a value to reference and dereferences a reference respectively.

• If expressions (if exp then exp else exp) execute the "then" branch if the first expression evaluates to a non-zero integer and execute the "else" branch if the first expression evaluates to 0. The else branch extends as far as possible. The else branch may be omitted, in which case it is equivalent to "else <>".

• n-ary tuples (n ≥ 0) are comma-separated sequences separated by angle-brackets: <exp,...,exp>.

• Precedence levels are as follows (1 is tightest binding):

1. #i, ref, function call, !, unary minus

2. 2. *

3. +, binary minus

4. =, <

5. not

6. &, ||

7. :

8. :=

9. if-then-else, do-while

10. ;

11. let-in

• All binary operators are left-associative.

• In a call expression, the function argument is surrounded by parentheses. Function call is left-associative
so f(x)y is (f(x))y.

• It is legal to use the name int as a function name, parameter name, or variable name; this does not
interfere with its use as a type name.

File Infos:

• lib/funpar.mly: parser specification

• lib/absyn.ml: abstract syntax tree definition

• lib/symbol.ml: symbol table definition

• lib/errormsg.ml, errormsg.mli: code for printing error message

• lib/funlex.ml: lexical analyzer for the Fun language

• lib/eval.ml, eval.mli: auxiliary code for interpreting a Fun program

• lib/heap.ml, heap.mli: auxiliary code for simulating heap memory

• lib/printer.ml, printer.mli: auxiliary code for printing a Fun program • data/*.fun : test programs in the Fun language
