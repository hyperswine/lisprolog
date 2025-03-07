:- module(alzint, [run/1, step/1]).

:- use_module(library(assoc)).
:- use_module(library(clpfd)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
% :- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

seq([])     --> [].
seq([E|Es]) --> [E], seq(Es).

interpret(step, Stmts, Prog, Env0, Env) :- step(Stmts, Prog, [], Env0, Env).
interpret(run, Stmts, Prog, Env0, Env)  :- run(Stmts, Prog, Env0, Env).

step([], _, _, Env, Env).
step([S0|Ss0], Prog, Undo0, Env0, Env) :-
  phrase(unfold_seqs([S0|Ss0]), [S|Ss]),
  question(Prog, S, Env0, Choice),
  ( Choice = c ->
      S = stm(Line,Stm),
      phrase(step_(Stm, Line, Env0, Env1), Rest, Ss),
      step(Rest, Prog, [[S|Ss]-Env0|Undo0], Env1, Env)
    ; Choice = u ->
      ( Undo0 = [Us-UEs|Undo1] ->
          step(Us, Prog, Undo1, UEs, Env)
        ; format("~nnothing to undo~2n", []), step([S|Ss], Prog, Undo0, Env0, Env)
      )
    ; Choice = q -> halt
    ; Choice = r -> run([S|Ss], Prog, Env0, Env)
    ; format("invalid choice~2n", []), step([S|Ss], Prog, Undo0, Env0, Env)
  ).

run([], _, Env, Env).
run([S0|Ss0], Prog, Env0, Env) :- phrase(unfold_seqs([S0|Ss0]), [stm(Line,Stm)|Ss]), display_program(Prog, Line), print_env(Env0), print_dashes, nl, nl, nl, phrase(step_(Stm, Line, Env0, Env1), Rest, Ss), run(Rest, Prog, Env1, Env).

print_dashes :- format("~`-t~70|~n", []).

unfold_seqs([S|Ss]) --> ({ S = stm(_,sequence(A,B)) } -> unfold_seqs([A,B|Ss]) ; seq([S|Ss])).

question(Prog, stm(Current,_), Env0, Choice) :- display_program(Prog, Current), print_env(Env0), print_dashes, format("(c)ontinue (u)ndo (r)un (q)uit: ", []), get_single_char(Choice), nl, print_dashes.

step_(if(Cond,Then,Else), _, Env, Env) --> { eval(Cond, Env, Value) }, ({ Value #\= 0 } -> [Then] ; [Else]).
step_(assign(Var, Expr), _, Env0, Env) --> { eval(Expr, Env0, Value), env_put_var(Var, Value, Env0, Env) }.
step_(while(Cond, Body), Line, Env, Env) --> { eval(Cond, Env, Value) }, ({ Value #\= 0 } -> [Body,stm(Line,while(Cond,Body))] ; []).

eval(bin(Op,A,B), Env, Value) :- eval(A, Env, VA), eval(B, Env, VB), eval_(Op, VA, VB, Value).
eval(v(V), Env, Value) :- (env_get_var(Env, V, Value) -> true ; format("~2nvariable '~w' not in environment.~3n", [V]), halt ).
eval(n(N), _, N).
eval(uminus(F), Env, Value) :- eval(F, Env, V), Value #= -V.

eval_(+, A, B, V)   :- V #= A + B.
eval_(-, A, B, V)   :- V #= A - B.
eval_(*, A, B, V)   :- V #= A * B.
eval_(/, A, B, V)   :- V #= A // B.
eval_(=, A, B, V)   :- (A #= B -> V = 1 ; V = 0).
eval_(>, A, B, V)   :- ( A #> B -> V = 1 ; V = 0).
eval_(<, A, B, V)   :- ( A #< B -> V = 1 ; V = 0).
eval_(leq, A, B, V) :- ( A #=< B -> V = 1 ; V = 0).
eval_(geq, A, B, V) :- ( A #>= B -> V = 1 ; V = 0).
eval_(neq, A, B, V) :- ( A #\= B -> V = 1 ; V = 0).
eval_(and, A, B, V) :- V #= min(A, B).
eval_(or, A, B, V)  :- V #= max(A, B).

lex_analysis(Chars, Tokens) :- phrase(tokens(0,Tokens), Chars).

tokens(N0, Ts) --> whitespace, tokens(N0, Ts).
tokens(N0, Ts) --> newline, { N1 #= N0 + 1 }, tokens(N1, Ts).
tokens(N0, [T|Ts]) --> token(T, N0), !, tokens(N0, Ts).
tokens(_, []) --> "".

token(';', _) --> ";".
token(',', _) --> ",".
token('(', _) --> "(".
token(')', _) --> ")".

token(rop(=), _)    --> "=".
token(rop(neq), _)  --> "!=".
token(t(N,'<-'), N) --> "<-".
token(rop(leq), _)  --> "<=".
token(rop(geq), _)  --> ">=".
token(rop(<), _)    --> "<".
token(rop(>), _)    --> ">".

token(aop(+), _) --> "+".
token(aop(-), _) --> "-".

token(mop(*), _) --> "*".
token(mop(/), _) --> "/".

token(lop(and), _) --> "&&".
token(lop(or), _)  --> "||".

token(t(N,Token), N) --> ident(Cs), { atom_chars(I, Cs), (keyword(I) -> Token = I ; Token = id(I)) }.
token(num(N), _) --> number(Cs), { number_chars(N, Cs) }.

ident([C|Cs]) --> letter(C), identr(Cs).

identr([C|Cs]) --> letter(C), identr(Cs).
identr([C|Cs]) --> digit(C), identr(Cs).
identr([])     --> [].

number([C|Cs]) --> digit(C), number(Cs).
number([C])    --> digit(C).

letter(C)  --> [C], { char_type(C, alpha) }.

digit(C)   --> [C], { char_type(C, decimal_digit) }.

whitespace --> [C], { char_type(C, whitespace), dif(C, '\n') }.

newline    --> ['\n'].

keyword(K) :- member(K, [if,then,else,begin,end,while,do,'I']).

syn_analysis(Tokens, Tree, Env) :- env_new(Env0), phrase(prog(Tree,Env0,Env), Tokens).

prog(Tree, Env0, Env) --> [t(_,'I')], ['('], [t(_, id(ID))], [')'], [rop(=)], int_exp(IE), { eval(IE, Env0, Value), env_put_var(ID, Value, Env0, Env1) }, prog(Tree, Env1, Env).
prog(Tree, Env, Env) --> alz(Tree).

alz(stm(N,assign(Id, E)))  --> [t(_,id(Id))], [t(N,'<-')], exp(E).
alz(stm(N,if(Cond,S1,S2))) --> [t(N,if)], cond(Cond), [t(_,then)], alz(S1), [t(_,else)], alz(S2).
alz(stm(N,while(Cond, S))) --> [t(N,while)], cond(Cond), [t(_,do)], alz(S).
alz(stm(N,sequence(A,B)))  --> [t(N,begin)], alz(A), [';'], alz(B), [t(_,end)].

cond(C)       --> rel(C1), cond_r(C1, C).
cond_r(C1, C) --> [lop(and)], cond(C2), cond_r(bin(and,C1,C2), C).
cond_r(C1, C) --> [lop(or)], cond(C2), cond_r(bin(or,C1,C2), C).
cond_r(E, E)  --> [].

rel(bin(Op,A,B)) --> exp(A), [rop(Op)], exp(B).
rel(C)           --> ['('], cond(C), [')'].

exp(E)       --> term(E1), exp_r(E1, E).
exp_r(E1, E) --> [aop(Op)], term(E2), exp_r(bin(Op, E1, E2), E).
exp_r(E, E)  --> [].

term(E)       --> factor(E1), term_r(E1, E).
term_r(E1, E) --> [mop(Op)], factor(E2), term_r(bin(Op, E1, E2), E).
term_r(E, E)  --> [].

factor(n(N))      --> [num(N)].
factor(v(Id))     --> [t(_,id(Id))].
factor(E)         --> ['('], exp(E), [')'].
factor(uminus(F)) --> [aop(-)], factor(F).

int_exp(E)       --> int_term(E1), int_exp_r(E1, E).
int_exp_r(E1, E) --> [aop(Op)], int_term(E2), int_exp_r(bin(Op, E1, E2), E).
int_exp_r(E, E)  --> [].

int_term(E)       --> int_factor(E1), int_term_r(E1, E).
int_term_r(E1, E) -->
        [mop(Op)], int_factor(E2), int_term_r(bin(Op, E1, E2), E).
int_term_r(E, E)  --> [].

int_factor(n(N))      --> [num(N)].
int_factor(E)         --> ['('], int_exp(E), [')'].
int_factor(uminus(F)) --> [aop(-)], int_factor(F).

display_program(prog(Lines,Displace), Current0) :-
        Current #= Current0 - Displace,
        foldl(display_line(Current), Lines, 0, _),
        nl.

display_line(Current, Line, N0, N) :-
        N #= N0 + 1,
        (   N0 #= Current ->
            format(" ==>  ~s~n", [Line])
        ;   format("      ~s~n", [Line])
        ).

env_new(E) :- empty_assoc(E).

env_put_var(Name, Value, Env0, Env) :- put_assoc(Name, Env0, Value, Env).

env_get_var(Env, Name, Value) :- get_assoc(Name, Env, Value).

print_env(Env) :- assoc_to_list(Env, Ls), format("     ", []), maplist(print_pair, Ls), nl.

print_pair(Var-Value) :- format("~w = ~w   ", [Var,Value]).

lines([]) --> [].
lines([L|Ls]) --> { phrase(upto_nl(Line), [L|Ls], Rest) }, [Line], lines(Rest).

upto_nl([])     --> "\n", !.
upto_nl([L|Ls]) --> [L], !, upto_nl(Ls).
upto_nl([])     --> [].

run(File, Option) :- phrase_from_file(seq(Chars), File), run_(Chars, Option).

run_(Chars, Option) :-
  ( lex_analysis(Chars, Tokens) ->
    ( syn_analysis(Tokens, Tree, Env0) ->
        nl, nl, Tree = stm(First,_), phrase(lines(Chars), Lines), length(Drop, First), append(Drop, Program, Lines),
        interpret(Option, [Tree], prog(Program,First), Env0, Env), nl, print_env(Env), nl, nl
        ; format("syntax error~n", [])
    ),
    halt
  ;  format("lexical error", []),
    halt
  ).

step(File) :- run(File, step).
run(File) :- run(File, run).
