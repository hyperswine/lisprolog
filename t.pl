:- set_prolog_flag(double_quotes, chars).
% :- set_prolog_flag(double_quotes, codes).

:- use_module(library(dcg/basics)).


% SO BASICALLY IT ACCUMULATES ON [Name]
% Like [Name1] [Name2] [Name3
% which then gets concatenated together...

% LOOPS if you try to generate!
% tree_nodes(nil) --> [].
% tree_nodes(node(Name, L, R)) --> tree_nodes(L), [Name], tree_nodes(R).

% doesnt loop cause not left recursive
tree_nodes(nil, Ls, Ls) --> [].
tree_nodes(node(Name, Left, Right), [_ | Ls0], Ls) --> tree_nodes(Left, Ls0, Ls1), [Name], tree_nodes(Right, Ls1, Ls).

safe_divide(X, Y, Result) :-
  catch(Result is X / Y, Error, handle_error(Error)).

% wow?
handle_error(Error) :-
  ( Error = division_by_zero -> writeln('Cannot divide by zero')
  ; writeln('Unknown error occurred') ).

% cant declare types
% well you can kinda

type(nil).
% type(node(Data, Left, Right)).
type(node(data, left, right)).

expr(a).
expr(b).
expr(A + B) :- expr(A), expr(B).

% as --> [].
% as --> [a], as.

% order matters in these cases
as --> [a], as.
as --> [].

% non terminating due to left recursion
% arith_expr --> "1".
% arith_expr --> arith_expr, "+", arith_expr.

% arith_expr --> "1", arith_expr_r.
% arith_expr_r --> [].
% arith_expr_r --> "+", arith_expr.

% can only call these when at least one terminal is present, with the _
% arith_expr([_ | Rs], Rs) --> "1".
% arith_expr([_ | Rs], Rs_prime) --> arith_expr(Rs, Rs1), "+", arith_expr(Rs1, Rs_prime).

arith_expr(1, [_ | Rs], Rs) --> "1".
arith_expr(A+B, [_ | Rs], Rs_prime) --> arith_expr(A, Rs, Rs1), "+", arith_expr(B, Rs1, Rs_prime).

% that means 0 or more though right? unless they also want to use it for like a trick
% word --> [].
% word --> [C], { char_type(C, alpha) }, word.

word([C | Cs]) --> [C], { char_type(C, alpha) }, word(Cs).
word([]) --> [].

% word --> [C], { char_type(C, alphabetic) }, word.
% word --> [C], { char_type(C, is_alpha) }, word.

words([]) --> [].
words([W | Ws]) --> ws, word(W), ws, words(Ws).

% ws --> [] | [W], { char_type(W, white) }, ws.

ws --> [W], { char_type(W, white) }, ws | [].
