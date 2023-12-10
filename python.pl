:- use_module(library(charsio)).
% :- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(assoc)).

% :- set_prolog_flag(double_quotes, chars).

parsing(String, Exprs) :- phrase(expressions(Exprs), String).

expressions([E|Es]) --> ws, expression(E), ws, !, expressions(Es).
expressions([]) --> [].

ws --> [W], { char_type(W, white) }, ws.
ws --> [].

% it is procedural, kinda
% which i mean...
% idk tbh

% choice point at []
hs(Val_) --> [W], { char_type(W, white)}, hs(Val), { Val_ is Val+1 }.
hs(0) --> [].

indent(Indentation) --> [W], { char_type(W, newline) }.

expression(s(A)) --> symbol(Cs), { atom_chars(A, Cs) }.
expression(n(N)) --> number(Cs), { number_chars(N, Cs) }.
expression(List) --> "(", expressions(List), ")".
expression([s(quote), Q]) --> "'", expression(Q).

number([D|Ds]) --> digit(D), number(Ds).
number([D]) --> digit(D).

digit(D) --> [D], { char_type(D, digit) }.

symbol([A|As]) --> [A], { memberchk(A, "+/-*><=") ; char_type(A, alpha) }, symbolr(As).

symbolr([A|As]) --> [A], { memberchk(A, "+/-*><=") ; char_type(A, alnum) }, symbolr(As).
symbolr([]) --> [].
