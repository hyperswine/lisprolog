:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(assoc)).
:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(pairs)).

% for swipl
% :- use_module(library(clpfd)).

markov_chain(Order, mc(Order,A)) --> { empty_assoc(A0), Order #>= 0, length(Firsts, Order) }, chars(Firsts), build_chain(Firsts, A0, A).

chars([]) --> [].
chars([Char | Chars]) --> [Char], chars(Chars).

generate(Chain, Seed, N, Ls) :- set_random(seed(Seed)), length(Ls, N), initial_sequence(Chain, Path), Chain = mc(_,A), once(phrase(generate_(Path, Chain, A), Ls)).

generate_(_, _, _)  --> [].
generate_(Path0, Chain, Assoc) -->
 ({ generate_element(Path0, Assoc, E) } -> [E], { append(Path0, [E], [_ | Path1]) } ; { initial_sequence(Chain, Path1) }),
 generate_(Path1, Chain, Assoc).

generate_element([], Assoc, Elem) :- atoms_with_weights(Assoc, List), choose_weighted(List, Elem).
generate_element([C|Cs], Assoc, Gen) :- get_assoc(C, Assoc, nc(_,Child)), generate_element(Cs, Child, Gen).

initial_sequence(mc(Order,A), Ls) :- length(Ls, Order), initial_(Ls, A).

initial_([], _).
initial_([L|Ls], Assoc) :- atoms_with_weights(Assoc, As), choose_weighted(As, L), get_assoc(L, Assoc, nc(_,Child)), initial_(Ls, Child).

atoms_with_weights(Assoc, As) :- assoc_to_list(Assoc, As0), maplist(atom_with_weight, As0, As).

atom_with_weight(A-nc(N,_), A-N).

choose_weighted(Ls0, Elem) :- transpose_pairs(Ls0, Sorted), weights_sum(Ls0, Sum), random_integer(0, Sum, X), choose(Sorted, 0, X, Elem).

choose([N-G|NGs], A0, X, Gen) :- A1 #= N + A0, A2 #= A1 - 1, (between(A0, A2, X) -> Gen = G ; choose(NGs, A1, X, Gen)).

transpose_pairs(Pairs0, Pairs) :- maplist(flip, Pairs0, Pairs1), keysort(Pairs1, Pairs).

flip(A-B, B-A).

build_chain(_, A, A) --> [].
build_chain(Path0, A0, A) -->
  [Char],
  { append(Path0, [Char], Path1), insert_path(Path1, A0, A1), Path1 = [_ | Path] },
  build_chain(Path, A1, A).

insert_path([], A, A).
insert_path([Char|Chars], A0, A) :-
  (get_assoc(Char, A0, nc(N0,Child0)) -> N #= N0 + 1 ; empty_assoc(Child0), N #= 1),
  put_assoc(Char, A0, nc(N,Child), A),
  insert_path(Chars, Child0, Child).

chain_entropy(mc(Order,A), E) :- length(Ls, Order), chain_entropy_(Ls, A, E0), E is -E0.

log_2(S, _-W, L) :- P is W/S, L0 is log(P) / log(2), L is P*L0.

weights_sum(Pairs, Sum) :- pairs_values(Pairs, Vs), sum(Vs, #=, Sum).

chain_entropy_([], A, E) :- atoms_with_weights(A, As), weights_sum(As, S), maplist(log_2(S), As, Logs), sum_list(Logs, E).
chain_entropy_([_|Rest], A, E) :- assoc_to_list(A, As0), maplist(atom_with_weight, As0, As), weights_sum(As, W), maplist(entropy_(Rest, W), As0, Ss), sum_list(Ss, E).

entropy_(Rest, W, _-nc(N,Child), S) :- P is N / W, chain_entropy_(Rest, Child, E), S is P * E.
