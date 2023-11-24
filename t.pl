% SO BASICALLY IT ACCUMULATES ON [Name]
% Like [Name1] [Name2] [Name3
% which then gets concatenated together...

% LOOPS if you try to generate!
% tree_nodes(nil) --> [].
% tree_nodes(node(Name, L, R)) --> tree_nodes(L), [Name], tree_nodes(R).

% doesnt loop cause not left recursive
tree_nodes(nil, Ls, Ls) --> [].
tree_nodes(node(Name, Left, Right), [_|Ls0], Ls) --> tree_nodes(Left, Ls0, Ls1), [Name], tree_nodes(Right, Ls1, Ls).
