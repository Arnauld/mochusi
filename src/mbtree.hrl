% [ptr1, key1]
-record(tk_pair, {mbtree, key}).

% node: [ptr1, key1, ptr2, key2, last_ptr ]
%
%        [ | key1 | | key2 | ]
%        /        /         \
%      /        /            \
%    N1       N2              last_node
-record(mbtree, {tk_pairs=[], last_mbtree=nil}).
