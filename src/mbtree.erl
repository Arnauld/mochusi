-module(mbtree).

-include("mbtree.hrl").

%%
%
%
%
%%

-record(kp, {ptr, key}).

-record(kp_node, {pairs}).

-record(kv, {key, values}).

-record(kv_node, {pairs}).

%%
%
% 
%
%%

new() -> {tree, nil}.

%%
%
%
%
%%
insert(nil, _, _) -> throw({invalid_tree, nil});
insert(_, nil, _) -> throw({invalid_key, nil});
insert({tree, KP}, Key, Value) ->
	case insert_1(KP, Key, Value) of
		{ok, NewNode} -> {tree, NewNode};
		{split, SplitKey, Left, Right} -> {tree, 
			#kp_node{pairs=[
						#kp{ptr=Left, key=SplitKey},
						last_bucket(Right)]}}
	end.

%
% empty case
%
insert_1(nil, Key, Value) -> 
	KV = #kv{key=Key, values=[Value]},
	{ok, #kv_node{pairs=[KV]}};

%
% non-leaf cases
%
insert_1(KV=#kp_node{pairs=Pairs}, Key, Value) ->
	{KP, ReverseTraversed, Tail} = traverse_kp_pairs(Key, [], Pairs),
	% io:format("~w [KP=~p, Key=~p, Pairs=~p] ~n", [?LINE, KP, Key, Pairs]),
	NewPairs = case insert_1(KP#kp.ptr, Key, Value) of
		{ok, NewNode} -> 
			Reversed = lists:reverse(Tail, [KP#kp{ptr=NewNode} | ReverseTraversed]),
			lists:reverse(Reversed)
			;
		{split, SplitKey, Left, Right} -> 
			NewKP  = #kp{key=SplitKey, ptr=Left},
			% leaf child has been splitted: generate a new bucket
			case Tail of 
				[] ->
					% KP is the last, simply discard it
					LastKP = last_bucket(Right),
					Reversed = lists:reverse([LastKP], [NewKP | ReverseTraversed]),
					lists:reverse(Reversed);
				[Next,Tail1] ->
					% replace Next ptr
					NewNext = Next#kp{ptr=Right},
					Reversed = lists:reverse(Tail1, [NewNext | [NewKP | ReverseTraversed]]),
					lists:reverse(Reversed)
			end
		end,
	create_kp_node(NewPairs);

%
% leaf cases
%
insert_1(KV=#kv_node{pairs=Pairs}, Key, Value) ->
	Pairs1 = insert_pair(Key, Value, [], Pairs),
	case is_overloaded(Pairs1) of 
		{Len, true} ->
			% leaf node must be splitted
			{Left, Right=[H|T]} = lists:split(Len div 2 +1, Pairs1),
			{split, H#kv.key, #kv_node{pairs=Left}, #kv_node{pairs=Right}}
			;
		{_, false} -> 
			{ok, #kv_node{pairs=Pairs1}}
	end.

%
%
last_bucket(Ptr) -> #kp{key=last, ptr=Ptr}.

%
% insert leaf utilities
%
insert_pair(Key, Value, ReverseTraversed, []) -> 
	lists:reverse([#kv{key=Key, values=[Value]} | ReverseTraversed]);
insert_pair(Key, Value, ReverseTraversed, [KVH=#kv{key=KH} | T]) when KH < Key ->
	insert_pair(Key, Value, [KVH|ReverseTraversed], T);
insert_pair(Key, Value, ReverseTraversed, [KVH=#kv{key=KH} | T]) when KH > Key ->
	% append T in reverse order, thus a full reverse can be applied at end
	Reversed = lists:reverse(T, [KVH | [#kv{key=Key, values=[Value]} | ReverseTraversed]]),
	lists:reverse(Reversed);
insert_pair(Key, Value, ReverseTraversed, [KVH=#kv{key=KH, values=VH} | T]) when KH =:= Key ->
	% append T in reverse order, thus a full reverse can be applied at end
	Reversed = lists:reverse(T, [KVH#kv{values=[Value|VH]} | ReverseTraversed]),
	lists:reverse(Reversed).

%
% insert non-leaf utilities
%
create_kp_node(Pairs) ->
	case is_overloaded(Pairs) of 
		{Len, true} ->
			% node must be splitted
			{Left, Right=[H|T]} = lists:split(Len div 2, Pairs),
			NewLeftPairs = Left ++ [last_bucket(H#kp.ptr)],
			NewLeftNode  = #kp_node{pairs=NewLeftPairs},
			NewRightNode = #kp_node{pairs=T},
			{split, H#kp.key, NewLeftNode, NewRightNode}
			;
		{_, false} ->
			{ok, #kp_node{pairs=Pairs}}
	end.
	
%
%
%
traverse_kp_pairs(Key, ReverseTraversed, [Last]) -> 
	{Last, ReverseTraversed, []};
traverse_kp_pairs(Key, ReverseTraversed, [KPH=#kp{key=KH}|T])  when KH < Key -> 
	traverse_kp_pairs(Key, [KPH|ReverseTraversed], T);
traverse_kp_pairs(Key, ReverseTraversed, [KPH|T])  -> %when KH >= Key -> 
	{KPH, ReverseTraversed, T}.

%
%
%
is_overloaded(Nodes) ->
	Len = length(Nodes), 
	{Len, Len>4}.
	
%
%
%
traverse_in_order({tree, Tree}, Fun, Arg) ->
	traverse_in_order_1(Tree, Fun, Arg).

traverse_in_order_1(#kv{key=Key, values=Values}, Fun, Arg) ->
	Fun(Key, Values, Arg);

traverse_in_order_1(#kp{ptr=Ptr}, Fun, Arg) ->
	traverse_in_order_1(Ptr, Fun, Arg);

traverse_in_order_1(#kp_node{pairs=Pairs}, Fun, Arg) ->
	traverse_in_order_1(Pairs, Fun, Arg);

traverse_in_order_1(#kv_node{pairs=Pairs}, Fun, Arg) ->
	traverse_in_order_1(Pairs, Fun, Arg);

traverse_in_order_1([H|T], Fun, Arg) ->
	Arg1 = traverse_in_order_1(H, Fun, Arg),
	traverse_in_order_1(T, Fun, Arg1);

traverse_in_order_1([], Fun, Arg) -> Arg.


%%
%
% Tests
%
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
	Tree = new(),
	Expected = {tree,nil},
	?assertEqual(Expected, Tree),
	ok.
	
nil_key_test() ->
	Tree = new(),
	?assertException(throw, {invalid_key, _}, insert(Tree, nil, "Bob")),
	ok.

nil_tree_test() ->
	Tree = new(),
	?assertException(throw, {invalid_tree, _}, insert(nil, 11, "Bob")),
	ok.

insert_pair_ex1_test() ->
	Res0 = insert_pair(17, "McCallum", [], 
		[
			#kv{key=19, values=["Travis"]},
			#kv{key=23, values=["Pacman"]}
		]),
	% unpack to check value
	[#kv{key=K1, values=[V1]}, #kv{key=K2, values=[V2]}, #kv{key=K3, values=[V3]}] = Res0,
	?assertEqual(17,K1),
	?assertEqual("McCallum",V1),
	?assertEqual(19,K2),
	?assertEqual("Travis",V2),
	?assertEqual(23,K3),
	?assertEqual("Pacman",V3),
	ok.

insert_pair_ex2_test() ->
	Res0 = insert_pair(19, "Travis", [], 
		[
			#kv{key=17, values=["McCallum"]},
			#kv{key=23, values=["Pacman"]}
		]),
	% unpack to check value
	[#kv{key=K1, values=[V1]}, #kv{key=K2, values=[V2]}, #kv{key=K3, values=[V3]}] = Res0,
	?assertEqual(17,K1),
	?assertEqual("McCallum",V1),
	?assertEqual(19,K2),
	?assertEqual("Travis",V2),
	?assertEqual(23,K3),
	?assertEqual("Pacman",V3),
	ok.
	
insert_pair_ex3_test() ->
	Res0 = insert_pair(23, "Pacman", [], 
		[
			#kv{key=17, values=["McCallum"]},
			#kv{key=19, values=["Travis"]}
		]),
	% unpack to check value
	[#kv{key=K1, values=[V1]}, #kv{key=K2, values=[V2]}, #kv{key=K3, values=[V3]}] = Res0,
	?assertEqual(17,K1),
	?assertEqual("McCallum",V1),
	?assertEqual(19,K2),
	?assertEqual("Travis",V2),
	?assertEqual(23,K3),
	?assertEqual("Pacman",V3),
	ok.


insert_empty_tree_test() ->
	Tree0 = new(),
	Tree1 = insert(Tree0, 17, "McCallum"),
	% unpack to check value
	{tree, KV=#kv_node{pairs=Values}} = Tree1,
	[#kv{key=K, values=[V]}] = Values,
	?assertEqual(17, K),
	?assertEqual("McCallum", V),
	ok.
	
insert_root_leaf_ascending_2_items_test() ->
	Tree0 = new(),
	Tree1 = insert(Tree0, 17, "McCallum"),
	Tree2 = insert(Tree1, 19, "Travis"),
	% unpack to check value
	{tree, KV=#kv_node{pairs=Values}} = Tree2,
	[#kv{key=K1, values=[V1]}, #kv{key=K2, values=[V2]}] = Values,
	?assertEqual(17, K1),
	?assertEqual("McCallum", V1),
	?assertEqual(19, K2),
	?assertEqual("Travis", V2),
	ok.

insert_root_leaf_ascending_3_items_test() ->
	Tree0 = new(),
	Tree1 = insert(Tree0, 17, "McCallum"),
	Tree2 = insert(Tree1, 19, "Travis"),
	Tree3 = insert(Tree2, 23, "Pacman"),
	% unpack to check value
	{tree, KV=#kv_node{pairs=Values}} = Tree3,
	[#kv{key=K1, values=[V1]}, #kv{key=K2, values=[V2]}, #kv{key=K3, values=[V3]}] = Values,
	?assertEqual(17, K1),
	?assertEqual("McCallum", V1),
	?assertEqual(19, K2),
	?assertEqual("Travis", V2),
	?assertEqual(23, K3),
	?assertEqual("Pacman", V3),
	ok.
	
insert_root_leaf_ascending_4_items_test() ->
	Tree0 = new(),
	Tree1 = insert(Tree0, 17, "McCallum"),
	Tree2 = insert(Tree1, 19, "Travis"),
	Tree3 = insert(Tree2, 23, "Pacman"),
	Tree4 = insert(Tree3, 29, "Vlad"),
	% unpack to check value
	{tree, KV=#kv_node{pairs=Values}} = Tree4,
	[#kv{key=K1, values=[V1]}, #kv{key=K2, values=[V2]}, #kv{key=K3, values=[V3]}, #kv{key=K4, values=[V4]}] = Values,
	?assertEqual(17, K1),
	?assertEqual("McCallum", V1),
	?assertEqual(19, K2),
	?assertEqual("Travis", V2),
	?assertEqual(23, K3),
	?assertEqual("Pacman", V3),
	?assertEqual(29, K4),
	?assertEqual("Vlad", V4),
	ok.

insert_root_leaf_descending_test() ->
	Tree0 = new(),
	Tree1 = insert(Tree0, 19, "Travis"),
	Tree2 = insert(Tree1, 17, "McCallum"),
	% unpack to check value
	{tree, KV=#kv_node{pairs=Values}} = Tree2,
	[#kv{key=K1, values=[V1]}, #kv{key=K2, values=[V2]}] = Values,
	?assertEqual(17, K1),
	?assertEqual("McCallum", V1),
	?assertEqual(19, K2),
	?assertEqual("Travis", V2),
	ok.
	
insert_root_leaf_descending_4_items_test() ->
	Tree0 = new(),
	Tree1 = insert(Tree0, 29, "Vlad"),
	Tree2 = insert(Tree1, 23, "Pacman"),
	Tree3 = insert(Tree2, 19, "Travis"),
	Tree4 = insert(Tree3, 17, "McCallum"),
	% unpack to check value
	{tree, KV=#kv_node{pairs=Values}} = Tree4,
	[#kv{key=K1, values=[V1]}, #kv{key=K2, values=[V2]}, #kv{key=K3, values=[V3]}, #kv{key=K4, values=[V4]}] = Values,
	?assertEqual(17, K1),
	?assertEqual("McCallum", V1),
	?assertEqual(19, K2),
	?assertEqual("Travis", V2),
	?assertEqual(23, K3),
	?assertEqual("Pacman", V3),
	?assertEqual(29, K4),
	?assertEqual("Vlad", V4),
	ok.

insert_root_leaf_filled_test() ->
	Tree = sample_tree(),
	% unpack to check value
	{tree, KP=#kp_node{pairs=Values}} = Tree,
	?assertEqual(23, 24),
	ok.
	
sample_tree() ->
	Tree0 = new(),
	Tree1 = insert(Tree0, 29, "Vlad"),
	Tree2 = insert(Tree1, 23, "Pacman"),
	Tree3 = insert(Tree2, 19, "Travis"),
	Tree4 = insert(Tree3, 17, "McCallum"),
	Tree5 = insert(Tree4, 11, "Thundercat"),
	Tree6 = insert(Tree5, 31, "Terry"),
	Tree7 = insert(Tree6, 37, "cyberneurs"),
	Tree8 = insert(Tree7, 39, "Oslo"),
	Tree9 = insert(Tree8, 41, "Naoko"),
	TreeA = insert(Tree9, 43, "Bugg"),
	TreeB = insert(TreeA, 47, "Earp"),
	TreeC = insert(TreeB, 49, "Russel"),
	TreeD = insert(TreeC, 7, "Kirkwood"),
	TreeE = insert(TreeD, 51, "Deux Ex Machina"),
	TreeF = insert(TreeE, 53, "Pendjab"),
	TreeG = insert(TreeF, 57, "Mazzere"),
	TreeG.

traverse_in_order_test () ->
	Tree = sample_tree(),
	Collected = traverse_in_order(Tree, 
		fun(Key, Values, Acc) ->
			% ?debugFmt("~n ~p --> ~p ~n", [Key, Values]),
			[Key|Acc]
		end, []),
	Expected = [57,53,51,49,47,43,41,39,37,31,29,23,19,17,11,7],
	?assertEqual(Expected, Collected),
	ok.

-endif.
