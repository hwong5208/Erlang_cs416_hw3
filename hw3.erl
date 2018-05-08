-module(hw3).

-export([distance/2, distance_combine/2, distance_examples/0]).
-export([biggest_distance_ser/1, bds/3, biggest_distance_ser_examples/0]).
-export([max_distance_tuple/2]).
-export([biggest_distance_par/2, biggest_distance_par_examples/1]).
-export([bdp_leaf/1, bdp_combine/2, bdp_root/1]).

-export([running_count_par/3, running_count_par_examples/1]).
-export([rcp_leaf1/1, rcp_leaf2/4, rcp_combine/2, rcp_root/1]).
-export([merge_map_with_fun/3, mmwf/3]).
-export([running_count_ser/1, rcs/3, running_count_ser_examples/0]).
-export([print_sorted_by_count/1, print_running_count/1]).

-export([big_number/0]).
-export([random_vector_list/2, rvl/3, time/0,count_for_run_time/3]).
-export([you_need_to_write_this/2, you_need_to_write_this/3]).

%-----------------------------------------------------------------
%-----------------------------------------------------------------
% CODE FOR QUESTION 1
%-----------------------------------------------------------------
%-----------------------------------------------------------------

%-------------------------------------------------------------------
% distance(ListX, ListY) -> float 
%
%   Returns the (2-norm) distance between two vectors:
%   sqrt(sum_i(pow(x_i - y_i, 2))).  X and Y are two vectors in some
%   high dimensional space represented as Erlang lists.  Note that
%   ListX and ListY must have the same length.
%
% This function is already written for you using standard Erlang
% functions.  You need to write the helper function distance_combine().
distance(ListX, ListY) ->
    math:sqrt(
      lists:sum(
	lists:zipwith(fun(Xi, Yi) -> distance_combine(Xi, Yi) end, 
		      ListX, ListY))).

% distance_combine(Xi, Yi) -> float
distance_combine(Xi, Yi) when is_number(Xi), is_number(Yi) ->
    math:pow((Xi-Yi),2).
%-------------------------------------------------------------------
distance_examples() ->
    % Create some vectors on which to test.
    { V10, V11, V12 } = { [0.0], [2.0], [-5.5] },
    { V20, V21, V22, V23, V24 } = 
	{ [0.0, 0.0], [0.0, -1.0], [1.0, 1.0], [-1.0, -1.0], [5, 12] },
    { V30, V31, V32 } = 
	{ [0.0, 0.0, 0.0], [1.0, 1.0, 1.0], [-1, +1, -1] },
    { VN0, VN1, VN2 } = 
	{ lists:duplicate(big_number(), 0.0), 
	  lists:duplicate(big_number(), 1.0), 
	  lists:seq(1, big_number()) },
    % The examples are pairs of vectors.
    Examples = 
	[ { [], [] },   % -> 0.0.
	  { V10, V11 }, % -> 2.0.
	  { V10, V12 }, % -> 5.5.
	  { V20, V21 }, % -> 1.0.
	  { V20, V22 }, % -> 1.414...
	  { V20, V23 }, % -> 1.414...
	  { V22, V23 }, % -> 2.828...
	  { V20, V24 }, % -> 13.00...
	  { V30, V31 }, % -> 1.732...
	  { V30, V32 }, % -> 1.732...
	  { V31, V32 }, % -> 2.828...
	  { VN0, VN1 }, % -> sqrt(N) which is 100.0 for N = 10000
	  { VN0, VN2 }, % -> sqrt(N^3/3+N^2/2+N/6) which is 577393.57...
	  { V10, V20 }  % should raise an error.
	],
    Try = fun(LX, LY) ->
		  try
		      distance(LX, LY)
		  catch _:_ -> error
		  end
	  end,
    [     io:format("distance(~W, ~W) -> ~w~n", 
		    [ LX, 5, LY, 5, Try(LX, LY)])
	  ||  { LX, LY } <- Examples
    ].


%-------------------------------------------------------------------
% biggest_distance_ser(List) -> { Distance, Index }
%
%   Returns the largest distance between two consective vectors in the
%   list, and the index of the first of those two vectors.  If more
%   than one pair of vectors has the same largest distance, return the
%   index of the first element of the first pair.  List is a list of
%   vectors, where each vector is represented as a list.  If List is
%   empty or contains only 1 vector, return the tuple { none, none }.
%   Note that all vectors in List must have the same length.
%biggest_distance_ser(List) when is_list(List) ->
%    you_need_to_write_this(biggest_distance_ser, 
%			   "call helper bds()", List).

biggest_distance_ser(List) when is_list(List) -> 
bds(List,{none,none},1).


% Base cases for helper function.
%bds([], BigTuple, CurrIndex) ->
%    you_need_to_write_this(bds, "empty list", 
% 			   [BigTuple, CurrIndex]);
% bds([X], BigTuple, CurrIndex) ->
%     you_need_to_write_this(bds, "singleton list", 
% 			   [X, BigTuple, CurrIndex]);

bds([], BigTuple, _CurrIndex) -> BigTuple;
bds([_X], BigTuple, _CurrIndex) ->  BigTuple;

% If we haven't seen a biggest distance yet and we have two elements
% at the start of the list, this must be the biggest distance seen so
% far.
%bds([X, Y | Tl], { none, none }, 1) ->
%    you_need_to_write_this(bds, 
%			   "first pair in list of two or more elements",
%			   [X,Y,Tl]);
bds([X, Y | Tl], { none, none }, 1) ->
   Distance = distance(X,Y),
   bds([Y|Tl],{Distance,1},2);

% Otherwise, if we have two elements at the start of the list, compare
% their distance to the previous biggest.
%
% You may find the function max_distance_tuple() useful.
%bds([X, Y | Tl], BigTuple, CurrIndex) ->
%    you_need_to_write_this(bds, "list of two or more elements", 
%			   [X, Y, Tl, BigTuple, CurrIndex]).

bds([X, Y | Tl], BigTuple, CurrIndex) ->
   Distance = distance(X,Y),
   Cur = {Distance,CurrIndex},
   {New_distance,New_index} = max_distance_tuple(Cur,BigTuple),
    bds([Y|Tl],{New_distance,New_index},CurrIndex+1).


%-------------------------------------------------------------------
biggest_distance_ser_examples() ->
    % Create some vectors on which to test.
    { V10, V11, V12 } = { [0.0], [2.0], [-5.5] },
    { V20, V21, V22, V23, V24 } = 
	{ [0.0, 0.0], [0.0, -1.0], [1.0, 1.0], [-1.0, -1.0], [5, 12] },
    { V30, V31, V32 } = 
	{ [0.0, 0.0, 0.0], [1.0, 1.0, 1.0], [-1, +1, -1] },
    { VN0, VN1, VN2 } = 
	{ lists:duplicate(big_number(), 0.0), 
	  lists:duplicate(big_number(), 1.0), 
	  lists:seq(1, big_number()) },
    % The examples are pairs of vectors.
    Examples =
	[ [V10, V11, V12], % -> { 7.5, 2 }.
	  [V20, V21, V23, V22, V24, V20], % -> { 13.0, 5 }.
	  [V31, V30, V30, V32, V30], % -> { 1.732.., 1 }.
	  [VN1, VN0, VN2],  % -> { 577393.57, 2 }.
	  [VN1], % -> { none, none }.
	  [], % -> { none, none }.
	  [V10, V20], % -> should raise an error.
	  % Furthest distance between corners of a box [ -1, +1 ] in D
	  % dimensions is 2 * sqrt(D), but as dimension increases the
	  % chances of picking points in those corners decreases so
	  % the maxima will be further from the theoretical bound.
	  random_vector_list(big_number(), 1), % -> ~2.0
	  random_vector_list(big_number(), 2), % -> ~2.828...
	  random_vector_list(big_number(), 3), % -> ~3.464...
	  random_vector_list(big_number(), 5), % -> ~4.472...
	  random_vector_list(big_number(), 49) % -> ??
	],
    Try = fun(List) ->
		  try
		      biggest_distance_ser(List)
		  catch _:_ -> error
		  end
	  end,
    [     io:format("biggest_distance_ser(~W) -> ~w~n", 
		    [List, 5, Try(List)])
	  ||  List <- Examples
    ].


%-------------------------------------------------------------------
% max_distance_tuple({Dist1, Index1}, {Dist2, Index2}) -> 
%    {float, int} or {none, none}
%
%   Compares two {distance, index} tuples and returns the one with the
%   larger distance.  If one tuple contains {none, none}, returns the
%   other tuple (which may also be {none, none}).  If both tuples
%   contain the same distance, returns the smaller index.
max_distance_tuple({none, none}, Tuple2) -> Tuple2; 
max_distance_tuple(Tuple1, {none, none}) -> Tuple1; 
max_distance_tuple({ Dist1, Index1}, {Dist2, Index2}) 
  when is_number(Dist1), is_integer(Index1), 
       is_number(Dist2), is_integer(Index2) -> 
    { Dist, Index } = if (Dist1 == Dist2) ->
			      { Dist1, min(Index1, Index2) };
			 (Dist1 > Dist2) -> {Dist1, Index1};
			 (Dist2 > Dist1) -> {Dist2, Index2}
		      end,
    { Dist, Index }.


%------------------------------------------------------------------
% biggest_distance_par(W, ListKey) -> { Distance, Index }
%
%   Returns the largest distance between two consective vectors in the
%   list, and the index of the first of those two vectors.  If more
%   than one pair of vectors has the same largest distance, return the
%   index of the first element of the first pair.  W is a list of
%   workers on which the list is distributed.  ListKey the key under
%   which the workers can find their local list of vectors, where each
%   vector is represented as a list.  If the list is empty or contains
%   only 1 vector, return the tuple { none, none }.  Note that all
%   vectors in the list must have the same length.
% biggest_distance_par(W, ListKey) ->
%     you_need_to_write_this(biggest_distance_par, 
% 			   "biggest distance parallel reduce", 
% 			   [W, ListKey]).

biggest_distance_par(W, ListKey) ->
  wtree:reduce(W,
    fun(ProcState)->bdp_leaf(wtree:get(ProcState,ListKey))end,
    fun(Left,Right)->bdp_combine(Left,Right)end,
    fun(RootSummary)->bdp_root(RootSummary)end).


%-------------------------------------------------------------------
% Don't forget to handle the case when the leaf node has an empty
% list.  You will need to make sure that whatever you return in that
% case can be handled by bdp_combine().
%bdp_leaf(List) ->
%    you_need_to_write_this(bdp_leaf, "biggest distance leaf", List).

bdp_leaf([])->{none,{none,none},none,0};
bdp_leaf(List) when is_list(List)->	
{hd(List),biggest_distance_ser(List),lists:last(List),length(List)}.



%-------------------------------------------------------------------
%bdp_combine(Left, Right) ->
%    you_need_to_write_this(bdp_leaf, 
%			   "biggest distance combine", 
%			   [Left, Right]).

bdp_combine({none,{none,none},none,0}, Right) -> Right;
bdp_combine(Left,{none,{none,none},none,0}) -> Left;

bdp_combine(Left,Right)->
  {LHead,LDist,LEnd,LLength} = Left,
  {RHead,RDist,REnd,RLength} = shiftNumber(Right,LLength),
  Mid = {distance(LEnd,RHead),LLength},
  Max1 = max_distance_tuple(Mid,LDist),
  Max2 =  max_distance_tuple(Max1,RDist),
  {LHead,Max2,REnd,LLength+RLength } .    

shiftNumber(Seg = {_,{none,none},_,_},_)-> Seg;  % need for p>2
shiftNumber({L,{Distance,Index},R,Length},Offset)->{L,{Distance,Index+Offset},R,Length}.

%-------------------------------------------------------------------
%bdp_root(RootSummary) ->    
%    you_need_to_write_this(bdp_leaf, 
%			   "biggest distance root", RootSummary).
bdp_root( {_LHead,Max,_REnd,_Length}) ->Max.


%-------------------------------------------------------------------
biggest_distance_par_examples(NumWorkers) ->
    % Create some vectors on which to test.
    { V10, V11, V12 } = { [0.0], [2.0], [-5.5] },
    { V20, V21, V22, V23, V24 } = 
	{ [0.0, 0.0], [0.0, -1.0], [1.0, 1.0], [-1.0, -1.0], [5, 12] },
    { V30, V31, V32 } = 
	{ [0.0, 0.0, 0.0], [1.0, 1.0, 1.0], [-1, +1, -1] },
    { VN0, VN1, VN2 } = 
	{ lists:duplicate(big_number(), 0.0), 
	  lists:duplicate(big_number(), 1.0), 
	  lists:seq(1, big_number()) },
    % The examples are pairs of vectors.
    Examples =
	[ [V10, V11, V12], % -> { 7.5, 2 }.
	  [V20, V21, V23, V22, V24, V20], % -> { 13.0, 5 }.
	  [V31, V30, V30, V32, V30], % -> { 1.732.., 1 }.
	  [VN1, VN0, VN2],  % -> { 577393.57, 2 }.
	  [VN1], % -> { none, none }.
	  [], % -> { none, none }.
	  [V10, V20], % -> should raise an error.
	  % Furthest distance between corners of a box [ -1, +1 ] in D
	  % dimensions is 2 * sqrt(D), but as dimension increases the
	  % chances of picking points in those corners decreases so
	  % the maxima will be further from the theoretical bound.
	  random_vector_list(big_number(), 1), % -> ~2.0
	  random_vector_list(big_number(), 2), % -> ~2.828...
	  random_vector_list(big_number(), 3), % -> ~3.464...
	  random_vector_list(big_number(), 5), % -> ~4.472...
	  random_vector_list(big_number(), 49) % -> << 14.0
	],
    W = wtree:create(NumWorkers),
    Try = fun(List) ->
		  try
		      CutList = misc:cut(List, NumWorkers),
		      workers:update(W, testList, 
				     fun(_ProcState, LocalList) -> 
					     LocalList end, 
				     CutList),
		      biggest_distance_par(W, testList)
		  catch _:_ -> error
		  end
	  end,
    Result = [     io:format("biggest_distance_par(~W) -> ~w~n", 
			     [List, 5, Try(List)])
		   ||  List <- Examples
	     ],
    % Make sure all of that previous work is done before killing the
    % workers.
    wtree:barrier(W),
    wtree:reap(W),
    Result.

%-------------------------------------------------------------------
% random_vector_list(N, D) -> List of N vectors of dimension D.
%
%   Each vector is itself a list of length D.  Each element of each
%   vector is a random float between -1.0 and +1.0.
random_vector_list(N, D) when is_integer(N), N >= 0, is_integer(D) ->
    rvl(N, D, []).

rvl(0, _D, L) -> L;
rvl(N, D, L) when is_integer(N), N > 0->
    NewVector = lists:map(fun(X) -> X - 1.0 end, misc:rlist(D, 2.0)),
    rvl(N-1, D, [ NewVector | L ]).


%-----------------------------------------------------------------
%-----------------------------------------------------------------
% CODE FOR QUESTION 2
%-----------------------------------------------------------------
%-----------------------------------------------------------------

%-----------------------------------------------------------------
% running_count_ser(ListIn) -> ListOut
%
%   Computes the running count of appearences of elements in ListIn.
%   Element i of ListOut is a map containing one key for each unique
%   element in ListIn elements 0...i.  The value corresponding to that
%   key is the number of elements 0...i of ListIn that were that key.
%
%   When writing the code for this function and its helper, you may
%   find the functions maps:new(), maps:get(), maps:map(), maps:put(),
%   maps:update() and/or maps:update_with() helpful.
running_count_ser(List) when is_list(List) -> 
    % We reverse the result of rcs() because in order to make rcs()
    % tail recursive it produces the output list in reversed order.
    lists:reverse(rcs(List, maps:new(), [])).

% rcs(ListIn, CurrCountMap, ListOfRunningCountsRev) -> 
%      ListOfRunningCountsRev
%
%   Helper function for running_count_ser().  ListIn is the not yet
%   processed part of the input list.  CurrCountMap is the current
%   histogram count map for elements of the input list that have been
%   processed.  ListOfRunningCountsRev is the list of running
%   histogram count maps for elements of the input list that have been
%   processed, but in reverse order (so that rcs() can be tail
%   recursive).
%rcs([], CurrCountMap, ListOfRunningCountsRev) ->
%    you_need_to_write_this(rcs, "empty list", 
%			   [CurrCountMap, ListOfRunningCountsRev]);
  

rcs([], _CurrCountMap, ListOfRunningCountsRev) ->
      ListOfRunningCountsRev;
rcs([H | T], CurrCountMap, ListOfRunningCountsRev) ->
   Count = maps:get(H,CurrCountMap,0),
   NewCountMap = maps:put(H,Count+1,CurrCountMap),
   rcs(T,NewCountMap,[NewCountMap|ListOfRunningCountsRev]).


%-----------------------------------------------------------------
running_count_ser_examples() ->
    % Create some lists on which to test.
    % See file hw3_scan_examples.txt for sample output.
    Examples = [ [ alan, george, alan, ian, mark, alan, ian, paul ],
		 [ "AA", 42, poodle, 3.14159, "BBBB", "AA", 
		   "AA", 42, "AA", true ],
		 [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
		   0, 2, 4, 6, 8, 0, 3, 6, 9, 0 ],
		 (lists:duplicate(big_number() div 2, 0.0)
		  ++ lists:duplicate(big_number() div 2, 1.0)),
		 misc:rlist(big_number(), 10)
	       ],
    Try = fun(List) -> 
		  io:format("running_count_ser(~W) -> ~n", [List, 5 ]),
		  print_running_count(running_count_ser(List))
	  end,
    [ Try(List) ||  List <- Examples ].

%-----------------------------------------------------------------
% running_count_par(W, ListKeyIn, ListKeyOut) -> FinalCount
%
%   Computes the running count of appearences of elements in ListIn.
%   Element i of ListOut is a map containing one key for each unique
%   element in ListIn elements 0...i.  The value corresponding to that
%   key is the number of elements 0...i of ListIn that were that key.
%   
%   Computation is done in parallel across the workers in W.
%   ListKeyIn is the key under which each worker can find its local
%   component of the list.  ListKeyOut is the key under which each
%   worker should put its local component of ListOut.  FinalCount is
%   the map containing the total counts for the whole list (it will
%   also be the final map in ListOut).
%
%   Note that running_count_par() does not return the same thing as
%   running_count_ser().
running_count_par(W, ListKeyIn, ListKeyOut) ->
    Rcp_Leaf1 = fun(ProcState) ->
    	Data = wtree:get(ProcState, ListKeyIn),
    	rcp_leaf1(Data)
    end,
    Rcp_leaf2 = fun(ProcState,AccIn) ->
    	rcp_leaf2(ProcState,AccIn,ListKeyIn,ListKeyOut)
    end,
    Combine = fun(Left,Right)->
    	rcp_combine(Left,Right)
    end,
    wtree:scan(W,Rcp_Leaf1,Rcp_leaf2,Combine,maps:new()).
    
%-----------------------------------------------------------------
% rcp_leaf1(ListIn) -> Summary
%
%   Returns the summary for the portion of the input list stored on
%   this leaf.  Note that leaf1() cannot update the local worker's
%   state, so any intermediate information computed in this routine
%   will be discarded.
rcp_leaf1(ListIn) ->
    List = running_count_ser(ListIn),
    case List == [] of
     true-> maps:new();
     false->lists:last(List)
    end.

%-----------------------------------------------------------------
% rcp_leaf2(ProcState, AccIn, ListKeyIn, ListKeyOut) -> NewProcState
%
%   Compute the running count maps for all entries in this leaf node.
%   Note that the running counts need to be computed from scratch
%   since leaf1() did not store any information.  AccIn should be a
%   summary for all elements of the list to the left of this node.
%   ListKeyIn and ListKeyOut are the keys under which the local input
%   and output lists should be stored.
%
%   When writing the code for this function, you may find the
%   functions wtree:get(), wtree:put(), maps:new(), maps:get(),
%   maps:map(), maps:put(), maps:update() and/or maps:update_with()
%   helpful.
rcp_leaf2(ProcState, AccIn, ListKeyIn, ListKeyOut) ->
  SourceData = wtree:get(ProcState, ListKeyIn),
  case is_list(SourceData) of 
  	true-> MapLists = lists:reverse(rcs(SourceData,AccIn,[])),
  		   wtree:put(ProcState,ListKeyOut,MapLists);
  	false -> throw("input is not a list")	   
  end.

%-----------------------------------------------------------------
% rcp_combine(LeftOrParent, RightOrLeft) -> ParentOrRight
%
%   All arguments and return values are the same type as the summary
%   returned by rcp_leaf1().
%
%   During the upward sweep through the tree, the first argument will
%   be the left child's summary and the second argument will be
%   the right child's summary; the return value will be passed
%   to the parent.
%
%   During the downward sweep the first argument will be the parent's
%   summary (containing the summary for all nodes to the
%   left of this subtree) and the second argument will be the left
%   child's summary; the return value will be passed to the
%   right child (the left child just gets the parent's summary
%   without modification).
%
%   When writing the code for this function, you may find the
%   functions maps:new(), maps:get(), maps:map(), maps:put(),
%   maps:update() and/or maps:update_with() helpful.  You may also
%   find the function merge_map_with_fun() implemented below helpful.
rcp_combine(LeftOrParent, RightOrLeft) ->
    Function = fun(A,B)->A+B end,
    merge_map_with_fun(LeftOrParent, RightOrLeft,Function).

%-----------------------------------------------------------------
% rcp_root(RootSummary) -> { ReturnValue, Acc0 }
%
%   Receives the result of combining all workers' summaries.  Returns
%   a tuple, where ReturnValue is the value returned by the call to
%   wtree:scan() and Acc0 is the initial accumulator for the left
%   subtree.
%
%   When writing the code for this function, you may find the
%   functions maps:new(), maps:get(), maps:map(), maps:put(),
%   maps:update() and/or maps:update_with() helpful.
rcp_root(RootSummary) ->
    {RootSummary,maps:new()}.


%-----------------------------------------------------------------
running_count_par_examples(NumWorkers) ->
    % Create some lists on which to test.
    % See file hw3_scan_output.txt for sample output.
    Examples = [ [ alan, george, alan, ian, mark, alan, ian, paul ],
		 [ "AA", 42, poodle, 3.14159, "BBBB", "AA", 
		   "AA", 42, "AA", true ],
		 [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
		   0, 2, 4, 6, 8, 0, 3, 6, 9, 0 ],
		 (lists:duplicate(big_number() div 2, 0.0)
		  ++ lists:duplicate(big_number() div 2, 1.0)),
		 misc:rlist(big_number(), 10)
	       ],
    W = wtree:create(NumWorkers),
    Try = fun(List) ->
		  CutList = misc:cut(List, NumWorkers),
		  workers:update(W, listIn, 
				 fun(_ProcState, LocalList) -> 
					 LocalList end, 
				 CutList),
		  io:format("running_count_par(~W) -> ~n", [List, 5 ]),
		  FinalCount = running_count_par(W, listIn, listOut),
		  % Check the return value of the wtree:scan().
		  print_sorted_by_count(FinalCount),
		  % Retrieve and check what the wtree:scan() stored on
		  % the workers.
		  ListOfMaps = lists:flatten(workers:retrieve(W, listOut)),
		  print_running_count(ListOfMaps)
	  end,
    Result = [ Try(List) ||  List <- Examples ],
    % Make sure all of that previous work is done before killing the
    % workers.
    wtree:barrier(W),
    wtree:reap(W),
    Result.

%-----------------------------------------------------------------
% merge_map_with_fun(M1, M2, Fun) -> M3
%
% Creates a new map containing all the keys of both M1 and M2.  If a
% key appears only in M1 or M2, then the corresponding value is
% unchanged in M3.  If the key appears in both M1 and M2, then the
% corresponding value is given by Fun(V1, V2).
merge_map_with_fun(M1, M2, Fun) when is_map(M1), is_map(M2) ->
    mmwf(M1, Fun, maps:to_list(M2)).

mmwf(M3, _Fun, []) -> M3;
mmwf(OldM3, Fun, [ {K2, V2} | T]) ->
    NewM3 = maps:update_with(K2, % Key
			     fun(V1) -> Fun(V1, V2) end, % Fun
			     V2,  % Init
			     OldM3), %Map1
    mmwf(NewM3, Fun, T). %Map2

%-----------------------------------------------------------------
% Prints a single frequency map with the keys sorted by count.  May be
% useful for debugging.
print_sorted_by_count(Map) ->
    % Convert to a list.
    List = maps:to_list(Map),
    Len = length(List),
    % Sort the list according to frequencies (values).
    SortedList = lists:sort(fun({ _, V1 }, { _, V2 }) ->
				    V1 > V2 end, 
			    List),
    % Need to flatten the tuples for printing (without flattening any
    % lists which may appear as keys).
    FlatList = lists:flatmap(fun({K,V}) -> [K,V] end, SortedList),

    % How should we format a single count?
    EntryStringData = "~n    element: ~w, frequency: ~w",
    % Construct a single format string for all the entries.
    FormatStringData = "  Counts:"
	++ lists:flatten(lists:duplicate(Len,EntryStringData))
	++ "~n",
    % Generate the output.
    io:format(FormatStringData, FlatList).

%-----------------------------------------------------------------
% Prints a list of count maps, with the keys sorted by key.  May be
% useful for debugging.  Note that the output for very long lists will
% only show the first and last TruncateSize counts (but you can modify
% that if necessary).
print_running_count(ListOfMaps) ->
    % Half the size that is too big to display fully.  Beyond that
    % size we will show only the first and last entries of ListOfMaps.
    TruncateSize = 10,
    NumMaps = length(ListOfMaps),

    % Get the keys from the last map (which should be a superset of
    % all keys in previous maps).
    LastMap = lists:last(ListOfMaps),
    AllKeys = lists:sort(maps:keys(LastMap)),
    NumKeys = length(AllKeys),

    % Test that there are no keys in earlier maps which are not in the
    % final map.  For every earlier Map, check that all of its keys
    % are in the final map.  Slow but good for debugging.  Will
    % generate a {badkey, Key} error if a key appears in an earlier
    % map but not the final one.
    lists:foreach(fun(Map) ->
			  lists:foreach(fun(Key) ->
						maps:get(Key, LastMap)
					end, 
					maps:keys(Map))
		  end,
		  ListOfMaps),

    % Should we truncate?
    Truncate = (NumMaps > 2 * TruncateSize),
    % Now truncate ListOfMaps if it is too large.
    TruncatedListOfMaps = 
	if Truncate -> 
		(lists:sublist(ListOfMaps, TruncateSize)
		 ++ lists:nthtail(NumMaps-TruncateSize, ListOfMaps));
	   not Truncate -> ListOfMaps
	end,

    % Create a list containing one sublist for every key in the final
    % map.  Each sublist contains the key first and then the relative
    % frequency for each member of the sequence.  A flag value of the
    % atom 'n' is used to denote members of the sequence in which the
    % key does not appear.
    Everything = lists:map(
		   fun(Key) -> 
			   [ Key | 
			     lists:map(
			       fun(Map) ->
				       maps:get(Key, Map, n)
			       end, 
			       TruncatedListOfMaps)
			   ]
		   end, 
		   AllKeys),
    % Flatten the first level of those sublists.  Do not fully
    % flatten, since the keys may be lists themselves.
    EveryFlat = lists:flatmap(fun(Sublist) -> Sublist end, Everything),

    % Construct a single format string for all the entries.
    ElementStringData = 
	if Truncate ->
		lists:flatten("~n    element ~w:"
			      ++ lists:duplicate(TruncateSize," ~w,")
			      ++ " ...,"
			      ++ lists:duplicate(TruncateSize-1," ~w,")
			      ++ " ~w");
	   not Truncate ->
		lists:flatten("~n    element ~w:"
			      ++ lists:duplicate(NumMaps-1, " ~w,")
			      ++ " ~w")
	end,

    FormatStringData = "  Counts over ~w elements:"
	++ lists:flatten(lists:duplicate(NumKeys, ElementStringData))
	++ "~n",
    % Generate the output.
    io:format(FormatStringData, [NumMaps | EveryFlat]).


%-----------------------------------------------------------------
%-----------------------------------------------------------------
% OTHER CODE
%-----------------------------------------------------------------
%-----------------------------------------------------------------

% Define a big number to use for testing high dimensions or large
% lists.
big_number() -> 10000.

%-------------------------------------------------------------------
% you_need_to_write_this(...)
%
%   Two functions that raise errors if you've left part of the
%   homework incomplete.  You don't need to write these function --
%   I've done that.  You do need to replace each call to these
%   functions in the templates above with your solutions to the
%   problems.
you_need_to_write_this(Who, What) ->
  error(error, [{missing_implementation, Who}, {details, What}]).

you_need_to_write_this(Who, What, _Pretend_to_use_these_variables) ->
  you_need_to_write_this(Who, What).


count_for_run_time(P,M,N)->
  List = misc:rlist(N,M),
  SeqT = time_it:t(fun() -> running_count_ser(List) end),
  SeqTime = element(2, hd(SeqT)),
  W = wtree:create(P),
  wtree:rlist(W,N,M,src),
  wtree:barrier(W),
  ParT = time_it:t(fun() -> running_count_par(W, src, dst) end),
  ParTime =  element(2, hd(ParT)),
  wtree:barrier(W),
  wtree:reap(W),
  io:format("P = ~w  , N= ~w ,M = ~w ~n ", [P,N,M]),
  io:format("SeqTime = ~f~n", [ SeqTime]),
    io:format("ParTime = ~f~n", [ParTime]),
	io:format("SpeedUp = ~w~n",[SeqTime / ParTime]).


time()->
    code:add_path("/home/c/cs418/public_html/resources/erl"),
	count_for_run_time(32,100,100),
   count_for_run_time(32,100,1000),
	count_for_run_time(32,100,10000),
	count_for_run_time(32,100,100000),
	count_for_run_time(32,100,1000000),
	count_for_run_time(1,100,1000000),
	count_for_run_time(2,100,1000000),
	count_for_run_time(4,100,1000000),
	count_for_run_time(8,100,1000000),
	count_for_run_time(16,100,1000000),
	count_for_run_time(32,100,1000000),
	count_for_run_time(64,100,1000000),
	count_for_run_time(128,100,1000000),
	count_for_run_time(256,100,1000000),
	count_for_run_time(32,10,100000),
	count_for_run_time(32,100,100000),
	count_for_run_time(32,1000,100000),
	count_for_run_time(32,10000,100000).	






