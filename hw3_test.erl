
-module(hw3_test).

-include_lib("eunit/include/eunit.hrl").
-import(hw3, [biggest_distance_ser/1,biggest_distance_par/2,running_count_ser/1,running_count_par/3,big_number/0]).

-export([run_all_tests/0,biggest_distance_par_examples/2]).

biggest_distance_ser_test()->
    { V10, V11, V12 } = { [0.0], [2.0], [-5.5] },
    { V20, V21, V22, V23, V24 } = 
	{ [0.0, 0.0], [0.0, -1.0], [1.0, 1.0], [-1.0, -1.0], [5, 12] },
    { V30, V31, V32 } = 
	{ [0.0, 0.0, 0.0], [1.0, 1.0, 1.0], [-1, +1, -1] },
    { VN0, VN1, VN2 } = 
	{ lists:duplicate(big_number(), 0.0), 
	  lists:duplicate(big_number(), 1.0), 
	  lists:seq(1, big_number()) },  
   L1 = [V10, V11, V12],
   L2 = [V20, V21, V23, V22, V24, V20],
   L3 = [V31, V30, V30, V32, V30],
   L4 = [VN1, VN0, VN2],
   L5 = [],
   L6 = [[1],[2],[3],[4],[5],[6]],
 ?assertEqual( { 7.5, 2 }, biggest_distance_ser(L1)),
 ?assertEqual(  { 13.0, 5 }, biggest_distance_ser(L2)),
  ?assertEqual( {1.7320508075688772,1 }, biggest_distance_ser(L3)),
 ?assertEqual({ 577393.5702794065,2 }, biggest_distance_ser(L4)),
 ?assertEqual({ none,none}, biggest_distance_ser(L5)),
 ?assertEqual({ 1.0,1}, biggest_distance_ser(L6))
 .

biggest_distance_par_test()->
    { V10, V11, V12 } = { [0.0], [2.0], [-5.5] },
    { V20, V21, V22, V23, V24 } = 
	{ [0.0, 0.0], [0.0, -1.0], [1.0, 1.0], [-1.0, -1.0], [5, 12] },
    { V30, V31, V32 } = 
	{ [0.0, 0.0, 0.0], [1.0, 1.0, 1.0], [-1, +1, -1] },
    { VN0, VN1, VN2 } = 
	{ lists:duplicate(big_number(), 0.0), 
	  lists:duplicate(big_number(), 1.0), 
	  lists:seq(1, big_number()) },  

  L1 = [V10, V11, V12],
   L2 = [V20, V21, V23, V22, V24, V20],
   L3 = [V31, V30, V30, V32, V30],
   L4 = [VN1, VN0, VN2],
    L5 = [],
   L6 = [[1],[2],[3],[4],[5],[6]],
 ?assertEqual(biggest_distance_ser(L1), biggest_distance_par_examples(4,L1)),
 ?assertEqual(  biggest_distance_ser(L2), biggest_distance_par_examples(4,L2)),
  ?assertEqual( biggest_distance_ser(L3), biggest_distance_par_examples(4,L3)),
 ?assertEqual(biggest_distance_ser(L4), biggest_distance_par_examples(4,L4)),
 ?assertEqual(biggest_distance_ser(L4), biggest_distance_par_examples(4,L4)),
   ?assertEqual(biggest_distance_ser(L5), biggest_distance_par_examples(4,L5)),
   ?assertEqual(biggest_distance_ser(L6), biggest_distance_par_examples(4,L6)),
    ?assertEqual( biggest_distance_ser(L3), biggest_distance_par_examples(1,L3)),
     ?assertEqual( biggest_distance_ser(L3), biggest_distance_par_examples(2,L3)),
      ?assertEqual( biggest_distance_ser(L3), biggest_distance_par_examples(4,L3)),
       ?assertEqual( biggest_distance_ser(L3), biggest_distance_par_examples(8,L3)),
        ?assertEqual( biggest_distance_ser(L3), biggest_distance_par_examples(16,L3))
 .


biggest_distance_par_examples(NumWorkers,List) ->
 %   List= List1,
    W = wtree:create(NumWorkers),
    Try = fun(_List) ->
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
	     Result =   Try(List) ,
    wtree:barrier(W),
    wtree:reap(W),
    Result.


running_count_ser_test()->
      Output1 = [[{alan,1}],
 [{alan,1},{george,1}],
 [{alan,2},{george,1}],
 [{alan,2},{george,1},{ian,1}],
 [{alan,2},{george,1},{ian,1},{mark,1}],
 [{alan,3},{george,1},{ian,1},{mark,1}],
 [{alan,3},{george,1},{ian,2},{mark,1}],
 [{alan,3},{george,1},{ian,2},{mark,1},{paul,1}]],
 Output2 = [[{"AA",1}],
 [{42,1},{"AA",1}],
 [{42,1},{poodle,1},{"AA",1}],
 [{42,1},{3.14159,1},{poodle,1},{"AA",1}],
 [{42,1},{3.14159,1},{poodle,1},{"AA",1},{"BBBB",1}],
 [{42,1},{3.14159,1},{poodle,1},{"AA",2},{"BBBB",1}],
 [{42,1},{3.14159,1},{poodle,1},{"AA",3},{"BBBB",1}],
 [{42,2},{3.14159,1},{poodle,1},{"AA",3},{"BBBB",1}],
 [{42,2},{3.14159,1},{poodle,1},{"AA",4},{"BBBB",1}],
 [{42,2},{3.14159,1},{poodle,1},{true,1},{"AA",4},{"BBBB",1}]],
  
 ?assertEqual([],running_count_ser([])),
 ?assertEqual([[{a,1}]],maplist_to_list(running_count_ser([a]))),
 ?assertEqual(Output1,maplist_to_list(running_count_ser( [ alan, george, alan, ian, mark, alan, ian, paul ]))),
 ?assertEqual(Output2, maplist_to_list(running_count_ser([ "AA", 42, poodle, 3.14159, "BBBB", "AA", "AA", 42, "AA", true ]))),
 ?assertEqual([[{a,1}],[{a,2}],[{a,2},{b,1}]], maplist_to_list(running_count_ser([a,a,b])))

.


maplist_to_list(MapList)->
    Fun = fun(Map)->maps:to_list(Map) end,
    lists:map(Fun,MapList).


running_count_par_examples(NumWorkers, List) ->
   W = wtree:create(NumWorkers),
  CutList = misc:cut(List, NumWorkers),
		  workers:update(W, listIn, 
				 fun(_ProcState, LocalList) -> 
					 LocalList end, 
				 CutList),
    running_count_par(W, listIn, listOut),
    ListOfMaps = lists:flatten(workers:retrieve(W, listOut)),
     wtree:barrier(W),
    wtree:reap(W),
    ListOfMaps.



running_count_par_test() ->
    L1 = [ alan, george, alan, ian, mark, alan, ian, paul ],
    L2 = [ "AA", 42, poodle, 3.14159, "BBBB", "AA", "AA", 42, "AA", true ],
    L3 = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 2, 4, 6, 8, 0, 3, 6, 9, 0 ],
    L4 = (lists:duplicate(big_number() div 2, 0.0) ++ lists:duplicate(big_number() div 2, 1.0)),
    L5 =  misc:rlist(big_number(), 10),
    ?assertEqual(running_count_ser( L1), running_count_par_examples(2, L1)),
    ?assertEqual(running_count_ser( L2), running_count_par_examples(2, L2)),
    ?assertEqual(running_count_ser( L3), running_count_par_examples(2, L3)),
    ?assertEqual(running_count_ser( L4),    running_count_par_examples(2, L4)),
 
    ?assertEqual(running_count_ser(L5),    running_count_par_examples(2, L5)),
     ?assertEqual(running_count_ser( L1), running_count_par_examples(4, L1)),
     ?assertEqual(running_count_ser(L1),  running_count_par_examples(8,  L1)),
     ?assertEqual(running_count_ser( L1), running_count_par_examples(16,  L1)).



run_all_tests()->
code:add_path("/home/c/cs418/public_html/resources/erl"),
biggest_distance_ser_test()
,biggest_distance_par_test()
,running_count_ser_test(),
running_count_par_test()
.