-module(numerology_tests).

-include_lib("eunit/include/eunit.hrl").

perform_all_steps_test() ->
    Input = [1,2,3,4,5,6,7,8,9,10],
    Res = numerology:replace(Input),
    ?assertEqual([1,1,3,4,5,3,3,3,3,3,3,3,3,3,3,7,8,10,10,10],Res).

step_01_replace_9_by_two_tens_test() ->
    Input = [1,2,3,4,5,6,7,8,9,10],
    Res = numerology:step_01(Input),
    ?assertEqual([1,2,3,4,5,6,7,8,10,10,10],Res).

step_02_replace_2_by_ones_test() ->
    Input = [3,2,3,4,5],
    Res = numerology:step_02(Input),
    ?assertEqual([3,1,1,1,3,4,5], Res).

step_02_replace_6_by_threes_test() ->
    Input = [1,6,3,4,5],
    Res = numerology:step_02(Input),
    ?assertEqual([1,3,3,3,3,4,5], Res).    

step_03_do_not_replace_4_by_a_three_test() ->
    Input = [6,5,10,4],
    Res = numerology:step_03(Input),
    ?assertEqual([6,5,10,4],Res).

step_03_do_not_replace_second_3_by_five_test() ->
    Input = [6,3,10,3,15],
    Res = numerology:step_03(Input),
    ?assertEqual([6,5,10,3,15],Res).

step_03_do_not_replace_second_4_by_three_test() ->
    Input = [6,3,10,4,6,4],
    Res = numerology:step_03(Input),
    ?assertEqual([6,5,10,3,6,4],Res).

step_03_replace_both_3_by_five_and_4_by_a_three_test() ->
    Input = [6,3,10,4],
    Res = numerology:step_03(Input),
    ?assertEqual([6,5,10,3],Res).

step_03_replace_second_3_by_five_test() ->
    Input = [6,3,10,4,3],
    Res = numerology:step_03(Input),
    ?assertEqual([6,5,10,3,5],Res).    

step_03_replace_second_4_by_three_test() ->
    Input = [6,3,10,4,3,7,4],
    Res = numerology:step_03(Input),
    ?assertEqual([6,5,10,3,5,7,3],Res).    

step_03_replace_third_3_by_five_test() ->
    Input = [6,3,10,4,3,7,4,9,3,10],
    Res = numerology:step_03(Input),
    ?assertEqual([6,5,10,3,5,7,3,9,5,10],Res).    

step_03_replace_third_4_by_three_test() ->
    Input = [3,10,4,3,7,4,3,7,4,11],
    Res = numerology:step_03(Input),
    ?assertEqual([5,10,3,5,7,3,5,7,3,11],Res).

step_03_replace_forth_3_by_five_test() ->
    Input = [6,3,10,4,3,7,4,9,3,10,4,12,3,13],
    Res = numerology:step_03(Input),
    ?assertEqual([6,5,10,3,5,7,3,9,5,10,3,12,5,13],Res).    

step_03_do_not_replace_forth_4_by_three_test() ->
    Input = [6,3,10,4,3,7,4,9,3,10,4,12,3,13,4,111],
    Res = numerology:step_03(Input),
    ?assertEqual([6,5,10,3,5,7,3,9,5,10,3,12,5,13,4,111],Res).
