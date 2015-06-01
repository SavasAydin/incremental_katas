-module(numerology_tests).

-include_lib("eunit/include/eunit.hrl").

replacer_test() ->
    Input = [1,2,3,4,5,6,7,8,9,10],
    Res = numerology:replace(Input),
    ?assertEqual([1,1,3,4,5,3,3,3,3,3,3,3,3,3,3,7,8,10,10,10],Res).

replace_9_by_two_tens_test() ->
    Input = [1,2,3,4,5,6,7,8,9,10],
    Res = numerology:replace_nine_by_ten(Input),
    ?assertEqual([1,2,3,4,5,6,7,8,10,10,10],Res).

replace_2_by_equal_amount_of_ones_as_number_to_left_of_2_test() ->
    Input = [3,2,3,4,5],
    Res = numerology:replace_two_by_one(Input),
    ?assertEqual([3,1,1,1,3,4,5], Res).

replace_6_by_equal_amount_of_threes_as_described_in_the_step_02_test() ->
    Input = [1,6,3,4,5],
    Res = numerology:replace_six_by_three(Input),
    ?assertEqual([1,3,3,3,3,4,5], Res).    
