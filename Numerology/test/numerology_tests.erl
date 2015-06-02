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

do_not_replace_4_by_a_three_if_3_not_replaced_by_five_test() ->
    Input = [6,5,10,4],
    Res = numerology:replace_step_03(Input),
    ?assertEqual([6,5,10,4],Res).

do_not_replace_3_by_five_in_row_if_4_not_replaced_by_three_in_between_test() ->
    Input = [6,3,10,3,15],
    Res = numerology:replace_step_03(Input),
    ?assertEqual([6,5,10,3,15],Res).

do_not_replace_4_by_three_in_row_if_3_not_replaced_by_five_in_between_test() ->
    Input = [6,3,10,4,6,4],
    Res = numerology:replace_step_03(Input),
    ?assertEqual([6,5,10,3,6,4],Res).

replace_4_by_a_three_if_3_is_replaced_by_five_test() ->
    Input = [6,3,10,4],
    Res = numerology:replace_step_03(Input),
    ?assertEqual([6,5,10,3],Res).
