-module(numerology_tests).

-include_lib("eunit/include/eunit.hrl").

replace_nines_by_tens_test() ->
    Input = [1,2,3,4,5,6,7,8,9,10],
    Res = numerology:replace(Input),
    ?assertEqual([1,2,3,4,5,6,7,8,10,10,10],Res).
    
