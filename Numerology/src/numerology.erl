-module(numerology).

-export([replace/1]).

replace(Numbers) ->
    lists:foldr(fun replace_nine_by_two_tens/2,[],Numbers).

replace_nine_by_two_tens(9,Acc) -> 
    [10,10 | Acc];
replace_nine_by_two_tens(Number,Acc) -> 
    [Number | Acc].
