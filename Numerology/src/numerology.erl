-module(numerology).

-export([replace/1,
	 replace_nine_by_ten/1,
	 replace_two_by_one/1,
	 replace_six_by_three/1,
	 replace_three_once_by_five/1,
	 replace_four_once_by_three/1,
	 replace_step_03/1
	]).

replace(Numbers) ->
   lists:foldl(fun(Fun,Acc) -> Fun(Acc) end,
		Numbers,
		[fun replace_nine_by_ten/1,
		 fun replace_two_by_one/1,
		 fun replace_six_by_three/1]).

replace_nine_by_ten([]) ->
    [];
replace_nine_by_ten([9|Numbers]) ->
    [10,10 | replace_nine_by_ten(Numbers)];
replace_nine_by_ten([Number|Numbers]) ->
    [Number | replace_nine_by_ten(Numbers)].

replace_two_by_one([]) ->
    [];
replace_two_by_one([Number,2|Numbers]) ->
    [Number] ++ lists:duplicate(Number,1) ++ replace_two_by_one(Numbers);
replace_two_by_one([Number|Numbers]) ->
    [Number | replace_two_by_one(Numbers)].

replace_six_by_three([]) ->
    [];
replace_six_by_three([Number,6|Numbers])->
    [Number] ++ lists:duplicate(lists:nth(Number,Numbers),3) ++
	replace_six_by_three(Numbers);
replace_six_by_three([Number|Numbers]) ->
    [Number | replace_six_by_three(Numbers)].

replace_step_03(Numbers) ->    
    Funs = [fun replace_three_once_by_five/1,
	    fun replace_four_once_by_three/1],
    apply_next_if_first_succeed(Funs,Numbers).

apply_next_if_first_succeed([],Numbers) ->
    Numbers;
apply_next_if_first_succeed([F|Funs],Numbers) ->
    case F(Numbers) of
	Numbers ->
	    Numbers;
	NewNumbers ->
	    apply_next_if_first_succeed(Funs,NewNumbers)
    end.

replace_three_once_by_five([]) ->
    [];
replace_three_once_by_five([3,Number|Numbers]) when Number /= 5 ->
    [5,Number|Numbers];
replace_three_once_by_five([Number|Numbers]) ->
    [Number | replace_three_once_by_five(Numbers)].

replace_four_once_by_three([]) ->
    [];
replace_four_once_by_three([Number,4|Numbers]) when Number /= 5 ->
    [Number,3|Numbers];
replace_four_once_by_three([Number|Numbers]) ->
    [Number | replace_four_once_by_three(Numbers)].
