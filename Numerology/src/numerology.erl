-module(numerology).

-export([replace/1,
	 step_01/1,
	 step_02/1,
	 step_03/1
	]).

replace(Numbers) ->
   lists:foldl(fun(Fun,Acc) -> Fun(Acc) end,
		Numbers,
	       [fun step_01/1,
		fun step_02/1
		%%fun step_03/1
	       ]).

step_01(Numbers) ->
    replace_nine_by_ten(Numbers).

step_02(Numbers) ->
    replace_six_by_three(replace_two_by_one(Numbers)).

step_03(Numbers) ->
    replace_three_by_five_and_four_by_three(Numbers).

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


replace_three_by_five_and_four_by_three(Numbers) ->
    run_checks([fun replace_three_by_five/1,
		fun replace_four_by_three/1],
	       Numbers,
	       [],
	       0).

run_checks(_,Remaining,Result,7) ->
    Result++Remaining;
run_checks([F|Funs],Numbers,Result,Times) ->
    case F(Numbers) of
	Numbers ->
	    Result ++ Numbers;
	NewNumbers ->
	    {Replaced,Rest} = extract_rest(NewNumbers),
	    NewResult = Result ++ Replaced,
	    run_checks(lists:append(Funs,[F]),Rest,NewResult,Times+1)
    end.

extract_rest(Numbers) ->
    case lists:partition(fun(X) -> is_integer(X) end,
			 Numbers) of 
	{A,[]} ->
	    {A,[]};
	{A,[B]} ->
	    {A,B}
    end.

replace_three_by_five([]) ->
    [];
replace_three_by_five([3]) ->
    [5];
replace_three_by_five([3|Numbers]) when hd(Numbers) /= 5 ->
    [5,Numbers];
replace_three_by_five([Number|Numbers]) ->
    [Number|replace_three_by_five(Numbers)].

replace_four_by_three([]) ->
    [];
replace_four_by_three([Number,4|Numbers]) when Number /= 5 ->
    [Number,3,Numbers];
replace_four_by_three([Number|Numbers]) ->
    [Number|replace_four_by_three(Numbers)].

%% replace_three_by_five_and_four_by_three([]) ->
%%     [];
%% replace_three_by_five_and_four_by_three([3|Numbers]) when hd(Numbers) /= 5 ->
%%     [5 ];
%% replace_three_by_five_and_four_by_three([Number|Numbers]) ->
%%     [Number | replace_three_by_five_and_four_by_three(Numbers)].

%% replace_three_by_five_and_four_by_three([]) ->
%%     [];
%% replace_three_by_five_and_four_by_three([3]) ->
%%     [5];
%% replace_three_by_five_and_four_by_three([3|Numbers]) when hd(Numbers) /= 5 ->
%%     [5 | replace_first_four_by_three(Numbers)];
%% replace_three_by_five_and_four_by_three([Number|Numbers]) ->
%%     [Number | replace_three_by_five_and_four_by_three(Numbers)].

%% replace_first_four_by_three([]) ->
%%     [];
%% replace_first_four_by_three([4|Numbers]) ->
%%     [3|replace_three_by_five_and_four_by_three(Numbers)];
%% replace_first_four_by_three([Number,4|Numbers]) when Number /= 5 ->
%%     [Number,3 | replace_three_by_five_and_four_by_three(Numbers)];
%% replace_first_four_by_three([Number|Numbers]) ->
%%     [Number | replace_first_four_by_three(Numbers)].
