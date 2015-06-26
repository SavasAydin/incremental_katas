-module(chaos_driven_development).

-export([visualize_game_state/1]).

-define(COMMANDS_FILE, "/tmp/commands.txt").
-define(STATES_FILE, "/tmp/states.txt").

visualize_game_state(Dimension) ->
    M = initialize_robot_in_matrix(Dimension),
    Commands = read_commands(),
    perform_commands(Commands,M).

initialize_robot_in_matrix(Dimension) ->
    M = create_matrix(Dimension),
    NewM = place_robot_in_matrix(M,Dimension),    
    ok = store_state(NewM),
    NewM.

create_matrix({N,M}) ->
    [ {{X,Y},Z} || X <- lists:seq(1,N), Y <- lists:seq(1,M), Z <- [" "] ].

place_robot_in_matrix(M,{X,Y}) ->
    lists:keyreplace({X,Y}, 1, M, {{X,Y}, "N"}).

read_commands() ->
    {ok, Bin} = file:read_file(?COMMANDS_FILE),
    string:tokens(binary_to_list(Bin),"\n").

perform_commands(Commands,State) ->
    lists:foldl(fun(Command,S) -> NewState = perform_command(Command,S),
				  store_state(NewState),
				  NewState
		end,
		State,
		Commands).
    
perform_command([],State) ->
    State;
perform_command("DF",State) ->
    Position = get_robot_position(State),
    move_forward(Position,State);
perform_command("DB",State) ->
    Position = get_robot_position(State),
    move_backward(Position,State);
perform_command("TR",State) ->
    Position = get_robot_position(State),
    turn_right(Position,State);   
perform_command("TL",State) ->
    Position = get_robot_position(State),
    turn_left(Position,State).

get_robot_position([{_," "}|Rest]) ->
    get_robot_position(Rest);
get_robot_position([H|_]) ->
    H.

move_forward({Key={X,Y},Direction},State) ->
    S = update_position(Key," ",State), 
    NewPos = proplists:get_value(Direction, [{"N",{X-1,Y}},
					     {"W",{X,Y-1}},
					     {"E",{X,Y+1}},
					     {"S",{X+1,Y}}
					    ]),
    update_position(NewPos,Direction,S).

move_backward({Key={X,Y},Direction},State) ->
    S = update_position(Key," ",State),
    NewPos = proplists:get_value(Direction, [{"N",{X+1,Y}},
					     {"S",{X-1,Y}},
					     {"E",{X,Y-1}},
					     {"W",{X,Y+1}}]),
    update_position(NewPos,Direction,S).
    
turn_right({Key,Direction},State) ->
    NewDir = proplists:get_value(Direction, [{"N","E"},
					     {"S","W"},
					     {"W","N"},
					     {"E","S"}]),
    update_position(Key,NewDir,State).

turn_left({Key,Direction},State) ->
    NewDirection = proplists:get_value(Direction, [{"N","W"},
						   {"E","N"},
						   {"W","S"},
						   {"S","E"}
						  ]),
    update_position(Key,NewDirection,State).

update_position(Key,Value,State) ->
    lists:keyreplace(Key, 1, State, {Key, Value}).

store_state(State) ->
    Result = visualize_state(State),
    file:write_file(?STATES_FILE, list_to_binary(Result), [append]).

visualize_state(State) ->
    Column = round(math:sqrt(length(State))),
    visualize_state(State,Column).

visualize_state([],_) ->
    [];
visualize_state([{{_,Column},_}=H|T],Column) ->
    generate_cell(H) ++ generate_newline() ++ visualize_state(T,Column);
visualize_state([H|T],Column) ->
    generate_cell(H) ++ visualize_state(T,Column).

generate_cell({_,X}) ->
    "[" ++ X ++ "]".
		
generate_newline() ->
    "\n".
