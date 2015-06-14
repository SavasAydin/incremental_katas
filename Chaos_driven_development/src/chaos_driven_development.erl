-module(chaos_driven_development).

-export([visualize/0,
	 create_matrix/0,
	 init_place_of_robot/1]).

-define(COMMANDS_FILE, "/tmp/commands.txt").
-define(STATES_FILE, "/tmp/states.txt").

visualize() ->
    M = create_matrix(),
    NewM = init_place_of_robot(M),    
    Res = visualize_state(NewM),
    ok = write_states(list_to_binary(Res)),
    Commands = read_commands(),
    perform_commands(Commands,NewM).

create_matrix() ->
    [ {{X,Y},Z} || X <- [1,2], Y <- [1,2], Z <- [" "] ].

init_place_of_robot(M) ->
    lists:keyreplace({2,2}, 1, M, {{2,2}, "N"}).

read_commands() ->
    {ok, Bin} = file:read_file(?COMMANDS_FILE),
    string:tokens(binary_to_list(Bin),"\n").

perform_commands([],State) ->
    State;
perform_commands(Commands,State) ->
    lists:foldl(fun(Command,S) -> NewState = perform_command(Command,S),
				  Res = visualize_state(NewState),
				  write_states(list_to_binary(Res)),
				  NewState
		end,
		State,
		Commands).
			       
visualize_state([{_,Dir1},{_,Dir2},{_,Dir3},{_,Dir4}]) ->   
    generate_cell(Dir1) ++ generate_cell(Dir2) ++ generate_newline() ++
	generate_cell(Dir3) ++ generate_cell(Dir4) ++ generate_newline().

generate_cell(Direction) ->
    "[" ++ Direction ++ "]".

generate_newline() ->
    "\n".
    
perform_command([],State) ->
    State;
perform_command("DF",State) ->
    {{X,Y}, Direction} = get_robot_position(State),
    S2 = lists:keyreplace({X,Y}, 1, State, {{X,Y}, " "}),
    lists:keyreplace({X-1,Y}, 1, S2, {{X-1,Y}, Direction});
perform_command("TR",State) ->
    {{X,Y}, Direction} = get_robot_position(State),
    lists:keyreplace({X,Y}, 1, State, {{X,Y}, "E"}).            

    
get_robot_position([{_," "}|Rest]) ->
    get_robot_position(Rest);
get_robot_position([H|_]) ->
    H.

write_states(Actions) ->    
    file:write_file(?STATES_FILE, Actions, [append]).
