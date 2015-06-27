-module(chaos_driven_development).

-export([visualize_game_state/1]).

-define(COMMANDS_FILE, "/tmp/commands.txt").
-define(STATES_FILE, "/tmp/states.txt").

visualize_game_state(Dimension) ->
    M = initialize_robot_in_matrix(Dimension),
    Commands = read_commands(),
    perform_commands_and_store_results(Commands,M).

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

perform_commands_and_store_results(Commands,State) ->
    lists:foldl(fun(Command,S) -> 
			NewState = perform_command(Command,S),
			store_state(NewState),
			NewState
		end,
		State,
		Commands).
    
perform_command([],State) ->
    State;
perform_command(Command,State) ->
    Pos = get_position(State),
    Fun = get_command_fun(Command),
    NewPos = Fun(Pos),
    NewState = clear_out_old_position(Pos,State),    
    update_position(NewPos,NewState).

get_position(State) ->
    [Pos] = lists:filter(fun({_,Direction}) -> Direction /= " " end, State),
    Pos.

get_command_fun(Command) ->
    Funs = [{"DF",fun move_forward/1},
	    {"DB",fun move_backward/1},
	    {"TR",fun turn_right/1},
	    {"TL",fun turn_left/1}],
    proplists:get_value(Command,Funs).

clear_out_old_position({Key,_},State) ->    
    update_position({Key," "},State).

move_forward({{X,Y},Direction}) ->
    Coordinate = proplists:get_value(Direction, [{"N",{X-1,Y}},
						 {"W",{X,Y-1}},
						 {"E",{X,Y+1}},
						 {"S",{X+1,Y}}]),
    {Coordinate,Direction}.

move_backward({{X,Y},Direction}) ->
    Coordinate = proplists:get_value(Direction, [{"N",{X+1,Y}},
						 {"S",{X-1,Y}},
						 {"E",{X,Y-1}},
						 {"W",{X,Y+1}}]),
    {Coordinate,Direction}.
    
turn_right({Coordinate,Direction}) ->
    NewDirection = proplists:get_value(Direction, [{"N","E"},
						   {"S","W"},
						   {"W","N"},
						   {"E","S"}]),
    {Coordinate,NewDirection}.    

turn_left({Coordinate,Direction}) ->
    NewDirection = proplists:get_value(Direction, [{"N","W"},
						   {"E","N"},
						   {"W","S"},
						   {"S","E"}]),
    {Coordinate,NewDirection}.    

update_position({Coordinate,Direction},State) ->
    lists:keyreplace(Coordinate, 1, State, {Coordinate, Direction}).

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
