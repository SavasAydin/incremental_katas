-module(chaos_driven_development_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MATRIX_2_X_2, {2,2}).

state_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun initial_state_if_no_command/0,
      fun drive_forward/0,
      fun drive_forward_and_drive_backward/0,
      fun drive_forward_and_turn_right/0,
      fun turn_right_and_turn_left/0,
      fun turn_left_twice/0,
      fun turn_left_three_times/0,
      fun turn_right_three_times/0,
      fun drive_forward_turn_right_and_drive_backward/0,
      fun turn_right_twice_and_drive_backward/0,
      fun turn_left_and_drive_forward/0,
      fun turn_left_drive_forward_and_drive_backward/0,
      fun turn_left_drive_forward_and_turn_right_drive_forward_three_times/0
     ]
    }.

initial_state_if_no_command() ->
    chaos_driven_development:visualize_game_state(?MATRIX_2_X_2),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n">>, Bin).       

drive_forward() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"DF">>),
    chaos_driven_development:visualize_game_state(?MATRIX_2_X_2),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n"
		   "[ ][N]\n[ ][ ]\n">>, Bin).  

drive_forward_and_drive_backward() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"DF\nDB">>),
    chaos_driven_development:visualize_game_state(?MATRIX_2_X_2),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n"
		   "[ ][N]\n[ ][ ]\n"
		   "[ ][ ]\n[ ][N]\n">>, Bin).  

drive_forward_and_turn_right() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"DF\nTR">>),
    chaos_driven_development:visualize_game_state(?MATRIX_2_X_2),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n"
		   "[ ][N]\n[ ][ ]\n"
		   "[ ][E]\n[ ][ ]\n">>, Bin).  

turn_right_and_turn_left() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"TR\n\TL">>),
    chaos_driven_development:visualize_game_state(?MATRIX_2_X_2),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n"
		   "[ ][ ]\n[ ][E]\n"
		   "[ ][ ]\n[ ][N]\n">>, Bin).  

drive_forward_turn_right_and_drive_backward() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"DF\nTR\nDB">>),
    chaos_driven_development:visualize_game_state(?MATRIX_2_X_2),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n"
		   "[ ][N]\n[ ][ ]\n"
		   "[ ][E]\n[ ][ ]\n"
		   "[E][ ]\n[ ][ ]\n">>, Bin).  

turn_left_twice() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"TL\n\TL">>),
    chaos_driven_development:visualize_game_state(?MATRIX_2_X_2),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n"
		   "[ ][ ]\n[ ][W]\n"
		   "[ ][ ]\n[ ][S]\n">>, Bin).  

turn_left_three_times() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"TL\n\TL\nTL">>),
    chaos_driven_development:visualize_game_state(?MATRIX_2_X_2),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n"
		   "[ ][ ]\n[ ][W]\n"
		   "[ ][ ]\n[ ][S]\n"
		   "[ ][ ]\n[ ][E]\n">>, Bin).  

turn_right_twice_and_drive_backward() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"TR\n\TR\nDB">>),
    chaos_driven_development:visualize_game_state(?MATRIX_2_X_2),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n"
		   "[ ][ ]\n[ ][E]\n"
		   "[ ][ ]\n[ ][S]\n"
		   "[ ][S]\n[ ][ ]\n">>, Bin).  

turn_right_three_times() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"TR\n\TR\nTR">>),
    chaos_driven_development:visualize_game_state(?MATRIX_2_X_2),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n"
		   "[ ][ ]\n[ ][E]\n"
		   "[ ][ ]\n[ ][S]\n"
		   "[ ][ ]\n[ ][W]\n">>, Bin).  
    
turn_left_and_drive_forward() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"TL\nDF">>),
    chaos_driven_development:visualize_game_state(?MATRIX_2_X_2),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n"
		   "[ ][ ]\n[ ][W]\n"
		   "[ ][ ]\n[W][ ]\n">>, Bin).  

turn_left_drive_forward_and_drive_backward() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"TL\nDF\nDB">>),
    chaos_driven_development:visualize_game_state(?MATRIX_2_X_2),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n"
		   "[ ][ ]\n[ ][W]\n"
		   "[ ][ ]\n[W][ ]\n"
		   "[ ][ ]\n[ ][W]\n">>, Bin).  

turn_left_drive_forward_and_turn_right_drive_forward_three_times() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"TL\nDF\nTR\nDF\nTR\nDF\nTR\nDF">>),
    chaos_driven_development:visualize_game_state(?MATRIX_2_X_2),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n"
		   "[ ][ ]\n[ ][W]\n"
		   "[ ][ ]\n[W][ ]\n"
		   "[ ][ ]\n[N][ ]\n"
		   "[N][ ]\n[ ][ ]\n"
		   "[E][ ]\n[ ][ ]\n"
		   "[ ][E]\n[ ][ ]\n"
		   "[ ][S]\n[ ][ ]\n"
		   "[ ][ ]\n[ ][S]\n">>, Bin).  


    
setup() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<>>),
    StatesFile = "/tmp/states.txt",
    ok = file:write_file(StatesFile, <<>>),
    [CommandsFile, StatesFile].

cleanup(Files) ->
    [ file:delete(File) || File <- Files ].
