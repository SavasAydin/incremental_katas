-module(chaos_driven_development_tests).

-include_lib("eunit/include/eunit.hrl").

state_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun initial_state_if_no_command/0,
      fun drive_forward/0,
      fun drive_forward_and_turn_right/0
     ]
    }.

initial_state_if_no_command() ->
    chaos_driven_development:visualize(),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n">>, Bin).       

drive_forward() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"DF">>),
    chaos_driven_development:visualize(),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n[ ][N]\n[ ][ ]\n">>, Bin).  

drive_forward_and_turn_right() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<"DF\nTR">>),
    chaos_driven_development:visualize(),
    {ok, Bin} = file:read_file("/tmp/states.txt"),
    ?assertEqual(<<"[ ][ ]\n[ ][N]\n[ ][N]\n[ ][ ]\n[ ][E]\n[ ][ ]\n">>, Bin).  

setup() ->
    CommandsFile = "/tmp/commands.txt",
    ok = file:write_file(CommandsFile, <<>>),
    StatesFile = "/tmp/states.txt",
    ok = file:write_file(StatesFile, <<>>),
    [CommandsFile, StatesFile].

cleanup(Files) ->
    [ file:delete(File) || File <- Files ].

    
create_matrix_test() ->
    Res = chaos_driven_development:create_matrix(),
    ?assertEqual([{{1,1}," "},{{1,2}," "},{{2,1}," "},{{2,2}," "}], Res).

initialize_robot_in_matrix_test() ->
    M = chaos_driven_development:create_matrix(),
    Res = chaos_driven_development:init_place_of_robot(M),
    ?assertEqual([{{1,1}," "},{{1,2}," "},{{2,1}," "},{{2,2},"N"}], Res).
