%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc : represent a logical vm   
%%% 
%%% Supports the system with standard erlang vm functionality, load and start
%%% of an erlang application (downloaded from git hub) and "dns" support 
%%% 
%%% Make and start the board start SW.
%%%  boot_service initiates tcp_server and l0isten on port
%%%  Then it's standby and waits for controller to detect the board and start to load applications
%%% 
%%%     
%%% -------------------------------------------------------------------
-module(compute_unit_test). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("test_src/db_passwd.hrl").
-include("test_src/db_shop.hrl").
-include("src/db_lock.hrl").
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-define(WAIT_FOR_TABLES,10000).	  
%% --------------------------------------------------------------------
-export([node_name/1]).

%% ====================================================================
%% External functions
%% ====================================================================
node_name(Name)->
    {ok,Host}=inet:gethostname(),
    Node=list_to_atom(Name++"@"++Host),    
    Node.


%% --------------------------------------------------------------------
%% 
%% 
%% --------------------------------------------------------------------

% Single Mnesia 
clean_start_test_xx()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    rpc:call(NodeA,init,stop,[]),
    rpc:call(NodeB,init,stop,[]),
    timer:sleep(500),
    ok.
start_test()->	  
    ?assertEqual(ok,application:start(compute)),
    ok.

