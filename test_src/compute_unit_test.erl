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
-export([node_name/1,
	 monkey/0,
	 a_monkey/0,
	 a_sysinfo/0
	%,
	% b_sysinfo/0,
	% c_sysinfo/0,
	% a_kill/0,
	% b_kill/0,
	% c_kill/0,
	% a_boot/0,
	% b_boot/0,
	% c_boot/0

	]).

%% ====================================================================
%% External functions
%% ====================================================================
node_name(Name)->
    {ok,Host}=inet:gethostname(),
    Node=list_to_atom(Name++"@"++Host),    
    Node.

a_sysinfo()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    rpc:call(NodeA,mnesia,system_info,[]).

%%- Negative testing
a_monkey()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    rpc:call(NodeA,?MODULE,monkey,[]).
monkey()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    NodeC=list_to_atom("c@"++Host),
    WorkerNodes=[NodeA,NodeB,NodeC],
  %  io:format("~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("~p~n",[rpc:multicall(WorkerNodes,db_passwd,read,["Joq"])]),
    io:format("~p~n",[{time(),":",?MODULE,?FUNCTION_NAME,?LINE}]),
    timer:sleep(1000),
    slave:stop(NodeB),
    rpc:call(NodeB,application,stop,[mnesia]),
    rpc:call(NodeB,application,stop,[compute]),
    io:format("~p~n",[rpc:multicall(WorkerNodes,db_passwd,read,["Joq"])]),
    {ok,NodeB}=slave:start(Host,b,"-pa ebin -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot "),
    timer:sleep(12000),
    monkey().
%% --------------------------------------------------------------------
%% 
%% 
%% --------------------------------------------------------------------

% Single Mnesia 
clean_start_test()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    NodeC=list_to_atom("c@"++Host),
    rpc:call(NodeA,init,stop,[]),
    rpc:call(NodeB,init,stop,[]),
    rpc:call(NodeC,init,stop,[]),
    timer:sleep(500),
    ok.

%% Intial installation test 
start_compute_test()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    NodeC=list_to_atom("c@"++Host),
    WorkerNodes=[NodeA,NodeB,NodeC],
    ?assertEqual({ok,NodeA},slave:start(Host,a,"-pa ebin -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot ")),
    timer:sleep(1),
    ?assertEqual({ok,NodeB},slave:start(Host,b,"-pa ebin -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot ")),
    timer:sleep(1),
    ?assertEqual({ok,NodeC},slave:start(Host,c,"-pa ebin  -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot ")),
    timer:sleep(20),
    ?assertEqual([pong,pong,pong],[net_adm:ping(Node)||Node<-WorkerNodes]),
    ?assertMatch([{pong,_,_},{pong,_,_},{pong,_,_}],[rpc:call(Node,compute,ping,[])||Node<-WorkerNodes]),
    ok.
    
install_test()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    NodeC=list_to_atom("c@"++Host),
    ?assertMatch([ok,ok],rpc:call(NodeA,compute,install,[])),
    ok.


%% Cluster running 
create_table_passwd_test()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    NodeC=list_to_atom("c@"++Host),

    Table=passwd,
    Args=[{attributes, record_info(fields, passwd)}],
    ?assertEqual(ok,rpc:call(NodeA,gen_mnesia,create_table,[Table,Args])),
    ?assertEqual(ok,rpc:call(NodeA,gen_mnesia,add_table,[NodeB,passwd,ram_copies])),
    ?assertEqual(ok,rpc:call(NodeA,gen_mnesia,add_table,[NodeC,passwd,ram_copies])),
    ok.

passwd_test()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    NodeC=list_to_atom("c@"++Host),
    ?assertEqual({atomic,ok},rpc:call(NodeA,db_passwd,create,["David",david1])),
    ?assertEqual({atomic,ok},rpc:call(NodeB,db_passwd,create,["Joq",joq1])),
    ?assertEqual([{"Joq",joq1}],rpc:call(NodeC,db_passwd,read,["Joq"])),  
    ?assertEqual([{"Joq",joq1},{"David",david1}],rpc:call(NodeA,db_passwd,read_all,[])),    
    ok.

%monkey_test()->
 %   spawn(fun()->monkey() end),
  %  ok.    

		  


