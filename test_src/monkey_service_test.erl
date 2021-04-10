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
-module(monkey_service_test). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("test_src/db_passwd.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-define(WAIT_FOR_TABLES,10000).	  
-define(MaxRandNum,5).
%% --------------------------------------------------------------------
-export([start/0,
	 monkey/0,

	 test_second_blood/0

	]).

%% ====================================================================
%% External functions
%% ====================================================================
start()->
    monkey().

monkey()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    NodeC=list_to_atom("c@"++Host),
    WorkerNodes=[NodeA,NodeB,NodeC],
    io:format("~n"),
    io:format("~p **************Start New session *****************~n",[time()]),
 %   io:format("~n"),
    
    Ping=[net_adm:ping(Node)||Node<-WorkerNodes],
    case Ping of
	
	[pang,pang,pang]->
	    A=slave:start(Host,a,"-pa ebin -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot "),
	    B=slave:start(Host,b,"-pa ebin -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot "),
	    C=slave:start(Host,c,"-pa ebin -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot "),
	    io:format("Missing nodes nodes= ~p~n",[Ping]),
	    io:format("Start slaves = ~p~n",[{A,B,C}]),
	    rpc:call(NodeA,compute,install,[]),
	    timer:sleep(21000),
	    monkey();
	 [pong,pong,pong]->
	    io:format("All nodes= ~p~n",[Ping]),
	    ok;
	_ ->
	    A=slave:start(Host,a,"-pa ebin -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot "),
	    B=slave:start(Host,b,"-pa ebin -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot "),
	    C=slave:start(Host,c,"-pa ebin -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot "),
	    io:format("Missing nodes nodes= ~p~n",[Ping]),
	    io:format("Start slaves = ~p~n",[{A,B,C}]),
	    timer:sleep(21000),
	    monkey()
	    
    end,

 %   [pong,pong,pong]=Ping,
    

  %  io:format(" Check All workers that should be runnng *****************~n"),
  %  io:format("~n"),
    check_1(WorkerNodes,[no_node]),
  %  check_1(WorkerNodes,no_killed),
 
 %   io:format("AllNodes are running ~p~n",[{time(),":",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   io:format("Node A = ~p~n",[{time(),":",rpc:call(NodeA,db_passwd,read,["Joq"])}]),
 %   io:format("Node B = ~p~n",[{time(),":",rpc:call(NodeB,db_passwd,read,["Joq"])}]),
 %   io:format("Node C = ~p~n",[{time(),":",rpc:call(NodeC,db_passwd,read,["Joq"])}]),
    timer:sleep(1000),
   % io:format("~n"),

    %% Kill one or two nodes 1=non, 2=NodeA,3=NodeB,4=NodeC, 5=non
    FirstBlood=rand:uniform(?MaxRandNum),    
    {FirstKilledNode,FirstKilledNodeId}=kill_node(FirstBlood),
  %  io:format("First blood ~p~n",[{{FirstKilledNode,FirstKilledNodeId},?MODULE,?FUNCTION_NAME,?LINE}]),

    SecondBlood=second_blood(FirstBlood,rand:uniform(?MaxRandNum)),
    {SecondKilledNode,SecondKilledNodeId}=kill_node(SecondBlood),
  %  io:format("Second blood ~p~n",[{{SecondKilledNode,SecondKilledNodeId},?MODULE,?FUNCTION_NAME,?LINE}]),
   
    io:format(" Killed Workers = ~p~n", [{FirstKilledNode,SecondKilledNode}]),
  %  io:format("~n"),
    check_1(WorkerNodes,[FirstKilledNode,SecondKilledNode]),
    %%check_1(WorkerNodes,SecondKilledNode),
    
    
  %  io:format("NodeB not running ~p~n",[{time(),":",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  io:format("Node A = ~p~n",[{time(),":",rpc:call(NodeA,db_passwd,read,["Joq"])}]),
  %  io:format("Node B = ~p~n",[{time(),":",rpc:call(NodeB,db_passwd,read,["Joq"])}]),
  %  io:format("Node C = ~p~n",[{time(),":",rpc:call(NodeC,db_passwd,read,["Joq"])}]),

    %% Start killed nodes 
  %  io:format(" Start killed workers FirstKilledNodeId,SecondKilledNodeId = ~p~n", [{FirstKilledNodeId,SecondKilledNodeId}]),
  %  io:format("~n"),
    start_killed_node([FirstKilledNodeId,SecondKilledNodeId]),
    
   % {ok,NodeB}=slave:start(Host,b,"-pa ebin -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot "),
    
    
  %  io:format("End session wait ********************************************~n"),
  %  io:format("~n"),
    
    timer:sleep(15000),
    monkey().

check_1([],_KilledNode)->
    ok;
check_1([Node|T],KilledNodes)->
  %  io:format("check_1 Node,KilledNodes = ~p~n",[{Node,KilledNodes,?MODULE,?FUNCTION_NAME,?LINE}]),
    case lists:member(Node,KilledNodes) of
	true->	   
	    {badrpc,_Err}=rpc:call(Node,db_lock,read_all,[]);
	 %   {badrpc,_Err}=rpc:call(Node,db_passwd,read,["Joq"]);
%	    io:format("True Killed Node = ~p~n",[{Node,{badrpc,Err},?MODULE,?FUNCTION_NAME,?LINE}]);
	false->
	    case rpc:call(Node,db_lock,read_all,[]) of
		[dbase_leader]->
		    ok;
		Err->
		    io:format("Error in running node = ~p~n",[{Node,Err,?MODULE,?FUNCTION_NAME,?LINE}])
	    end
    end,
    check_1(T,KilledNodes).


start_killed_node([])->
    ok;
start_killed_node([NodeId|T])->
 %   io:format("start_killed_node ~p~n",[{NodeId,?MODULE,?FUNCTION_NAME,?LINE}]),
    start_node(NodeId),
    start_killed_node(T).

start_node(no_id)->
  %  io:format("start_node ~p~n",[{no_id,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok;
start_node(NodeId)->
  %  io:format("start_node ~p~n",[{NodeId,?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,Host}=inet:gethostname(),
    {ok,_Node}=slave:start(Host,NodeId,"-pa ebin -pa test_ebin -pa gen_mnesia/ebin -setcookie abc -compute config_file nodes -run compute boot "),
  %  io:format("Started Node = ~p~n",[{Node,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.
    
kill_node(Num)->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    NodeB=list_to_atom("b@"++Host),
    NodeC=list_to_atom("c@"++Host),
    {KilledNode,NodeId}=case Num of
			    2->
				slave:stop(NodeA),
				{NodeA,a};
			    3->
				slave:stop(NodeB),
				{NodeB,b};
			    4->
				slave:stop(NodeC),
				{NodeC,c};
			    _->
				{no_node,no_id}
			end,
    {KilledNode,NodeId}.
    
	    
	    

test_second_blood()->
    FirstBlood=rand:uniform(?MaxRandNum),    
    SecondBlood=second_blood(FirstBlood,rand:uniform(?MaxRandNum)),
    io:format("~p~n",[{FirstBlood, SecondBlood,?MODULE,?FUNCTION_NAME,?LINE}]),
    timer:sleep(1000),
    test_second_blood().

second_blood(FirstBlood,FirstBlood)->
 %   io:format("~p~n",[{FirstBlood,?MODULE,?FUNCTION_NAME,?LINE}]),
    second_blood(FirstBlood,rand:uniform(?MaxRandNum));
second_blood(_FirstBlood,SecondBlood)->
    SecondBlood.
   
%% --------------------------------------------------------------------
%% 
%% 
%% --------------------------------------------------------------------
