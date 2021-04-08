%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(compute_lib).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("src/db_lock.hrl").
%% --------------------------------------------------------------------


%% External exports
-export([install/1
	]).

-define(WAIT_FOR_TABLES,5000).

%% ====================================================================
%% External functions
%% ====================================================================
install(ExternalNodes)->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    Table=lock,
    % init local
    ok=db_lock:create_table(),
    {atomic,ok}=db_lock:create(), 

    
    % Ensure connected
    R=[net_adm:ping(Node)||Node<-ExternalNodes],
    io:format("~p~n",[{R,?MODULE,?LINE}]),
    %add Nodes
    
    R_AddNode=[gen_mnesia:add_node(Node)||Node<-ExternalNodes],
    Result=case [Err||Err<-R_AddNode,Err/=ok] of
	       []->
		   StorageType=ram_copies,    
		   R_AddTable=[gen_mnesia:add_table(Node,Table,StorageType)||Node<-ExternalNodes],
		   case [Err||Err<-R_AddTable,Err/=ok] of
		       []->
			   ok;
		       _->
			   {error,[R_AddTable,?MODULE,?FUNCTION_NAME,?LINE]}
		   end;
	       _->
		   {error,[R_AddNode,?MODULE,?FUNCTION_NAME,?LINE]}
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------



%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

check_stopped_db_nodes()->
    case get_stopped_nodes() of
	[]->
	    ok;
	StoppedExtraDbNodes->
	    add_started_nodes(StoppedExtraDbNodes)
    end,
    get_stopped_nodes().


add_started_nodes([])->
    ok;
add_started_nodes([Vm|T])->
    initiate_added_node(Vm),
    timer:sleep(100),
    add_started_nodes(T).
	    
get_stopped_nodes()->
    ExtraDbNodes=mnesia:system_info(extra_db_nodes),
    RunningExtraDbNodes=lists:delete(node(),mnesia:system_info(running_db_nodes)),
    StoppedExtraDbNodes=[Node||Node<-ExtraDbNodes,
			     false==lists:member(Node,RunningExtraDbNodes)],
    StoppedExtraDbNodes.


create_table(Table,Args)->
    {atomic,ok}=mnesia:create_table(Table,Args),
    Tables=mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES).


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
initiate_added_node(Vm)->
    Result=case net_adm:ping(Vm) of
	       pong->
		   stopped=rpc:call(Vm,mnesia,stop,[]),
		   ok=mnesia:delete_schema([Vm]),
		   ok=rpc:call(Vm,mnesia,start,[]),
		   {ok,[Vm]}=mnesia:change_config(extra_db_nodes, [Vm]);
	       pang ->
		   {error,[not_running,Vm]}
	   end,    
    Result.


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

add_table(Vm,Table,StorageType)->
    mnesia:add_table_copy(Table,Vm,StorageType),
  %  [{Table,Args,_}]=db_gen_mnesia:read(Table),
    Tables=mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES).
    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

