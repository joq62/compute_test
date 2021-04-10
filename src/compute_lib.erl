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
-include("src/compute.hrl").
%% --------------------------------------------------------------------

%% External exports
-export([install/1,
	 start_restarted_nodes/1
	 
	]).

-define(WAIT_FOR_TABLES,5000).

%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

start_restarted_nodes(ExtraNodes)->
    start_restarted_nodes(ExtraNodes,[]).  
start_restarted_nodes([],R)->
    R;
start_restarted_nodes([Node|T],Acc) ->
    R=case rpc:call(Node,compute,read_status,[]) of
	  started->
	      case gen_mnesia:add_node(Node) of
		  ok->
		      case rpc:call(Node,compute,set_status,[running]) of
			  ok->
			      ok;
			  Err ->
			      {error,[Err,Node,compute,set_status,[running],?MODULE,?FUNCTION_NAME,?LINE]}
		      end;
		  Err->
		      {error,[Err,Node,add_node,?MODULE,?FUNCTION_NAME,?LINE]}
	      end;
	  running->
	      ok;
	  Err ->
	      
	      {error,[Err,Node,compute,read_status,[],?MODULE,?FUNCTION_NAME,?LINE]}
      end,
    start_restarted_nodes(T,[R|Acc]).
			      


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

install(ExternalNodes)->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    Table=lock,
    % init local
    ok=db_lock:create_table(),
    {atomic,ok}=db_lock:create(?DBASE_LEADER), 
    add_extra_nodes(ExternalNodes,Table,[]).
    
    % Add extra nodes
add_extra_nodes([],_Table,Result)->
    Result;
add_extra_nodes([Node|T],Table,Acc)->
    R=case net_adm:ping(Node) of
	  pang->
	      {error,[pang,Node,?MODULE,?FUNCTION_NAME,?LINE]};
	  pong->    %add Nodes
    	      case gen_mnesia:add_node(Node) of
		  ok->
		      StorageType=ram_copies,    
		      case gen_mnesia:add_table(Node,Table,StorageType) of
			  ok->
			      case rpc:call(Node,compute,set_status,[running]) of
				  ok->
				      ok;
				  Err->
				      {error,[Err,Node,compute,set_status,[running],?MODULE,?FUNCTION_NAME,?LINE]}
			      end;
			  Err->
			      {error,[Err,add_table,Node,?MODULE,?FUNCTION_NAME,?LINE]}
		      end;
		  Err->
		      {error,[Err,add_node,Node,?MODULE,?FUNCTION_NAME,?LINE]}
	      end
      end,
    add_extra_nodes(T,Table,[R|Acc]).

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

