%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created :  5 Mar 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(leader).

-behaviour(gen_server).
%% Server API
-export([who_is_leader/2,
	 am_i_leader/3,
	 ping/2]).



%% fms signals
-define(COORDINATOR_SERVER, ?MODULE).
-export([
	 start_election/1,
	 declare_victory/2,
	 i_am_alive/2
	]).


-define(ELECTION_RESPONSE_TIMEOUT, 3*50).

%% states
-define(ELECTION_STATE,election).
-define(CANDIDATE_STATE,candidate).
-define(LEADER_STATE,leader).

%% API
-export([start/1
	 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {leader,
		app,
		fsm_state,
		timestamp,
		nodes,
		timeout_pid}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_timestamp(LeaderPid,Timeout)->
    LeaderPid!{self(),get_timestamp},
    Result=receive
	       {LeaderPid,{TimeStamp,Node}}->
		   {TimeStamp,Node}
	   after Timeout ->
		   {error,[timeout]}
	   end,
    Result.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
who_is_leader(LeaderPid,Timeout)->
    LeaderPid!{self(),who_is_leader},
    Result=receive
	       {LeaderPid,Leader}->
		   Leader
	   after Timeout ->
		   {error,[timeout]}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ping(LeaderPid,Timeout)->
    LeaderPid!{self(),ping},
    Result=receive
	       {LeaderPid,pong}->
		   pong
	   after Timeout ->
		   {error,[timeout]}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
am_i_leader(LeaderPid,MyNode,Timeout)->
    Result=case leader:who_is_leader(LeaderPid,Timeout) of
	       {error,Reason}->
		   {error,Reason};
	       LeaderNode->
		   case LeaderNode==MyNode of
		       true->
			   true;
		       false->
			   false
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_election(LeaderPid)->
    LeaderPid!{start_election}.
declare_victory(LeaderPid,Node)->
    LeaderPid!{declare_victory,Node}.
i_am_alive(LeaderPid,Node)->
    LeaderPid!{i_am_alive,Node}.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start(App)->
    start_link(App).
start_link(App) ->
    gen_server:start_link(?MODULE, App, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init(App) ->
    ServerPid=self(),
    Pid=spawn(fun()->timeout_loop(ServerPid,infinity) end),
    send_start_election(App),
    set_election_timer(Pid,?ELECTION_RESPONSE_TIMEOUT),
    
    {ok, #state{
	    leader=undefined,
	    app=App,
	    fsm_state=election,
	    timeout_pid=Pid
	   }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Unmatched ",Request,From,node()]]),
    Reply = {error,[unmatched_signal,Request]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Unmatched ",Request,node()]]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info({nodedown,Node}, State) ->
 %   io:format(" ~p~n",[{nodedown,Node,node(),?MODULE,?LINE}]),
    case Node==State#state.leader of
	true->
	    send_start_election(State#state.app),
	    set_election_timer(State#state.timeout_pid,?ELECTION_RESPONSE_TIMEOUT),
	    NewState=State#state{fsm_state=election};
	false->
	    NewState=State
    end,
    {noreply, NewState};
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_info({CallerPid,get_timestamp}, State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["who_is_leader ",CallerPid,node()]]),
 %   io:format(" ~p~n",[{CallerPid,ping,node(),?MODULE,?LINE}]),
    CallerPid!{self(),State#state.leader},
    {noreply,State};

handle_info({CallerPid,who_is_leader}, State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["who_is_leader ",CallerPid,node()]]),
 %   io:format(" ~p~n",[{CallerPid,ping,node(),?MODULE,?LINE}]),
    CallerPid!{self(),State#state.leader},
    {noreply,State};

handle_info({CallerPid,ping}, State) ->
 %   io:format(" ~p~n",[{CallerPid,ping,node(),?MODULE,?LINE}]),
    CallerPid!{self(),pong},
    {noreply,State};


%% state election -----------------------------------------------------
handle_info({start_election}, #state{fsm_state=election}=State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["start_election  ",election,node()]]),
    send_start_election(State#state.app),
    set_election_timer(State#state.timeout_pid,?ELECTION_RESPONSE_TIMEOUT),
    NewState=State,
    {noreply, NewState};

handle_info({declare_victory,LeaderNode}, #state{fsm_state=election}=State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["declare_victory ",LeaderNode,election,node()]]),
    case LeaderNode==node() of
	true->
	    NewState=State#state{leader=node()};
	false->
	    monitor_node(State#state.leader, false),
	    monitor_node(LeaderNode, true),
	    NewState=State#state{leader=LeaderNode,
				 fsm_state=candidate}
    end,
    {noreply, NewState};

handle_info({i_am_alive,HigherNode}, #state{fsm_state=election}=State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["i_am_alive , HigherNode ",HigherNode,election,node()]]),
    set_election_timer(State#state.timeout_pid,infinity),
    NewState=State,
    {noreply, NewState};

handle_info({timeout_election}, #state{fsm_state=election}=State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["timeout_election ",election,node()]]),
    send_declare_victory(State#state.app),
    set_election_timer(State#state.timeout_pid,infinity),
    NewState=State#state{fsm_state=leader,
			 leader=node()},
    {noreply, NewState};

%% state candidate -----------------------------------------------------
handle_info({start_election}, #state{fsm_state=candidate}=State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["start_election  ",candidate,node()]]),
    send_i_am_alive(State#state.app),
    send_start_election(State#state.app),
    set_election_timer(State#state.timeout_pid,?ELECTION_RESPONSE_TIMEOUT),
    NewState=NewState=State#state{fsm_state=election},
    {noreply, NewState};

handle_info({i_am_alive,HigherNode}, #state{fsm_state=candidate}=State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["i_am_alive , HigherNode ",HigherNode,candidate,node()]]),
    NewState=State,
    {noreply, NewState};

handle_info({declare_victory,LeaderNode}, #state{fsm_state=candidate}=State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["declare_victory ",LeaderNode,candidate,node()]]),
    monitor_node(State#state.leader, false),
    monitor_node(LeaderNode, true),
    NewState=State#state{leader=LeaderNode},
    {noreply, NewState};

handle_info({timeout_election}, #state{fsm_state=candidate}=State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["timeout_election ",candidate,node()]]),
    set_election_timer(State#state.timeout_pid,infinity),
    NewState=State,
    {noreply, NewState};

%% state leader -----------------------------------------------------
handle_info({start_election}, #state{fsm_state=leader}=State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["start_election  ",leader,node()]]),
    send_i_am_alive(State#state.app),
    send_start_election(State#state.app),
    set_election_timer(State#state.timeout_pid,?ELECTION_RESPONSE_TIMEOUT),
    NewState=NewState=State#state{fsm_state=election},
    {noreply, NewState};

handle_info({i_am_alive,HigherNode}, #state{fsm_state=leader}=State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["i_am_alive , HigherNode ",HigherNode,leader,node()]]),
    NewState=State,
    {noreply, NewState};

handle_info({declare_victory,LeaderNode}, #state{fsm_state=leader}=State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["declare_victory ",LeaderNode,leader,node()]]),
    case LeaderNode==node() of
	true->
	    NewState=State#state{leader=node()};
	false->
	    monitor_node(State#state.leader, false),
	    monitor_node(LeaderNode, true),
	    NewState=State#state{leader=LeaderNode,
				 fsm_state=candidate}
    end,
    {noreply, NewState};

handle_info({timeout_election}, #state{fsm_state=leader}=State) ->
    %sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["timeout_election ",leader,node()]]),
    set_election_timer(State#state.timeout_pid,infinity),
    NewState=State,
    {noreply, NewState};


handle_info(Info, State) ->
    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Unmatched ",Info,node()]]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
set_election_timer(Pid,Timeout)->
    Pid!{self(),Timeout}.


timeout_loop(ServerPid,TimeOut)->
    receive
	{ServerPid,NewTimeOut}->
	    ok
    after
	TimeOut->
	    NewTimeOut=infinity,
	    ServerPid!{timeout_election}
    end,
    timeout_loop(ServerPid,NewTimeOut).
       
	
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
send_start_election(App)->
    Nodes=sd:get_node(App),
    HigherNodes=nodes_with_higher_ids(Nodes),
%    io:format("HigherNodes  ~p~n",[{HigherNodes,node(),?MODULE,?LINE}]),
    [rpc:cast(N,App,start_election,[])||N<-HigherNodes].
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
send_declare_victory(App)->
    Nodes=sd:get_node(App),
%    io:format("sd:get_node(App)  ~p~n",[{Nodes,App,node(),?MODULE,?LINE}]),
    LowerNodes=nodes_with_lower_ids(Nodes),
%    io:format("LowerNodes  ~p~n",[{LowerNodes,node(),?MODULE,?LINE}]),
    [rpc:cast(N,App,declare_victory,[node()])||N<-LowerNodes].
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
send_i_am_alive(App)->
    Nodes=sd:get_node(App),
    LowerNodes=nodes_with_lower_ids(Nodes),
%    io:format("LowerNodes  ~p~n",[{LowerNodes,node(),?MODULE,?LINE}]),
    [rpc:cast(N,App,i_am_alive,[])||N<-LowerNodes].
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
nodes_with_higher_ids(Nodes) ->
  [Node || Node <- Nodes, Node > node()].

nodes_with_lower_ids(Nodes) ->
  [Node || Node <- Nodes, Node < node()].

