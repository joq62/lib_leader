%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 12 Mar 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(dist).

-behaviour(gen_server).

%% API

%%--------------------------------------------------------------------
-export([
	 start_election/0,declare_victory/1,
	 i_am_alive/1
	 
	]).

-export([
	 your_leader_pid/1,my_leader_pid/1,
	 who_is_leader/0,am_i_leader/1,ping_leader/0
	]).

%%
-export([start/0,
	 ping/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {leader_pid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start()->
    start_link().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
your_leader_pid(CallingPid)->
    gen_server:call(?SERVER,{your_leader_pid,CallingPid},infinity).
my_leader_pid(CallingPid)->
    gen_server:call(?SERVER,{my_leader_pid,CallingPid},infinity).
who_is_leader()->
    gen_server:call(?SERVER,{who_is_leader},infinity).
am_i_leader(Node)->
    gen_server:call(?SERVER,{am_i_leader,Node},infinity).

ping()->
    gen_server:call(?SERVER,{ping},infinity).
ping_leader()->
    gen_server:call(?SERVER,{ping_leader},infinity).    

%% election callbacks
start_election()->
    gen_server:cast(?SERVER,{start_election}).

declare_victory(LeaderNode)->
    gen_server:cast(?SERVER,{declare_victory,LeaderNode}).
i_am_alive(MyNode)->
    gen_server:cast(?SERVER,{i_am_alive,MyNode}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok,LeaderPid}=leader:start(dist),
 %   io:format("LeaderPid ~p~n",[{LeaderPid,?MODULE,?LINE}]),
       
    {ok, #state{leader_pid=LeaderPid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({am_i_leader,Node}, _From, State) ->
    Reply = leader:am_i_leader(State#state.leader_pid,Node,5000),
    {reply, Reply, State};

handle_call({who_is_leader}, _From, State) ->
    Reply = leader:who_is_leader(State#state.leader_pid,5000),
    {reply, Reply, State};

handle_call({ping}, _From, State) ->
 %   io:format("ping ~p~n",[{?MODULE,?LINE}]),
    Reply = pong,
    {reply, Reply, State};

handle_call({ping_leader}, _From, State) ->
%    io:format("ping_leader ~p~n",[{?MODULE,?LINE}]),
    Reply = leader:ping(State#state.leader_pid,5000),
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    io:format("Unmatched signal ~p~n",[{Request,?MODULE,?LINE}]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({i_am_alive,MyNode}, State) ->
    leader:i_am_alive(State#state.leader_pid,MyNode),
    {noreply, State};

handle_cast({declare_victory,LeaderNode}, State) ->
    leader:declare_victory(State#state.leader_pid,LeaderNode),
    {noreply, State};

handle_cast({start_election}, State) ->
    leader:start_election(State#state.leader_pid),
    {noreply, State};

handle_cast(Request, State) ->
    io:format("Unmatched signal ~p~n",[{Request,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("Unmatched signal ~p~n",[{Info,?MODULE,?LINE}]),
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
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
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
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
