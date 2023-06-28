%%%-------------------------------------------------------------------
%% @doc org top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dist_sup). 
 
-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link([]) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  %  io:format("Nodes ~p~n",[{Nodes,?MODULE,?FUNCTION_NAME,?LINE}]),
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id=>dist,
		    start=>{dist,start,[]}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
