%%% @doc
%%% Commonly used functions.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 13 Dec 2014
%%% @copyright 2014, Aleksey Morarash <aleksey.morarash@gmail.com>

-module(zloca_lib).

%% API exports
-export([micros/0]).

-include("zloca.hrl").

%% --------------------------------------------------------------------
%% Data type definitions
%% --------------------------------------------------------------------

-export_type([micros/0]).

-type micros() :: non_neg_integer().

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Micros elapsed since Unix Epoch.
-spec micros() -> micros().
micros() ->
    {MegaSeconds, Seconds, MicroSeconds} = now(),
    (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds.
