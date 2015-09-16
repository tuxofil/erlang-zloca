%%%-------------------------------------------------------------------
%%% File        : zloca.hrl
%%% Author      : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Description : zloca definitions file
%%% Created     : 13 Dec 2014
%%%-------------------------------------------------------------------

-ifndef(_ZLOCA).
-define(_ZLOCA, true).

-record(
   item,
   {key :: zloca:key(),
    value :: any(),
    ttl :: zloca:ttl(),
    updated = zloca_lib:micros() :: zloca_lib:micros()
   }).

%% ----------------------------------------------------------------------
%% eunit

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-endif.
