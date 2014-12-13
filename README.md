# Zloca Erlang Library

_Zloca_ is an acronym for Zero Latency On update CAche.

Briefly _Zloca_ is a kind of
[memoization](http://en.wikipedia.org/wiki/Memoization)
but with time to live set for each separate item.

The main difference is the memoization is for pure functions,
but the _Zloca_ is intended to use with functions with side effects.

See example below for more usage details.

## Example

Instead of thousands of words...

```erlang
...
%% Suppose there is some KV storage which takes much time to lookup,
%% say ETS table which sleeps 1 second on each lookup.
my_dumb_ets = ets:new(my_dumb_ets, [named_table]),
%% Now create a function which will emulate these long lookups:
DumbLookup =
    fun(Key) ->
        ok = timer:sleep(1000),
        [{_, Value}] = ets:lookup(my_dumb_ets, Key),
        Value
    end,
...
%% Now create a special Zloca process, which will cache the
%% values obtained by the DumbLookup function:
BackendFun =
    fun(Key) ->
        Value = DumbLookup(Key),
        %% Note the format of the returned value. The third
        %% element is a TTL (in seconds) and can be 'infinity'.
        %% When the third element (TTL) is not present,
        %% 'infinity' is meant.
        {ok, Value, _TTL = 2}
    end,
{ok, _} = zloca:start_link(ZlocaName = my_zloca, BackendFun),
...
%% Now we are ready to use Zloca. First of all, feed some data in
%% the our dumb ETS table:
true = ets:insert(my_dumb_ets, {k1, v1}),
%% Now ask the Zloca process for the value of the 'k1' key:
v1 = zloca:get(ZlocaName, k1),
%% The call takes near 1 second to return because the value was not
%% cached yet in the Zloca cache. But from the moment any consequent
%% call for the 'k1' will be returned almost immediately (because
%% there is only one ETS lookup under the hood).
...
%% Now we change the contents of the ETS table:
true = ets:insert(my_dumb_ets, {k1, v2}),
%% If we try to immediately ask the Zloca for the 'k1' again, it will
%% probably answer with old value 'v1', but when we ask the same
%% after TTL seconds (as returns our BackendFun), the Zloca will
%% have enough time to reread the value from the our dumb ETS and
%% will return from the zloca:get/2 function the brand new value 'v2'
%% without any delay.
ok = timer:sleep(2000),
%% Try it now! It is really fast!
v2 = zloca:get(ZlocaName, k1),
...
```

So, for example, if the BackendFun will return only values with
TTL = 'infinity', the _Zloca_ will behave as a simple memoization
library, because the DumbLookup function will be called only once
for each key.

## License

_Zloca_ uses the
[FreeBSD License](http://www.freebsd.org/copyright/freebsd-license.html).
You can obtain the license online or in a file LICENSE on the top of
the _Zloca_ source tree.

## Dependencies

Runtime dependencies: nothing just Erlang OTP.

Build dependencies:

* GNU Make;
* Erlang OTP;
* erlang-tools (namely Erlang `make` tool);
* erlang-edoc (only for HTML docs generation, see below);
* erlang-eunit (only for unit testing);
* erlang-dialyzer (only for tests with a Dialyzer).

## Build&Test

To build the Erlang application (into the ```ebin``` subdir):

```sh
$ make compile
```

To generate Erlang docs from the code (into the ```doc``` subdir):

```sh
$ make html
```

To run a unit tests:

```sh
make eunit
```

To run a dialyzer tests:

```sh
$ make dialyze
```

-----------------------------------------------------------------
Aleksey Morarash <aleksey.morarash@gmail.com>, 2014
