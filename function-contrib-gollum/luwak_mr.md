# Map/Reducing Luwak File Data

[Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/mapred/erlang/luwak_mr.erl)

## Description and Usage

The primary tool in this module is a function that conforms to the
interface for "dynamic map/reduce inputs."  This function will allow
you to set up a map/reduce process for running a computation across
the blocks of a Luwak file.

To use the function via the Erlang client:

    C:mapred({modfun, luwak_mr, file, <<"my_file_name">>},
             [... your query ...]).

Over HTTP, structure your JSON query like:

    {"inputs":{"module":"luwak_mr",
               "function":"file",
               "arg":"my_file_name"},
     "query":[... your query ...]}

The luwak_mr:file/3 function will send an input to the map/reduce
query for each block in the file.  The "KeyData" for the block will be
its offset in the file.  As a trivial example, you might use this to
get an ordered list of the first byte of each block like so:

    F = fun(B, O, _) ->
           <<Y, _/binary>> = luwak_block:data(B),
           [{Y, O}]
        end,
    {ok, Bytes} = C:mapred({modfun,luwak_mr,file,<<"name">>},
                           [{map, {qfun, F}, none, true}]),
    OrderedBytes = lists:keysort(2, Bytes),
    [ Y || {Y, _} <- OrderedBytes.

## Installation

Before using the luwak_mr module, you'll need to build it, and add it
to the code path on your Riak nodes.

To build luwak_mr with Rebar, add the module to your project, and then
add then tell rebar to add luwak's include path to the Erlang compiler
options.  In rebar.config, add:

    {erl_opts, [{i, "/path/to/riak/lib/luwak-1.0.0/include"}]}.

To build luwak_mr with straight erlc commands, add luwak's include
path with the -I option:

    erl -I/path/to/riak/lib/luwak-1.0.0/include luwak_mr.erl

Once luwak_mr.beam is built, add the directory it's in to Riak's code
path.  Edit Riak's app.config, find the riak_kv section, and add and
add_paths parameter.  For example, if you compiled luwak_mr.beam to
/foo/luwak_mr/ebin/luwak_mr.beam, then your app.config should read:

    {riak_kv,
     ... other options ...
     {add_paths, ["/foo/luwak_mr/ebin"]}
    }

Or, if your Riak nodes are already running, connect to each node's
console, and execute the following:

    (riak@127.0.0.1)> code:add_path("/foo/luwak_mr/ebin").
