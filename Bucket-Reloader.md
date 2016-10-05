# Copying a Bucket

[Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/other/erlang/bucket_reloader.erl)

This Erlang function can be used to copy all or some contents of a bucket from one Riak cluster to another. It can also be used to copy one bucket to another bucket within the same cluster.

The function takes four or five arguments depending on if you want to export the entire bucket or just a percentage of the keys.

```erlang
-module(bucket_reloader).

-export([reload/4,
        reload/5]).

reload(FromServer, ToServer, Bucket, NewBucket) ->
 reload(FromServer, ToServer, Bucket, NewBucket, 1.0).

reload(FromServer, ToServer, Bucket, NewBucket, InputSize) ->
   {ok, CFrom} = riak:client_connect(FromServer),
   {ok, CTo} = riak:client_connect(ToServer),
   {ok, Keys0} = CFrom:list_keys(Bucket),
   Keys = truncate_keys(Keys0, InputSize),
   io:format("Transferring ~p keys~n", [length(Keys)]),
   transfer(CFrom, CTo, Bucket, NewBucket, Keys, 0).

transfer(_CFrom, _CTo, _Bucket, _NewBucket, [], _) ->
   io:format("~n"),
   ok;
transfer(CFrom, CTo, Bucket, NewBucket, [H|T], Count) when is_binary(H) ->
   Owner = self(),
   proc_lib:spawn(fun() ->
                          case CFrom:get(Bucket, H) of
                              {ok, FromObj} ->
                                  OldObj = riak_object:get_value(FromObj),
                                  OldKey = riak_object:key(FromObj),
                                  OldContentType = riak_object:key(FromObj),
                                  Object = riak_object:new(NewBucket, OldKey, OldObj, OldContentType),
                                  CTo:put(Object, 1),
                                  io:format("."),
                                  Owner ! done;
                              Error ->
                                  error_logger:error_msg("Error fetching ~p/~p: ~p~n", [Bucket, H, Error]),
                                  Owner ! done
                          end end),
   NewCount = if
                  Count == 250 ->
                      let_workers_catch_up(Count),
                      0;
                  true ->
                      Count + 1
              end,
   transfer(CFrom, CTo, Bucket, NewBucket, T, NewCount).

let_workers_catch_up(0) ->
   ok;
let_workers_catch_up(Count) ->
   receive
       done ->
           ok
   end,
   let_workers_catch_up(Count - 1).

truncate_keys(Keys, 1.0) ->
   Keys;
truncate_keys(Keys, InputSize) ->
   TargetSize = erlang:round(length(Keys) * InputSize),
   {Keys1, _} = lists:split(TargetSize, Keys),
   Keys1.
```

## Tips and tricks for using this functions

Copy the contents of a bucket to a different bucket:

```erlang
bucket_reloader:reload('riak@127.0.0.1', 'riak@127.0.0.1', <<"bucket">>, <<"new_bucket">>).
```

To export the entire contents of a bucket from one Riak node to another:

```erlang
bucket_reloader:reload('riak@192.168.1.15', 'riak@127.0.0.1', <<"bucket">>, <<"bucket">>).
```

To export 10 percent of the keys in a bucket run the following:

```erlang
bucket_reloader:reload('riak@192.168.1.15', 'riak@127.0.0.1', <<"bucket">>, <<"bucket">>, 0.1).
```
