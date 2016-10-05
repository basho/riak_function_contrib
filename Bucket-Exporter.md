# Exporting Data from a Bucket to a Directory 

[Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/other/erlang/bucket_exporter.erl)

This Erlang function allows you to export the contents of a bucket to a directory on the the filesystem, you can export the entire contents of a bucket or a percentage of the key space. The export will be placed in several directories and the filenames will be the key of record and the extension you specify. This function is made to work with the [[Bucket Importer]] available on this site as well.

The function takes either 4 or 5 arguments.

```erlang
-module(bucket_exporter).

-export([export_data/4,
         export_data/5]).

export_data(FromServer, Bucket, Extension, Directory) ->
  export_data(FromServer, Bucket, Extension, Directory, 1.0).

export_data(FromServer, Bucket, Extension, Directory, InputSize) ->
    {ok, CFrom} = riak:client_connect(FromServer),
    {ok, Keys0} = CFrom:list_keys(Bucket),
    Keys = truncate_keys(Keys0, InputSize),
    io:format("Got ~p keys~n", [length(Keys)]),
    export_data(CFrom, Bucket, Extension, Directory, Keys, 0),
    io:format("Data export complete~n").

export_data(_CFrom, _Bucket, _Extension, _Directory, [], _) ->
    io:format("~n"),
    ok;
export_data(CFrom, Bucket, Extension, Directory0, [H|T], Count) when is_binary(H) ->
    Owner = self(),
    proc_lib:spawn(fun() ->
                           case CFrom:get(Bucket, H) of
                               {ok, FromObj} ->
                                   Directory = munge_directory(Directory0, binary_to_list(H)),
                                   FileName = binary_to_list(H) ++ "." ++ Extension,
                                   Path = filename:join([Directory, FileName]),
                                   filelib:ensure_dir(Path),
                                   Obj = riak_object:get_value(FromObj),
                                   ok = file:write_file(Path, Obj),
                                   Owner ! done;
                               _Error ->
                                   Owner ! done end end),
    NewCount = if
                   Count == 250 ->
                       let_workers_catch_up(Count),
                       0;
                   true ->
                       Count + 1
               end,
    export_data(CFrom, Bucket, Extension, Directory0, T, NewCount).

let_workers_catch_up(0) ->
    ok;
let_workers_catch_up(Count) ->
    receive
        done ->
            ok
    end,
    let_workers_catch_up(Count - 1).

munge_directory(Directory0, [C1, C2, C3|_]) ->
    Directory0 ++ [$/,C1,$/,C2,$/,C3].

truncate_keys(Keys, 1.0) ->
    Keys;
truncate_keys(Keys, InputSize) ->
    TargetSize = erlang:round(length(Keys) * InputSize),
    {Keys1, _} = lists:split(TargetSize, Keys),
    Keys1.
```

## Tips and tricks for using this functions

This function can executed two ways, you can export the entire contents of a bucket:

```erlang
bucket_exporter:export_data('riak@127.0.0.1',<<"bucket">>, "json", "/tmp/bucket_export").
```

To export 10 percent of the keys in the bucket run the command with 5 arguments:

```erlang
bucket_exporter:export_data('riak@127.0.0.1',<<"bucket">>, "json", "/tmp/bucket_export", 0.1).
```
