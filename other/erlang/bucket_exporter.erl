%% -------------------------------------------------------------------
%%
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%  http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

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
    export_data(CFrom, list_to_binary(Bucket), Extension, Directory, Keys, 0),
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