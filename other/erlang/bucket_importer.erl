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

-module(bucket_importer).

-export([import_data/4]).



import_data(ToServer, Bucket, Directory, ContentType) ->
   {ok, Client} = riak:client_connect(ToServer),
   FL = filelib:fold_files(Directory, ".*", true, fun(F, L) -> [F|L] end, []),
   [ load_data(F, Client, list_to_binary(Bucket), list_to_binary(ContentType)) || F <- FL ].

load_data(FName, Client, Bucket, ContentType) ->
  case file:read_file(FName) of
         {ok, Data} ->
             Key = list_to_binary(filename:basename(FName, filename:extension(FName))),
             Object = riak_object:new(Bucket, Key, Data, ContentType),
             Client:put(Object, 1),
             io:format(".");
         {error, Reason} ->
             io:format("Error reading ~p:~p~n", [FName, Reason])
     end.