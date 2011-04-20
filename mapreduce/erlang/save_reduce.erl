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


-module(reduce_functions).

%%Function assumes JSON values
-export([save_reduce/2]).

%%Arg is a [bucket, key] combination
save_reduce([Data | _], [Bucket, Key]) ->
    {ok, C} = riak:local_client(),
    Json = iolist_to_binary(mochijson2:encode(Data)),
    Object = riak_object:new(Bucket, Key, Json, "application/json"),
    C:put(Object, 1),
    [];
save_reduce(_, _) ->
    [].
