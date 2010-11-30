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

-module(bucket_inspector).

-export([inspect/2]).

inspect(Bucket, Server) ->
    {ok, C} = riak:client_connect(Server),
    {ok, Keys} = C:list_keys(Bucket),
    inspect_objects(Bucket, Keys, C).

inspect_objects(_Bucket, [], _Client) ->
    ok;
inspect_objects(Bucket, [H|T], Client) ->
    Client:get(Bucket, H),
    inspect_objects(Bucket, T, Client).