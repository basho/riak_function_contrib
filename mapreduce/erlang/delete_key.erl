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

-export([delete/2]).

%Data is a list of bucket and key pairs
delete(Data, _None) ->
    {ok, C} = riak:local_client(),
    F = fun(BucketKey, Acc) ->
                case BucketKey of
                    [Bucket, Key] ->
                        C:delete(Bucket, Key, 0),
                        Acc + 1;
                    _ ->
                        Acc
                end end,
    lists:foldl(F, 0, Data),
    [length(Data)].