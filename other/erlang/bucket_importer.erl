%% -------------------------------------------------------------------
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


import_data(ToServer, Bucket, Directory, ContentType) when is_list(Bucket) ->
    import_data(ToServer, list_to_binary(Bucket), Directory, ContentType);
import_data(ToServer, Bucket, Directory, ContentType) ->
    {ok, Client} = riak:client_connect(ToServer),
    {ok, StripExtensionRe} = re:compile("\\.[a-z0-9]+$", [caseless]),
    DirectoryLen = length(Directory),

    F = fun(Filename_, Acc0_) ->
	    case file:read_file(Filename_) of
		{ok, Data} ->
		    FilenameRel = lists:nthtail(DirectoryLen, Filename_),
		    KeyBase = unmunge_directory(FilenameRel),
		    Key = re:replace(KeyBase, StripExtensionRe, "", [{return,binary}]),
		    Object = riak_object:new(Bucket, Key, Data, ContentType),
		    Client:put(Object, 1),
		    io:format(".");

		{error, Reason} ->
		    io:format("Error reading ~p:~p~n", [Filename_, Reason])
	    end,
	    Acc0_
    end,
    [] = filelib:fold_files(Directory, ".*", true, F, []),
    ok.


unmunge_directory([$/ | Rest]) ->
    unmunge_directory(Rest);
unmunge_directory([C1,$/,C2,$/,C3,$/ | [C1,C2,C3 | _] = Rest]) ->
    Rest.

