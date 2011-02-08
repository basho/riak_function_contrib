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

-module(digraph_importer).

-export([import_digraph/4]).

%% @spec import_digraph(ToServer :: node(), ToBucket :: string(), Ref :: digraph(), ContentType :: string()) -> ok.

import_digraph(ToServer, ToBucket, Ref, ContentType) ->
  {ok, Client} = riak:client_connect(ToServer),
  Vertices = digraph:vertices(Ref),
  Edges = digraph:edges(Ref),
  
  VTupleList = [{Vertex, []} || Vertex <- Vertices],
  MappedVTupleList = map_edges(VTupleList, Edges, Ref, ToBucket),

  lists:foreach(fun(VTuple) -> load_data(Client, VTuple, Ref, ToBucket, ContentType) end, MappedVTupleList).


map_edges(VTuple, [], _Ref, _Bucket) -> VTuple;

map_edges(VTuple, [H|T], Ref, Bucket) ->
  {Edge, Src, Dest, _} = digraph:edge(Ref, H),
  {value, {_, Links}} = lists:keysearch(Src, 1, VTuple),
  NewLinks = Links++[{{Bucket, Dest}, Edge}],
  NewVtuple = lists:keyreplace(Src, 1, VTuple, {Src, NewLinks}),
  map_edges(NewVtuple, T, Ref, Bucket).


load_data(Client, {Vertex, LinkList}, Ref, ToBucket, ContentType) ->
  {_, Label} = digraph:vertex(Ref, Vertex),

  Key      = to_binary(Vertex),
  Value    = to_binary(Label),
  Metadata = dict:from_list([{<<"content-type">>, ContentType},
                             {<<"Links">>, LinkList}]),
  
  Object = riak_object:new(to_binary(ToBucket), Key, Value, Metadata),
  Client:put(Object, 1),
  io:format("Vertex: ~p~n", [Key]).


to_binary(Item) when is_atom(Item) ->
  list_to_binary(atom_to_list(Item));
to_binary(Item) when is_list(Item) ->
  list_to_binary(Item);
to_binary(Item) when is_binary(Item) ->
  Item.

%%% EOF
