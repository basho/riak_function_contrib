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

-module(digraph_exporter).

-export([export_digraph/5, export_digraph/6]).

%% @spec export_digraph(Server :: ip_address(),
%%                      Port :: integer(),
%%                      Bucket :: bucket(),
%%                      Ref :: digraph(),
%%                      UseValue :: boolean()) -> ok.

export_digraph(Server, Port, Bucket, Ref, UseValue) ->
  export_digraph(Server, Port, Bucket, undefined, Ref, UseValue).

%% @spec export_digraph(Server :: ip_address(),
%%                      Port :: integer(),
%%                      Bucket :: bucket(),
%%                      FilterList :: list(),
%%                      Ref :: digraph(),
%%                      UseValue :: boolean()) -> ok.

export_digraph(Server, Port, Bucket, FilterList, Ref, UseValue) ->
  {ok, Client} = riakc_pb_socket:start(Server, Port),
  
  Input = 
          case FilterList of
            undefined ->
              Bucket;
            _ ->
              {Bucket, [FilterList]}
          end,

  MapFun = build_map_fun(),
  
  MapPhase = {map, {qfun, MapFun}, notused, true},
  Query    = [MapPhase],
  
  {ok, [{_, Data}]} = riakc_pb_socket:mapred(Client, Input, Query),
  
  build_vertices(Ref, UseValue, Data),
  add_edges(Ref, Data).


build_vertices(_Ref, _UseValue, []) -> ok;
build_vertices(Ref, UseValue, [{Key, Value, ContentType, _}|T]) ->
  Label = 
          case UseValue of 
            true ->
              case ContentType of
                "application/x-erlang-binary" ->
                  binary_to_term(Value);
                _ ->
                  binary_to_list(Value)
              end;
            false -> ""
          end,

  Vertex = binary_to_list(Key),
  
  digraph:add_vertex(Ref, Vertex, Label),
  io:format("vertex: ~s~n", [Vertex]), 
  build_vertices(Ref, UseValue, T).


add_edges(_Ref, []) -> ok;
add_edges(Ref, [{Key, _, _, Links}|T]) ->
  lists:foreach(fun({{_, Dest}, Tag}) ->
                  Edge  = binary_to_list(Tag),
                  VSrc  = binary_to_list(Key),
                  VDest = binary_to_list(Dest),
                  digraph:add_edge(Ref, Edge, VSrc, VDest, ""),
                  io:format("edge: ~s~n", [Edge]) 
                end, Links),
  add_edges(Ref, T).


build_map_fun() ->
  MapFun = "fun(Object, _KeyData, _Args) ->
              [{MetaDataDict, Value}] = riak_object:get_contents(Object),
              
              MetaData    = dict:to_list(MetaDataDict),

              ContentType = proplists:get_value(<<\"content-type\">>, MetaData, \"\"),
              Links       = proplists:get_value(<<\"Links\">>, MetaData, []),
              
              [{riak_object:key(Object), Value, ContentType, Links}]
            end.",
  
  {ok, Tokens, _} = erl_scan:string(MapFun),
  {ok, [Form]}    = erl_parse:parse_exprs(Tokens),
  Bindings        = erl_eval:new_bindings(),
  {value, Fun, _} = erl_eval:expr(Form, Bindings),
  Fun.

%%% EOF
