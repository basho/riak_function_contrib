## Exporting to an Erlang digraph

Contributed By: [Ryan Maclear](https://github.com/ryanmaclear)

[Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/other/erlang/digraph_exporter.erl)

This Erlang module allows you to construct an Erlang digraph from objects in a bucket. Each object will result in a vertex being created, identified by the key, and optionally, the object value will be used for the vertex label. Links on objects will result in edges being created as specified by each link. An optional filter list can be specified which will result in only specific objects matching the filter being used to construct the digraph. If the filter list is omitted, the entire bucket will be used.

This function can be used to recreate digraphs stored in a bucket using [[Digraph Importer]] found elsewhere on this site.

## Notes

The function uses the Erlang Protocol Buffer client, so the correct ip address and port must be specified.

The digraph needs to be passed into the function, so it needs to exist and have the correct properties (eg. `[cyclic, protected]`).

Since digraphs are created in ETS, there is a finite limit on the number of vertices and edges that can be created, so be sure that you use filter lists where necessary.

Unfortunately, edges cannot be added to digraphs unless both vertices exist, so all the vertices are created first, and then only are the edges added. This means that the list of objects is traversed twice, and might have an impact on performance. Edges that cannot be created due to one or both vertices missing will be ignored.

The content-type of the object is used when creating the digraph label. If the content-type is application/x-erlang-binary, the label will be an erlang term, otherwise it will be a string (list).

Setting the last parameter of the function to false will result in all vetices having a label of `[]`.

## Examples

(*Note*: The cluster used for the example below is the dev cluster used in the Fast Track, so the port number used is 8082, and not the default port of 8087).

To create a digraph from an entire bucket, using the object values for the label:

```erlang
A = digraph:new([cyclic, protected]).                                     
 
digraph_exporter:export_digraph("127.0.0.1", 8082, <<"networks">>, A, true).
```

To create a digraph from an entire bucket, with vetex labels:

```erlang
A = digraph:new([cyclic, protected]).                                     
 
digraph_exporter:export_digraph("127.0.0.1", 8082, <<"networks">>, A, false).
```

To create a digraph from a bucket, using only objects with keys starting with `home`, using the object values for the label:

```erlang
 A= digraph:new([cyclic, protected]).                                     
 
 digraph_exporter:export_digraph("127.0.0.1", 8082, <<"networks">>, [<<"starts_with">>,<<"home">>], A, true).
```

## Source

```erlang
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

              ContentType = proplists:get_value(<<\\"content-type\\">>, MetaData, \\"\\"),
              Links       = proplists:get_value(<<\\"Links\\">>, MetaData, []),
              
              [{riak_object:key(Object), Value, ContentType, Links}]
            end.",

  {ok, Tokens, _} = erl_scan:string(MapFun),
  {ok, [Form]}    = erl_parse:parse_exprs(Tokens),
  Bindings        = erl_eval:new_bindings(),
  {value, Fun, _} = erl_eval:expr(Form, Bindings),
  Fun.
```
