# Importing an Erlang digraph

Contributed By: [Ryan Maclear](https://github.com/ryanmaclear)

[Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/other/erlang/digraph_importer.erl)

This Erlang function allows you to import an Erlang digraph into a bucket. Each vertex corresponds to an object, with the key being assigned the vertex, and the value being assigned the vertex label. Edges are mapped to links on the corresponding object. Edge labels are ignored. A content type is required for the label. If the label contains erlang terms, use a content type of application/x-erlang-binary. 

The digraphs imported can be recreated using [[Digraph Exporter]] found elsewhere on this site.

The function takes 5 arguments:

```
digraph_importer:import_digraph(address of server, port, bucket, digraph, content-type).
```

## Notes

(*Note*: The cluster used for the example below is the dev cluster used in the Fast Track, so the port number used is 8082, and not the default port of 8087).

The function uses the Erlang Protocol Buffer client, so the correct ip address and port must be specified.

There are currently some restrictions regarding the construction of the digraph. The vertices should be created with `digraph:add_vertex/2` or `digraph:add_vertex/3` and the edges need to be created with `digraph:add_edge/5`

## Examples

A simple example where the vertex labels are json objects:

```erlang
D = digraph:new([cyclic, protected]).

JoeLabel  = "{\\"name\\":\\"Joe\\",  
              \\"surname\\":\\"Blogs\\"}".
FredLabel = "{\\"name\\":\\"Fred\\", 
              \\"surname\\":\\"Blogs\\", 
              \\"born\\":\\"3 January 1910\\"}".

digraph:add_vertex(D, "joe",  JoeLabel).
digraph:add_vertex(D, "fred", FredLabel).
digraph:add_edge(D, "son",    "joe",  "fred", []).
digraph:add_edge(D, "father", "fred", "joe", []).

digraph_importer:import_digraph("127.0.0.1", 8082, <<"family">>, D, "application/json").
```

The same example where the vertex labels are erlang terms:

```erlang
D = digraph:new([cyclic, protected]).

JoeLabel  = [{name, "Joe"}, 
             {surname, "Blogs"}].
FredLabel = [{name, "Fred"}, 
             {surname, "Blogs"}, 
             {born, "3 January 1910"}].

digraph:add_vertex(D, "joe", JoeLabel).
digraph:add_vertex(D, "fred", FredLabel).
digraph:add_edge(D, "son", "joe", "fred", []).
digraph:add_edge(D, "father", "fred", "joe", []).

digraph_importer:import_digraph("127.0.0.1", 8082, <<"family">>, D, "application/x-erlang-binary").
```
