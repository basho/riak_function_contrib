# Using the Contributed Functions

## Javascript

Javascript functions can be used ad hoc or by providing the path to the functions in your `app.config` file. If you choose to do this it is recommended that you place the functions in a namespace. For more information on executing queries please reference the [Riak Docs](http://docs.basho.com/riak/kv/latest/developing/usage/mapreduce).

# Erlang

All of the functions provided that are written in Erlang should be compiled and the resulting beams placed on each of the Riak nodes. You will need to edit your `app.config` file and add the `add_paths` directive to the `riak_kv` section so the node is aware of the additional modules. If you plan to use several of the Erlang functions provided they should be combined into one module and compiled for use by the system. You can use the Riak provided `erlc` to compile the modules. The `erlc` executable will be either in your base Riak directory if you compiled Riak from source or in the Riak `lib` directory if you used a binary package provided by Basho. 

Here is an example of compiling the [[bucket exporter|Bucket-Exporter]], loading the library dynamically into the Riak node, and using it in the Riak console.

```bash
$ /usr/lib/riak/erts-5.7.5/bin/erlc -o /tmp /tmp/bucket_reloader.erl
$ riak attach
(riak@127.0.0.1)1> code:add_path("/tmp").
(riak@127.0.0.1)2> m(bucket_exporter).
(riak@127.0.0.1)3> bucket_exporter:export_data('riak@127.0.0.1', <<"bucket">>, "json", "/tmp/bucket_export").
```
