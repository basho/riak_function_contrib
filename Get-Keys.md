# Map Phases to return Bucket and Key Pairs

[Erlang Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/mapreduce/erlang/get_keys.erl)

[Javascript Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/mapreduce/js/get_keys.js)

These functions return bucket and key pairs for use in subsequent map or reduce jobs. Either of these functions can be used in conjunction with the reduce function in [[Delete Keys]] to delete keys returned by these functions.

The Erlang module:

```erlang
-module(map_functions).

-export([get_keys/3]).

%Returns bucket and key pairs from a map phase
get_keys(Value,_Keydata,_Arg) ->
  [[riak_object:bucket(Value),riak_object:key(Value)]].
```

The Javascript function:

```js
function get_keys(object, keyData, arg) {
    return [[object.bucket, object.key]]
}
```
