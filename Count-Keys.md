# Map Phase to Count Keys

[Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/mapreduce/js/count_keys.js)

This function counts records in a given bucket with MapReduce. There are two phases to the map reduce. The below function provides the map phase and should be combined with `Riak.reduceSum` to provide the number of keys in the bucket.

```js
function mapCount() {
  return [1]
}
```
