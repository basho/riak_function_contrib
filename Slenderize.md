# Slenderize Map Function

_Contributed By:_ [Francisco Treacy](https://github.com/frank06)

[Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/mapreduce/js/slenderize.js)
 
This function allows for making a Javascript object thinner by deleting some of its properties. Useful in some scenarios to improve performance when dealing with large objects, and prevent VMs of running out of memory.

```js
var slenderize = function (v, kd, arg) {
  
  // we obviously assume it's a JSON document
  v = Riak.mapValuesJson(v)[0];
  
  // arg must be an Array
  if (arg instanceof Array) {
    arg.forEach(function(prop) {
      delete v[prop];
    });
  } else {
    throw new Error("The provided argument must be an Array");
  }
  
  return [v];
}
```

## Example Usage

```js
db
 .add('flights')
 .map('Contrib.slenderize', ['passengers', 'tickets'])
 .run()
```
