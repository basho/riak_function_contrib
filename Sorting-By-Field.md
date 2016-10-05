# Reduce Phase to Sort by Field

_Contributed By:_ [Francisco Treacy](https://github.com/frank06)

[Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/mapreduce/js/sorting-by-field.js)
 
This a JavaScript MapReduce function that is actually written in [CoffeeScript](http://jashkenas.github.com/coffee-script). Its original use was to sort by field on objects stored in Riak. 

*Here is what the functions looks like in CoffeeScript:*

```coffeescript
sort = (values, arg) ->
 field = arg?.by
 reverse = arg?.order is 'desc'
 values.sort (a, b) ->
   if reverse then [a,b] = [b,a]
   if a?[field] < b?[field] then -1
   else if a?[field] is b?[field] then 0
   else if a?[field] > b?[field] then 1
```

*This compiles into JavaScript and looks like this:*

```js
var sort = function(values, arg) {
 var field = (typeof arg === "undefined" || arg === null) ? undefined : arg.by;
 var reverse = ((typeof arg === "undefined" || arg === null) ?
undefined : arg.order) === 'desc';
 values.sort(function(a, b) {
   if (reverse) {
     var _ref = [b, a];
     a = _ref[0];
     b = _ref[1];
   }
   if (((typeof a === "undefined" || a === null) ? undefined :
a[field]) < ((typeof b === "undefined" || b === null) ? undefined :
b[field])) {
     return -1;
   } else if (((typeof a === "undefined" || a === null) ? undefined :
a[field]) === ((typeof b === "undefined" || b === null) ? undefined :
b[field])) {
     return 0;
   } else if (((typeof a === "undefined" || a === null) ? undefined :
a[field]) > ((typeof b === "undefined" || b === null) ? undefined :
b[field])) {
     return 1;
   }
 });
};
```

# Tips and tricks for using this functions

Here is how it's actually used with [[riak-js|riakjs.org]], a Node.js client.

```js
db
 .add('airlines')
 .link({ bucket: 'flights', keep: false })
 .map('Riak.mapValuesJson')
 .reduce('Contrib.sort', { by: 'passengers', order: 'desc' })
 .run()
```

Default sorting is ascending (`asc`) so you can drop it. Specify `desc` for descending. The `by` property is required.
