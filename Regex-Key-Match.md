# Matching Records in a Map Phase with a Regex 

[Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/mapreduce/js/regex_key_match.js)

As you can see, this is a simple function written in JavaScript. Its original use was to return keys that match a given regex.

This is the entirety of the function:

```js
function keyMatch(value, arg){ 
  if (value.values[0].data.match(arg)) { 
    return [value.key] 
  } else {
    return []
  }
}
```
