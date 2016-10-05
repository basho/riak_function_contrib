# Validate JSON Pre-Commit Hook 

[Source File on GitHub](https://github.com/basho/riak_function_contrib/blob/master/hooks/pre-commit/js/validate_json.js)

This function can be used in a pre-commit hook to validate that the input is a valid JSON document.

```js
function validateJSON(object){

  // A delete is a type of put in Riak so check and see what this
  // operation is doing and pass over objects being deleted
  if (obj.values[0]['metadata']['X-Riak-Deleted']){
    return obj;
  }

  try {
    Riak.mapValuesJson(object);
    return object;
  } catch(e) {
    return {"fail":"Object is not JSON"};
  }
}
```
