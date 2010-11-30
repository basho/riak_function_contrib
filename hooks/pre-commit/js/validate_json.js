// -------------------------------------------------------------------
//
//
// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License.  You may obtain
// a copy of the License at
//
//  http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//
// -------------------------------------------------------------------

// Makes sure the object being inserted is valid JSON
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
