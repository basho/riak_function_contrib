/* -------------------------------------------------------------------
 This file is provided to you under the Apache License,
 Version 2.0 (the "License"); you may not use this file
 except in compliance with the License.  You may obtain
 a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing,
 software distributed under the License is distributed on an
 "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 KIND, either express or implied.  See the License for the
 specific language governing permissions and limitations
 under the License.

-------------------------------------------------------------------*/


// This function was originally written in a "CoffeeScript" (http://jashkenas.github.com/coffee-script/) which compiles down to JavaScript
// The CoffeeScript code looks like this:

/* sort = (values, arg) ->
 field = arg?.by
 reverse = arg?.order is 'desc'
 values.sort (a, b) ->
   if reverse then [a,b] = [b,a]
   if a?[field] < b?[field] then -1
   else if a?[field] is b?[field] then 0
   else if a?[field] > b?[field] then 1 */

// The code below is what is generated after running it through a compiler


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