
%% Author: Hal Eisen (hal.eisen@ask.com)

%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%  http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.


-module(uppercase_text).

-export([uppertext/1]).

uppertext(IncomingObject) ->
  BinaryContent = riak_object:get_value(IncomingObject),
  StringContent = binary:bin_to_list(BinaryContent),
  UpperStringContent = string:to_upper(StringContent),
  UpperBinaryContent = binary:list_to_bin(UpperStringContent),
  riak_object:apply_updates(riak_object:update_value(IncomingObject, UpperBinaryContent)).

