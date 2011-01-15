%% -------------------------------------------------------------------
%% luwak_mr: utilities for map/reducing on Luwak data
%%
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
%%
%% -------------------------------------------------------------------

%% @doc Tools for map/reducing Luwak data.
%%
%%      The primary tool in this module is a function that conforms to
%%      the interface for "dynamic map/reduce inputs."  This function
%%      will allow you to set up a map/reduce process for running a
%%      computation across the blocks of a Luwak file.
%%
%%      To use the function via the Erlang client:
%%```
%%      C:mapred({modfun, luwak_mr, file, <<"my_file_name">>},
%%               [... your query ...]).
%%'''
%%      Over HTTP, structure your JSON query like:
%%```
%%      {"inputs":{"module":"luwak_mr",
%%                 "function":"file",
%%                 "arg":"my_file_name"},
%%       "query":[... your query ...]}
%%'''
%%
%%      The luwak_mr:file/3 function will send an input to the
%%      map/reduce query for each block in the file.  The "KeyData"
%%      for the block will be its offset in the file.  As a trivial
%%      example, you might use this to get an ordered list of the
%%      first byte of each block like so:
%%```
%%      F = fun(B, O, _) ->
%%             <<Y, _/binary>> = luwak_block:data(B),
%%             [{Y, O}]
%%          end,
%%      {ok, Bytes} = C:mapred({modfun,luwak_mr,file,<<"name">>},
%%                             [{map, {qfun, F}, none, true}]),
%%      OrderedBytes = lists:keysort(2, Bytes),
%%      [ Y || {Y, _} <- OrderedBytes.
%%'''

-module(luwak_mr).

-export([file/3]).

-include("luwak.hrl").

%% @spec file(pid(), binary(), integer()) -> ok
%% @doc Sends the bucket-keys for the blocks of a Luwak file as
%%      map/reduce inputs to the specified FlowPid.  Use it by
%%      specifying the map/reduce input as:
%%```
%%      {modfun, luwak_mr, file, <<"file_name">>}
%%'''
file(FlowPid, Filename, _Timeout) when is_binary(Filename) ->
    {ok, Client} = riak:local_client(),

    {ok, File} = luwak_file:get(Client, Filename),
    V = riak_object:get_value(File),
    {block_size, BlockSize} = lists:keyfind(block_size, 1, V),

    case lists:keyfind(root, 1, V) of
        {root, RootKey} -> tree(FlowPid, Client, BlockSize, RootKey, 0);
        false           -> ok
    end,

    luke_flow:finish_inputs(FlowPid).

%% @spec tree(pid(), riak_client(), integer(), binary(), integer())
%%          -> integer()
%% @doc Recursive tree walker used by file/3.  This function assumes
%%      that a child link in a tree is a data block if the size it
%%      lists is less than or equal to the specified BlockSize, and
%%      that it is a subtree if the size is greater than BlockSize.
%%
%%      The result is the offset of the byte that would imediately
%%      follow all of the bytes in this tree.  This fact is unused,
%%      but *could* be used for testing an invariant.
tree(FlowPid, Client, BlockSize, Key, Offset) ->
    {ok, #n{children=Children}} = luwak_tree:get(Client, Key),
    lists:foldl(
      fun({SubTree, Size}, SubOffset) when Size > BlockSize ->
              tree(FlowPid, Client, BlockSize, SubTree, SubOffset),
              SubOffset+Size;
         ({Leaf, Size}, LeafOffset) ->
              luke_flow:add_inputs(
                FlowPid, [{{?N_BUCKET, Leaf}, LeafOffset}]),
              LeafOffset+Size
      end,
      Offset,
      Children).
