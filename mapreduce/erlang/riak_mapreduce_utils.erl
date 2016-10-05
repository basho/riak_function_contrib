%% -------------------------------------------------------------------
%%
%% riak_mapreduce_utils: Utility functions for defining map/reduce processing.
%%
%% Copyright (c)2012, Christian Dahlqvist, WhiteNode Software Ltd.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(riak_mapreduce_utils).

-export([map_delete/3,
         map_indexinclude/3,
         map_indexlink/3,
         map_metafilter/3,
         map_id/3,
         map_key/3,
         map_datasize/3
        ]).

%% From riak_pb_kv_codec.hrl
-define(MD_USERMETA, <<"X-Riak-Meta">>).
-define(MD_INDEX,    <<"index">>).

%%
%% Map Phases
%%

%% @spec map_delete(riak_object:riak_object(), term(), term()) ->
%%                   [integer()]
%% @doc map phase function for deleting records
map_delete({error, notfound}, _, _) ->
    [];
map_delete(RiakObject, Props, Arg) when is_list(Arg) ->
    map_delete(RiakObject, Props, list_to_binary(Arg));
map_delete(RiakObject, Props, Arg) when is_atom(Arg) ->
    map_delete(RiakObject, Props, <<"">>);
map_delete(RiakObject, _, Arg) when is_binary(Arg) ->
    {ok, C} = riak:local_client(),
    Bucket = riak_object:bucket(RiakObject),
    Key = riak_object:key(RiakObject),
    case Arg of
        Bucket ->
            C:delete(Bucket, Key),
            [1];
        <<"">> ->
            C:delete(Bucket, Key),
            [1];
        _ ->
            []
    end;
map_delete(_, _, _) ->
    [].

%% @spec map_indexinclude(riak_object:riak_object(), term(), term()) ->
%%                   [{{Bucket :: binary(), Key :: binary()}, Props :: term()}]
%% @doc map phase function for including based on secondary index query in a
%% manner similar to links.
map_indexinclude({error, notfound}, _, _) ->
    [];
map_indexinclude(RiakObject, Props, JsonArg) ->
    Bucket = riak_object:bucket(RiakObject),
    Key = riak_object:key(RiakObject),
    InitialList = [{{Bucket, Key}, Props}],
    % Parse config arg
    %%Args = decode_arguments(JsonArg),
    {struct, Args} = mochijson2:decode(JsonArg),
    case proplists:get_value(<<"keep">>, Args) of
        <<"false">> ->
            Keep = false;
        _ ->
            Keep = true
    end,
    case {proplists:get_value(<<"source">>, Args),
            proplists:get_value(<<"target">>, Args),
            proplists:get_value(<<"indexname">>, Args)} of
        {undefined, _, _} ->
            return_list(InitialList, [], Keep);
        {_, undefined, _} ->
            return_list(InitialList, [], Keep);
        {_, _, undefined} ->
            return_list(InitialList, [], Keep);
        {Bucket, Target, IndexName} ->
            Result = get_index_items(Target, Props, IndexName, Key),
            return_list(InitialList, Result, Keep);
        _ ->
            return_list(InitialList, [], Keep)
    end.

%% @spec map_indexlink(riak_object:riak_object(), term(), term()) ->
%%                   [{{Bucket :: binary(), Key :: binary()}, Props :: term()}]
%% @doc map phase function for inclusion based on local secondary index value
map_indexlink({error, notfound}, _, _) ->
    [];
map_indexlink(RiakObject, Props, JsonArg) ->
    Bucket = riak_object:bucket(RiakObject),
    Key = riak_object:key(RiakObject),
    InitialList = [{{Bucket, Key}, Props}],
    {struct, Args} = mochijson2:decode(JsonArg),
    case proplists:get_value(<<"keep">>, Args) of
        <<"false">> ->
            Keep = false;
        _ ->
            Keep = true
    end,
    case {proplists:get_value(<<"source">>, Args),
            proplists:get_value(<<"target">>, Args),
            proplists:get_value(<<"indexname">>, Args)} of
        {undefined, _, _} ->
            return_list(InitialList, [], Keep);
        {_, undefined, _} ->
            return_list(InitialList, [], Keep);
        {_, _, undefined} ->
            return_list(InitialList, [], Keep);
        {Bucket, Target, IndexName} ->
            Result = create_indexlink_list(RiakObject, Props, IndexName, Target),
            return_list(InitialList, Result, Keep);
        _ ->
            return_list(InitialList, [], Keep)
    end.

%% @spec map_metafilter(riak_object:riak_object(), term(), term()) ->
%%                   [{{Bucket :: binary(), Key :: binary()}, Props :: term()}]
%% @doc map phase function for selectively discarding records from the current set
map_metafilter({error, notfound}, _, _) ->
    [];
map_metafilter(RiakObject, Props, JsonArg) ->
    Bucket = riak_object:bucket(RiakObject),
    Key = riak_object:key(RiakObject),
    MetaDataList = riak_object:get_metadatas(RiakObject),
    InitialList = [{{Bucket, Key}, Props}],
    {struct, Args} = mochijson2:decode(JsonArg),
    case {proplists:get_value(<<"source">>, Args),
            proplists:get_value(<<"criteria">>, Args)} of
        {Bucket, undefined} ->
            Result = true;
        {Bucket, []} ->
            Result = true;
        {Bucket, Criteria} when is_list(Criteria)->
            Result = check_criteria(MetaDataList, Criteria);
        {undefined, Criteria} when is_list(Criteria) ->
            Result = check_criteria(MetaDataList, Criteria);
        _ ->
            Result = false
    end,
    case Result of
        true ->
            [];
        _ ->
            InitialList
    end.

%% @spec map_id(riak_object:riak_object(), term(), term()) ->
%%                   [[Bucket :: binary(), Key :: binary()]]
%% @doc map phase function returning bucket name and key in a readable format
map_id({error, notfound}, _, _) ->
    [];
map_id(RiakObject, Props, Arg) when is_list(Arg) ->
    map_id(RiakObject, Props, list_to_binary(Arg));
map_id(RiakObject, Props, Arg) when is_atom(Arg) ->
    map_id(RiakObject, Props, <<"">>);
map_id(RiakObject, _, Arg) when is_binary(Arg) ->
    Bucket = riak_object:bucket(RiakObject),
    Key = riak_object:key(RiakObject),
    case Arg of
        Bucket ->
            [[Bucket, Key]];
        <<"">> ->
            [[Bucket, Key]];
        _ ->
            []
    end;
map_id(_, _, _) ->
    [].
    
%% @spec map_key(riak_object:riak_object(), term(), term()) ->
%%                   [Key :: binary()]
%% @doc map phase function returning object key in a readable format
map_key({error, notfound}, _, _) ->
    [];
map_key(RiakObject, Props, Arg) when is_list(Arg) ->
    map_key(RiakObject, Props, list_to_binary(Arg));
map_key(RiakObject, Props, Arg) when is_atom(Arg) ->
    map_key(RiakObject, Props, <<"">>);
map_key(RiakObject, _, Arg) when is_binary(Arg) ->
    Bucket = riak_object:bucket(RiakObject),
    Key = riak_object:key(RiakObject),
    case Arg of
        Bucket ->
            [Key];
        <<"">> ->
            [Key];
        _ ->
            []
    end;
map_key(_, _, _) ->
    [].   

%% @spec map_datasize(riak_object:riak_object(), term(), term()) ->
%%                   [integer()]
%% @doc map phase function returning size of the data stored in bytes.
%% It does return total size if siblings are found.
map_datasize({error, notfound}, _, _) ->
    [];
map_datasize(RiakObject, _, _) ->
    DataSize = lists:foldl(fun(V, A) ->
                               (byte_size(V) + A)
                           end, 0, riak_object:get_values(RiakObject)),
    [DataSize].

%% hidden
get_index_items(Bucket, Props, IndexName, Value) ->
    {ok, C} = riak:local_client(),
    case C:get_index(Bucket, {eq, IndexName, Value}) of
        {ok, KeyList} ->
            [{{Bucket, K}, Props} || K <- KeyList];
        {error, _} ->
            []
    end.

%% hidden
create_indexlink_list(RiakObject, Props, IndexName, Target) ->
    DictList = riak_object:get_metadatas(RiakObject),
    Result = create_indexlink_list(DictList, Props, IndexName, Target, []),
    sets:to_list(sets:from_list(Result)).

%% hidden
create_indexlink_list([], _Props, _IndexName, _Target, List) ->
    sets:to_list(sets:from_list(List));
create_indexlink_list([Dict | DictList], Props, IndexName, Target, List) ->
    case dict:find(?MD_INDEX, Dict) of
        error ->
            create_indexlink_list(DictList, Props, IndexName, Target, List);
        {ok, IndexList} ->
            case [I || {K, I} <- IndexList, K == IndexName] of
                [] ->
                    create_indexlink_list(DictList, Props, IndexName, Target, List);
                [Indexes] when is_list(Indexes) ->
                    Result = [{{Target, V}, Props} || V <- Indexes],
                    ResList = lists:append(Result, List),
                    create_indexlink_list(DictList, Props, IndexName, Target, ResList);
                [Index] ->
                    ResList = lists:append([{{Target, Index}, Props}], List),
                    create_indexlink_list(DictList, Props, IndexName, Target, ResList)
            end
    end.

%% hidden
return_list(Original, Result, true) ->
    lists:append([Original, Result]);
return_list(_, Result, _) ->
    Result.

%% hidden
check_criteria(MetaDataList, Criteria) when is_list(Criteria) ->
    case parse_criteria(Criteria, []) of
        error ->
            false;
        CList ->
            check_parsed_criteria(MetaDataList, CList)
    end;
check_criteria(_, _) ->
    error.

%% hidden
parse_criteria([], CList) ->
    CList;
parse_criteria([C | R], CList) ->
    case C of
        [Op, Field, Val] ->
            case {Op, parse_field(Field)} of
                {_, error} -> error;
                {<<"eq">>, F} -> parse_criteria(R, lists:append([{eq, F, Val}], CList));
                {<<"neq">>, F} -> parse_criteria(R, lists:append([{neq, F, Val}], CList));
                {<<"greater_than">>, F} -> parse_criteria(R, lists:append([{greater_than, F, Val}], CList));
                {<<"greater_than_eq">>, F} -> parse_criteria(R, lists:append([{greater_than_eq, F, Val}], CList));
                {<<"less_than">>, F} -> parse_criteria(R, lists:append([{less_than, F, Val}], CList));
                {<<"less_than_eq">>, F} -> parse_criteria(R, lists:append([{less_than_eq, F, Val}], CList));
                _ -> error
            end;
        _ -> error
    end.

%% hidden
parse_field(Field) when is_list(Field) ->
    parse_field(list_to_binary(Field));
parse_field(Field) when is_binary(Field) ->
    case Field of
        <<"meta:", Val/binary>> -> {meta, Val};
        <<"index:", Val/binary>> -> {index, Val};
        _ -> error
    end;
parse_field(_) ->
    error.

%% hidden
check_parsed_criteria([], _CList) ->
    false; 
check_parsed_criteria([MetaData | Rest], CList) ->
    case evaluate_criteria(MetaData, CList) of
        true -> true;
        _ -> check_parsed_criteria(Rest, CList)
    end.

%% hidden
evaluate_criteria(_MetaData, []) ->
    true;
evaluate_criteria(MetaData, [{Op, {Type, F}, V} | List]) ->
    case get_metadata_value(MetaData, Type, F) of
        undefined ->
            false;
        Value ->
            case check_value(Op, Value, V) of
                true ->
                    evaluate_criteria(MetaData, List);
                _ ->
                    false
            end
    end.
    
get_metadata_value(MetaData, Type, MetaName) ->
    case Type of
        meta ->
            MetaKey = ?MD_USERMETA,
            MN = binary_to_list(MetaName);
        index ->
            MetaKey = ?MD_INDEX,
            MN = MetaName
    end,
    case dict:find(MetaKey, MetaData) of
        {ok, Value} ->
            case [V || {K, V} <- Value, K == MN] of
                [] -> undefined;
                [V] when is_list(V) -> list_to_binary(V);
                [V] -> V
            end;
        error -> undefined
    end.

%% hidden    
check_value(Op, Value, Param) when is_integer(Value) andalso is_binary(Param) ->
    try list_to_integer(binary_to_list(Param)) of
        Integer -> check_value(Op, Value, Integer)
    catch
        _:_ ->
            BValue = list_to_binary(integer_to_list(Value)),
            BParam = list_to_binary(Param),
            check_value(Op, BValue, BParam)
    end;    
check_value(Op, Value, Param) when is_binary(Value) andalso is_integer(Param) ->
    Pbin = list_to_binary(integer_to_list(Param)),
    check_value(Op, Value, Pbin);
check_value(eq, Value, Param)->
    Value == Param;
check_value(neq, Value, Param) ->
    Value =/= Param;
check_value(greater_than, Value, Param) ->
    Value > Param;
check_value(greater_than_eq, Value, Param) ->
    Value >= Param;
check_value(less_than, Value, Param) ->
    Value < Param;
check_value(less_than_eq, Value, Param) ->
    Value =< Param.
