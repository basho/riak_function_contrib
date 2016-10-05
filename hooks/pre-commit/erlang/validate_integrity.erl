-module(validate_integrity).

-compile(export_all).

%%% Verify checksum if present.
%%% Note that the checksum is expected to be base64(md5(Body)).
precommit_verify_cksum({fail,_}=Error) -> Error;
precommit_verify_cksum(Object) ->
    case get_usermeta(<<"Content-MD5">>, Object) of
	error -> Object; % No checksum.
	{ok, ExpectedMD5} ->
	    Contents = riak_object:get_value(Object),
	    ActualMD5 = base64:encode(erlang:md5(Contents)),
	    if ActualMD5 =:= ExpectedMD5 ->
%% 		    io:format("{C}"),
		    Object; % We allow it!
		true ->
		    fail(precommit_verify_cksum,
			 "Bad checksum - expected ~s, computed ~s",
			 [ExpectedMD5, ActualMD5],
			 Object)
	    end
    end.

%%% Verify body size if present.
precommit_verify_size({fail,_}=Error) -> Error;
precommit_verify_size(Object) ->
    case get_usermeta(<<"Byte-count">>, Object) of
	error -> Object; % No size.
	{ok, ExpectedSize} ->
	    Contents = riak_object:get_value(Object),
	    ActualSize = list_to_binary(integer_to_list(byte_size(Contents))),
	    if ActualSize =:= ExpectedSize ->
%% 		    io:format("{S}"),
		    Object; % We allow it!
		true ->
		    fail(precommit_verify_size,"Bad byte-count - expected ~s, computed ~s", [ExpectedSize, ActualSize],
			 Object)
	    end
    end.

%%% Verify that payload is an inflatable (zlib/gzip-compressed) blob.
precommit_verify_compression({fail,_}=Error) -> Error;
precommit_verify_compression(Object) ->
    case get_usermeta(<<"Verify-Compression">>, Object) of
	error -> Object; % No check.
	{ok, Method} ->
	    Contents = riak_object:get_value(Object),
	    try uncompress(Method, Contents) of
		X when is_binary(X) ->
%% 		    io:format("{D}"),
		    Object; % Looks OK.
		_ ->
		    fail(precommit_verify_compression, "Verify-Compression failed.", [],
			 Object)
	    catch
		_:Reason ->
		    fail(precommit_verify_compression, "Verify-Compression failed: ~p", [Reason],
			 Object)
	    end
    end.


%%%==================== Helpers:

uncompress(<<"deflate">>, CompData) -> inflate(CompData);    
uncompress(<<"gzip">>, CompData)    -> zlib:gunzip(CompData);
uncompress(Method, _CompData) ->
    error(format("unknown compression method: ~s", [Method])).

inflate(CompData) ->
    Z = zlib:open(),
    try
	zlib:inflateInit(Z, 15),
	Data = zlib:inflate(Z, CompData),
	zlib:inflateEnd(Z),
	Data
    after
	zlib:close(Z)
    end.


fail(Tag, Format, Args, Object) ->
    ErrTxt = format(Format, Args),
    error_logger:error_msg("~p: ~s rejected write: ~s\n", [?MODULE, Tag, ErrTxt]),
    (Object == undefined) orelse
	error_logger:error_msg("~p: Dump of rejected object (~s):\n  ~p\n", [?MODULE, ErrTxt, Object]),
    {fail, ErrTxt}.

format(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

get_usermeta(Key, Object) ->
    MetaDict = riak_object:get_metadata(Object),
    UserMeta = dict:fetch(<<"X-Riak-Meta">>, MetaDict),
    UserMetaLC = [{string:to_lower(K), V} || {K,V} <- UserMeta],
    KeyLC = string:to_lower("X-Riak-Meta-" ++ binary_to_list(Key)),
    case lists:keyfind(KeyLC, 1, UserMetaLC) of
	false ->
	    error;
	{_,Value} ->
	    {ok, list_to_binary(Value)}
    end.
