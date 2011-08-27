%% -------------------------------------------------------------------
%%
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

-module(app_config_edit).
-export([get_config_value/3, update_config_value/4, example/1]).

example(FilePath) -> 
	{ok,[AppConfig]} = file:consult(FilePath),
	[Http] = get_config_value(AppConfig, riak_core, http),
	io:format("HTTP Address: ~p~n", [Http]),
	Updated = update_config_value(AppConfig, riak_core, http, [{"0.0.0.0", 8080}]),
	io:format("Original Values:~n~p~n", [AppConfig]),
	io:format("Updated Values:~n~p~n", [Updated]).	

update_config_value([], _, _, _) ->
	[];
update_config_value([Section | Rest], SectionName, Name, NewValue) ->
	case Section of
		{SectionName, Values} ->
			[{SectionName, update_config_value_in_section(Values, Name, NewValue)} | update_config_value(Rest, SectionName, Name, NewValue)];
		Other ->
			[Other | update_config_value(Rest, SectionName, Name, NewValue)]
	end.

update_config_value_in_section([], _, _) ->
	[];
update_config_value_in_section([Value | Rest], Name, NewValue) ->
	case Value of 
		{Name, _V} ->
			[{Name, NewValue} | update_config_value_in_section(Rest, Name, NewValue)];
		Other ->
			[Other | update_config_value_in_section(Rest, Name, NewValue)]
	end.
	

get_config_value([], _, _) ->
	not_found;
get_config_value([Section | Rest], SectionName, Name) ->
	case Section of
		{SectionName, Values} ->
			get_config_value_in_section(Values, Name);
		_ ->
			get_config_value(Rest, SectionName, Name)
	end.
	
get_config_value_in_section([], _) ->
	not_found;
get_config_value_in_section([Value | Rest], Name) ->
	case Value of 
		{Name, V} ->
			V;
		_ ->
			get_config_value_in_section(Rest, Name)
	end.
