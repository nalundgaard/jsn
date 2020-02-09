%%------------------------------------------------------------------------------
%% jsn - Functions for interacting with decoded JSON objects 
%%
%% @author Nicholas Lundgaard <nalundgaard@gmail.com>
%%------------------------------------------------------------------------------
-module(jsn).

-author("Nicholas Lundgaard <nalundgaard@gmail.com>").

-include("jsn.hrl").

%% lookup/storage API
-export([
    new/0, new/1, new/2,
    get/2, get/3,
    get_list/2, get_list/3,
    find/3, find/4,
    select/2, select/3,
    set/3, set_list/2,
    delete/2, delete_list/2,
    delete_if_equal/3,
    copy/3, copy/4,
    transform/2,
    path_transform/2,
    path_elements/1
]).
%% comparison functions
-export([equal/3, equal/4]).
-export([is_equal/2]).
-export([is_subset/2]).
%% object format conversion
-export([as_map/1, from_map/1, from_map/2,
         as_proplist/1, from_proplist/1, from_proplist/2]).

-ifdef(TEST).
-compile([export_all]).
-endif.

%%==============================================================================
%% types
%%==============================================================================

-export_type([json_proplist/0, json_eep18/0, json_struct/0,
              json_object/0,
              json_term/0,
              path/0, paths/0,
              jsn_option/0, jsn_options/0]).

%%==============================================================================
%% constants
%%==============================================================================

-define(DEFAULT_FORMAT, map).

%% guard for matching a JSON string, boolean, number, null
-define(IS_SIMPLE_JSON_TERM(X), 
        is_binary(X);
        is_boolean(X);
        is_number(X);
        X =:= null ).

%% guard for JSON key
-define(IS_JSON_KEY(K),
        is_binary(K);
        is_atom(K)).

%%==============================================================================
%% lookup/storage API
%%==============================================================================

-spec new() -> json_object().
%%------------------------------------------------------------------------------
%% @doc return an empty json_object in the default format
%%------------------------------------------------------------------------------
new() ->
    empty_object([]).

-spec new(path_value_tuple() | path_value_tuples() ) ->  json_object().
%%------------------------------------------------------------------------------
%% @doc given a path, value tuple or a list of such tuples, return a new
%% json_object in the default format with the given path(s) and value(s)
%%------------------------------------------------------------------------------
new(L) ->
    new(L, []).


-spec new(path_value_tuple() | path_value_tuples(), 
          jsn_options()) -> json_object().
%%------------------------------------------------------------------------------
%% @doc given a path, value tuple or a list of such tuples, return a new
%% json_object with the given path(s) and value(s)
%%------------------------------------------------------------------------------
new(L, Options) when is_tuple(L) ->
    new([L], Options);

new(L, Options) when is_list(L) ->
    set_list(L, empty_object(Options)).


-spec get(path(), json_object() | json_array()) -> 
         json_term() | undefined.
%%------------------------------------------------------------------------------
%% @doc given a path and a json_object, if the path is in the object, return
%% the value at the path. Otherwise, return undefined
%%------------------------------------------------------------------------------
get(Path, Object) ->
    get(Path, Object, undefined).


-spec get(path(), json_term(), term()) -> json_term() | term().
%%------------------------------------------------------------------------------
%% @doc given a path and a json_object, if the path is in the object, return
%% the value at the path. Otherwise, return Default
%%------------------------------------------------------------------------------
get(Path, Object, Default) ->
    Keys = path_elements(Path),
    try
        keys_get(Keys, Object, Default)
    catch
        error:_ ->
            Default
    end.


-spec get_list(paths(), json_term()) -> [ json_term() | undefined ].
%%------------------------------------------------------------------------------
%% @doc given a list of paths and a json_object, return the values found
%% at each path in a list; if a path does not exist, return `undefined'
%%------------------------------------------------------------------------------
get_list(PathList, Object) ->
    get_list(PathList, Object, undefined).


-spec get_list(paths(),
               json_term(),
               Default :: term()) -> [ json_term() | term() ].
%%------------------------------------------------------------------------------
%% @doc given a list of paths and a json_object, return the values found
%% at each path in a list; if a path does not exist, return `undefined'
%%------------------------------------------------------------------------------
get_list(PathList, Object, Default) ->
    lists:map(fun(Path) ->
                  get(Path, Object, Default)
              end,
              PathList).


-spec find(path(), 
           SearchTerm :: json_term(),
           json_array()) -> json_array().
%%------------------------------------------------------------------------------
%% @doc given a path, search term, and a list of json objects, find all the
%% elements of the list where the path in the element matches the search term
%%------------------------------------------------------------------------------
find(Path, Value, Objects) ->
    lists:filter(fun(E) ->
                     get(Path, E) == Value
                 end,
                 Objects).


-spec find(path(),
           Subpath :: path(),
           SearchTerm :: json_term(),
           json_object() | json_array() ) -> json_array().
%%------------------------------------------------------------------------------
%% @doc given a path, a subpath to be search objects in that list, a search
%% term, and an object containing a list of objects at the given path, find all
%% the elements of the list where the path in the element matches the given
%% search term
%%------------------------------------------------------------------------------
find(Path, Subpath, SearchTerm, Object) ->
    find(Subpath, SearchTerm, get(Path, Object)).


-spec select(selection()|selections(), json_array()) -> json_array().
%%------------------------------------------------------------------------------
%% @doc Transforms a list of Elements according to a given Selectionification
%%------------------------------------------------------------------------------
select(Selection, Elements) ->
    select(Selection, [], Elements).

-spec select(selection()|selections(),
             condition()|conditions(),
             json_array()) -> json_array().
%%------------------------------------------------------------------------------
%% @doc Same as select/2 but it first filters the elements according to a given
%% list of Conditions
%%------------------------------------------------------------------------------
select(Selection, Condition, Elements) when is_list(Elements) ->
    lists:filtermap(fun(Element) ->
        case apply_conditions(Condition, Element) of
            true ->
                {true, apply_selections(Selection, Element)};
            false ->
                false
        end
    end, Elements);
select(Selection, Conditions, Elements) ->
    erlang:error(badarg, [Selection, Conditions, Elements]).


-spec set(path(), json_object(), Value :: json_term()) -> json_object();
         (path(), json_array(), Value :: json_term()) -> json_array().
%%------------------------------------------------------------------------------
%% @doc given a path, value, and a json_object, return a new json_object
%% containing the path and value
%%------------------------------------------------------------------------------
set(Path, Object, Value) ->
    keys_set(path_elements(Path), Object, Value).


-spec set_list(path_value_tuples(), json_object()) -> json_object();
              (path_value_tuples(), json_array()) -> json_array().
%%------------------------------------------------------------------------------
%% @doc given a list of `{Path, Value}' tuples, apply the `set/3' function
%% to the each path and value pair with the given object, and return the result
%%------------------------------------------------------------------------------
set_list(TupleList, Object) ->
    lists:foldl(fun({K, V}, Acc) -> set(K, Acc, V) end, Object, TupleList).


-spec delete(path(), json_object()) -> json_object();
            (path(), json_array()) -> json_array().
%%------------------------------------------------------------------------------
%% @doc given a path and a json_object, if the path is in the object, return
%% a new json_object identical to the given one, with the exception that it
%% does not contain the path, value pair referred to by the input path
%%------------------------------------------------------------------------------
delete(Path, Object) ->
    keys_set(path_elements(Path), Object, jsn__delete).


-spec delete_list(paths(), json_object()) -> json_object();
                 (paths(), json_array()) -> json_array().
%%------------------------------------------------------------------------------
%% @doc given a list of paths and a json_object, apply delete/2 to each path
%% on the same json_object and return the result
%%------------------------------------------------------------------------------
delete_list(PathList, Object) ->
    lists:foldl(fun(Path, Acc) -> delete(Path, Acc) end, Object, PathList).


-spec delete_if_equal
     (path(), ValueOrValues :: json_term(), json_object()) -> json_object();
     (path(), ValueOrValues :: json_term(), json_array()) -> json_array().
%%------------------------------------------------------------------------------
%% @doc given a path, a value or list of values, and a json term, check if the
%% path contains any of the given Value(s); if so, delete the path from the
%% object
%%------------------------------------------------------------------------------
delete_if_equal(Path, Values, Object) when is_list(Values) ->
    Val = get(Path, Object),
    case lists:member(Val, Values) orelse Val =:= Values of
        true -> delete(Path, Object);
        _ -> Object
    end;
delete_if_equal(Path, Value, Object) ->
    delete_if_equal(Path, [Value], Object).


-spec copy(paths(),
           SrcObject :: json_object() | json_array(),
           DstObjects :: json_object() | json_array()) -> json_array().
%%------------------------------------------------------------------------------
%% @doc given a list of paths, a source object/array and one or more destination 
%% objects/arrays, copy the path-value pairs from the source to the 
%% destination(s), and return a list with the results
%%
%% if the path does not exist in object, the destination(s) will have the
%% atom `undefined' as the value
%%
%% this function always returns a list of objects
%%------------------------------------------------------------------------------
copy(PathList, Src, Dst) ->
    copy(PathList, Src, Dst, fun(Value) -> Value end).


-spec copy
     (paths(),
      SrcObject :: json_object() | json_array(),
      DstObjects :: json_object() | json_array(),
      Mutator :: fun((json_term() | undefined) -> json_term())) -> json_array().
%%------------------------------------------------------------------------------
%% @doc given a list of paths, a source json_object and one or more destination
%% json_objects, retrieve the value from the source object at the current path, 
%% pass it through the mutator function, and store it on the destination
%% object(s) at the same path; return the destination object(s)
%%
%% if the path does not exist in object, the destination(s) will have the
%% atom `undefined' as the value; the passed function should be prepared
%% to handle a value of `undefined'
%%
%% this function always returns a list of objects
%%------------------------------------------------------------------------------
copy(PathList, Src, [], Mutate) ->
    copy(PathList, Src, [[]], Mutate);
copy(PathList, Src, [{K,_}|_] = P, Mutate) when K =/= struct ->
    copy(PathList, Src, [P], Mutate);
copy(PathList, Src, Dst, Mutate) when is_list(PathList),
                                      is_list(Dst),
                                      is_function(Mutate) ->
    lists:map(fun(DstObj) ->
                 lists:foldl(fun(Path, Acc) ->
                                 set(Path, Acc, Mutate(get(Path, Src)))
                             end,
                             DstObj,
                             PathList)
              end,
              Dst);
copy(PathList, Src, Dst, Mutator) when not(is_list(Dst)) ->
    copy(PathList, Src, [Dst], Mutator);
copy(PathList, Src, Dst, Mutator) ->
    erlang:error(badarg, [PathList, Src, Dst, Mutator]).


-spec transform(Transforms :: [{path(), fun((json_term()) -> json_term())}],
                json_object()) -> json_object().
%%------------------------------------------------------------------------------
%% @doc given a list of transforms as `{path, fun/1}', modify the input object
%% by retrieving the value in the path, applying the given function and saving
%% the result into a new return object
%%
%% it's possible that the given path may specify a value that is not present;
%% if so, the transformation function will be given a value of `undefined'
%%------------------------------------------------------------------------------
transform(Transforms, Object) when is_list(Transforms) ->
    lists:foldl(
        fun({Path, Transform}, Acc) ->
            set(Path, Acc, Transform(get(Path, Object)))
        end,
        Object,
        Transforms);
transform(Transforms, Object) ->
    erlang:error(badarg, [Transforms, Object]).


-spec path_transform(Transforms :: [ {path(), path()} ],
                     json_object())  ->  json_object().
%%------------------------------------------------------------------------------
%% @doc given a list of path transforms as `{old_path, new_path}', modify the
%% input object by renaming the old path to the new path and saving
%% the result into a new return object
%%
%% it's possible that the given path may specify an existing path; if so, the
%% transformation will replace the existing path with the old path value
%%
%% if the old_path is not present, the existing object will be return
%%------------------------------------------------------------------------------
path_transform(Transforms, Object) when is_list(Transforms) ->
    lists:foldl(fun({OldPath, NewPath}, Acc) ->
                    case get(OldPath, Object, jsn__undefined) of
                        jsn__undefined -> Acc;
                        Value -> set(NewPath, delete(OldPath, Acc), Value)
                    end
                end,
                Object,
                Transforms);
path_transform(Transforms, Object) ->
    erlang:error(badarg, [Transforms, Object]).


-spec path_elements(path()) -> path_elements().
%%------------------------------------------------------------------------------
%% @doc given a path, parse it into an ordered list of json keys (binary) and/or 
%% array indexes
%%------------------------------------------------------------------------------
path_elements(Path) when is_binary(Path) ->
    binary:split(Path, <<".">>, [global]);
path_elements(Path) when is_atom(Path) ->
    path_elements(atom_to_binary(Path, utf8));
path_elements(Path) when is_list(Path) ->
    path_elements(list, lists:reverse(Path), []);
path_elements(Path) when is_tuple(Path) ->
    path_elements(tuple, lists:reverse(tuple_to_list(Path)), []);
path_elements(Path) -> erlang:error(badarg, [Path]).


path_elements(_Type, [], Acc) ->
    Acc;
path_elements(Type, [Key | Rest], Acc) when is_binary(Key) ->
    path_elements(Type, Rest, [Key | Acc]);
path_elements(list, [Key | Rest], Acc) when is_atom(Key)  ->
    path_elements(list, Rest, [atom_to_binary(Key, utf8) | Acc]);
path_elements(tuple, [Index | Rest], Acc) when is_integer(Index), Index > 0 ->
    path_elements(tuple, Rest, [Index | Acc]);
path_elements(tuple, [Index | Rest], Acc) when Index =:= first; Index =:= last ->
    path_elements(tuple, Rest, [Index | Acc]);
path_elements(Type, Path, Acc) ->
    erlang:error(badarg, [Type, Path, Acc]).

%%==============================================================================
%% comparison API
%%==============================================================================

-spec equal
     (paths(),
      Original :: json_object() | json_array(),
      OtherOrOthers :: json_object() | json_array()) ->
     ok | {error, {not_equal, Message :: binary() }}.
%%------------------------------------------------------------------------------
%% @doc given a list of paths, an original json_object, and a single or
%% list of new json_objects, verify that each path in each of the other
%% object(s) has the same value as the original does at the same path, for
%% each path in the list of paths; if so, return ok; otherwise, return an
%% error tuple with the error type and a summary of mismatches for the
%% first mismatched object
%%------------------------------------------------------------------------------
equal(Paths, OriginalObject, OtherObjectOrObjects) ->
    equal(Paths, OriginalObject, OtherObjectOrObjects, hard).


-spec equal
     (Paths :: paths(),
      OriginalObject :: json_object() | json_array(),
      OtherObjectOrObjects :: json_object() | json_array(),
      Mode :: soft | hard) -> 
     ok | {error, {not_equal, Message :: binary() }}.
%%------------------------------------------------------------------------------
%% @doc given a list of fields, an original json_object, and a single or
%% list of new json_objects, verify that each path in each of the other
%% object(s) has the same value as the original does at the same path, for
%% each path in the list of paths; if so, return ok; otherwise, return an
%% error tuple with the error type and a summary of mismatches for the
%% first mismatched object
%%
%% if mode is soft, a mismatch is allowed if the value is missing from any of
%% the new objects
%%------------------------------------------------------------------------------
equal(Paths, OriginalObject, [], Mode) ->
    object_equal(Paths, OriginalObject, [], Mode);
equal(Paths, OriginalObject, [{K,_}|_] = P, Mode) when K =/= struct ->
    object_equal(Paths, OriginalObject, P, Mode);
equal(Paths, OriginalObject, {_} = OtherObject, Mode) ->
    object_equal(Paths, OriginalObject, OtherObject, Mode);
equal(Paths, OriginalObject, {struct, _} = OtherObject, Mode) ->
    object_equal(Paths, OriginalObject, OtherObject, Mode);
equal(Paths, OriginalObject, OtherObject, Mode) when is_map(OtherObject) ->
    object_equal(Paths, OriginalObject, OtherObject, Mode);
equal(Paths, OriginalObject, OtherObjects, Mode) when is_list(OtherObjects) ->
    try
        lists:foreach(fun(OtherObject) ->
                          ok = object_equal(Paths, OriginalObject, OtherObject, Mode)
                      end,
                      OtherObjects)
    catch
        error:{badmatch, {error, {not_equal, Message}}} ->
            {error, {not_equal, Message}}
    end.


-spec is_equal(json_term(), json_term()) -> boolean().
%%------------------------------------------------------------------------------
%% @doc given 2 json terms A and B in any format (eep18, struct, proplist, or
%% map, if supported), return true if they are equivalent
%%------------------------------------------------------------------------------
is_equal({A}, B) when is_list(A) ->
    is_equal(maps:from_list(A), B);
is_equal({struct, A}, B) when is_list(A) ->
    is_equal(maps:from_list(A), B);
is_equal(A, {B}) when is_list(B) ->
    is_equal(A, maps:from_list(B));
is_equal(A, {struct, B}) when is_list(B) ->
    is_equal(A, maps:from_list(B));
is_equal([{K, _V}|_T] = A, B) when is_binary(K); is_atom(K), K =/= struct ->
    is_equal(maps:from_list(A), B);
is_equal(A, [{K, _V}|_T] = B) when is_binary(K); is_atom(K), K =/= struct ->
    is_equal(A, maps:from_list(B));
is_equal(A, B) when is_map(A), is_map(B) ->
    AKeys = maps:keys(A),
    AKeys == maps:keys(B) andalso compare_maps(AKeys, fun is_equal/2, A, B);
is_equal([HA|TA] = _A, [HB|TB] = _B) ->
    is_equal(HA, HB) andalso is_equal(TA, TB);
is_equal(A, A) when ?IS_SIMPLE_JSON_TERM(A); A =:= [] ->
    true;
is_equal(_A, _B) ->
    false.


-spec is_subset(A :: json_term(), B :: json_term()) ->  boolean().
%%------------------------------------------------------------------------------
%% @doc given 2 json terms A and B in any format (eep18, struct, proplist),
%% return true if all the value(s) in A are present in B
%%
%% CAUTION: this comparison treats json array comparisons as subset comparisons,
%% not just object comparisons. so, `is_subset([1,1,1], [1,2])' is `true'
%%------------------------------------------------------------------------------
is_subset({A}, B) when is_list(A) ->
    is_subset(maps:from_list(A), B);
is_subset({struct, A}, B) when is_list(A) ->
    is_subset(maps:from_list(A), B);
is_subset(A, {B}) when is_list(B) ->
    is_subset(A, maps:from_list(B));
is_subset(A, {struct, B}) when is_list(B) ->
    is_subset(A, maps:from_list(B));
is_subset([{K, _V}|_T] = A, B) when is_binary(K); is_atom(K), K =/= struct ->
    is_subset(maps:from_list(A), B);
is_subset(A, [{K, _V}|_T] = B) when is_binary(K); is_atom(K), K =/= struct ->
    is_subset(A, maps:from_list(B));
is_subset(A, B) when is_map(A), is_map(B) ->
    compare_maps(maps:keys(A), fun is_subset/2, A, B);
is_subset([H|T] = _A, B) when is_list(B)  ->
    lists:any(fun(X) -> is_subset(H, X) end, B) andalso is_subset(T, B);
is_subset([] = _A, B) when is_map(B); is_list(B) ->
    true;
is_subset(A, A) when ?IS_SIMPLE_JSON_TERM(A) ->
    true;
is_subset(_A, _B) ->
    false.

%%==============================================================================
%% conversion API
%%==============================================================================

-spec as_map(json_term()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc convert any JSON objects in the input JSON term into map format
%%------------------------------------------------------------------------------
as_map(M) when is_map(M) ->
    maps:map(fun(Key, Value) when ?IS_JSON_KEY(Key) ->
                 as_map(Value);
                (Key, Value) -> erlang:error(badarg, [Key, Value]) 
             end,
             M);
as_map({struct, P}) when is_list(P) ->
    as_map(P);
as_map({P}) when is_list(P) ->
    as_map(P);
as_map([{struct, H}|T]) when is_list(H) ->
    [as_map(H) | as_map(T)];
as_map([{H}|T]) when is_list(H) ->
    [as_map(H) | as_map(T)];
as_map([{_K, _V}|_T] = P) ->
    M = maps:from_list(P),
    maps:map(fun(Key, Value) when ?IS_JSON_KEY(Key) ->
                 as_map(Value);
                (Key, Value) ->
                 erlang:error(badarg, [Key, Value])
             end,
             M);
as_map(L) when is_list(L) ->
     [as_map(Value) || Value <- L];
as_map(T) when ?IS_SIMPLE_JSON_TERM(T) -> T;
as_map(T) -> erlang:error(badarg, [T]).


-spec from_map(json_term()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc convert a JSON term with map-format JSON objects into an identical JSON
%% term with all of the JSON objects converted into the default format
%%------------------------------------------------------------------------------
from_map(T) ->
    from_map(T, []).


-spec from_map(json_term(), jsn_options()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc convert a JSON term with map-format JSON objects into an identical JSON
%% term with all of the JSON objects converted into the specified object format
%%------------------------------------------------------------------------------
from_map(M, Opts) -> 
    from_map_impl(M, get_format(Opts), Opts).


-spec as_proplist(json_term()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc convert any JSON objects in the input JSON term into proplist format
%%------------------------------------------------------------------------------
as_proplist(M) when is_map(M) ->
    maps:fold(fun(Key, Value, Acc) when ?IS_JSON_KEY(Key) ->
                  [{Key, as_proplist(Value)} | Acc];
                 (Key, Value, Acc) ->
                  erlang:error(badarg, [Key, Value, Acc])
              end,
              [],
              M);
as_proplist({struct, List}) when is_list(List) ->
    as_proplist(List);
as_proplist({List}) when is_list(List) ->
    as_proplist(List);
as_proplist([{struct, H}|T]) when is_list(H) ->
    [as_proplist(H) | as_proplist(T)];
as_proplist([{H}|T]) when is_list(H) ->
    [as_proplist(H) | as_proplist(T)];
as_proplist([{_K, _V}|_T] = Plist) ->
    lists:map(fun({Key, Value}) when ?IS_JSON_KEY(Key) ->
                  {Key, as_proplist(Value)};
                 (Element) -> erlang:error(badarg, [Element]) 
              end,
              Plist);
as_proplist(T) when is_list(T) ->
     [as_proplist(Value) || Value <- T];
as_proplist(T) when ?IS_SIMPLE_JSON_TERM(T) -> T;
as_proplist(T) -> erlang:error(badarg, [T]).


-spec from_proplist(json_term()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc convert a JSON term with proplist-format JSON objects into an identical
%% JSON term with all of the JSON objects converted into the default format
%%------------------------------------------------------------------------------
from_proplist(T) ->
    from_proplist(T, []).


-spec from_proplist(json_term(), jsn_options()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc convert a JSON term with proplist-format JSON objects into an identical
%% JSON term with all of the JSON objects converted into the specified object
%% format
%%------------------------------------------------------------------------------
from_proplist(T, Opts) ->
    from_proplist_impl(T, get_format(Opts), Opts).

%%==============================================================================
%% internal functions
%%==============================================================================

-spec apply_conditions(conditions(), json_term()) -> boolean().
apply_conditions([Condition|Rest], Element) ->
    case apply_condition(Condition, Element) of
        true ->
            apply_conditions(Rest, Element);
        false ->
            false
    end;
apply_conditions([], _Element) ->
    true;
apply_conditions(Condition, Element) ->
    apply_condition(Condition, Element).

-spec apply_condition(condition(), json_term()) -> boolean().
apply_condition({Path, ConditionFun}, Element) when is_function(ConditionFun, 1) ->
    ConditionFun(get(Path, Element));
apply_condition({Path, Value}, Element) ->
    get(Path, Element) == Value;
apply_condition(ConditionFun, Element) when is_function(ConditionFun, 1) ->
    ConditionFun(Element);
apply_condition(Condition, Element) ->
    erlang:error(badarg, [Condition, Element]).


-spec apply_selections(selections(), json_term()) -> term().
apply_selections(Selections, Element) when is_list(Selections) ->
    [apply_selection(Selection, Element) || Selection <- Selections];
apply_selections(Selection, Element) ->
    apply_selection(Selection, Element).

-spec apply_selection(selection(), json_term()) -> term().
apply_selection(identity, Element) ->
    Element;
apply_selection({value, Path}, Element) ->
    get(Path, Element);
apply_selection({value, Path, Default}, Element) ->
    get(Path, Element, Default);
apply_selection(Selection, Element) ->
    erlang:error(badarg, [Selection, Element]).


-spec from_proplist_impl(json_term(), format(), jsn_options()) -> json_term().
%%------------------------------------------------------------------------------
%% @private implement `from_proplist/2' with the format extracted
%%------------------------------------------------------------------------------
from_proplist_impl([{_,_}|_] = P, map, Opts) ->
    M = maps:from_list(P),
    maps:map(fun(Key, Value) when ?IS_JSON_KEY(Key) ->
                 from_proplist_impl(Value, map, Opts);
                (Key, Value) ->
                 erlang:error(badarg, [Key, Value])
             end,
             M);
from_proplist_impl([{_,_}|_] = P0, Format, Opts) ->
    P = lists:map(fun({Key, Value}) when ?IS_JSON_KEY(Key) ->
                      {Key, from_proplist_impl(Value, Format, Opts)};
                     (Elem) -> erlang:error(badarg, [Elem]) 
                  end,
                  P0),
    case Format of
        proplist -> P;
        struct -> {struct, P};
        eep18 -> {P}
    end;
from_proplist_impl(L, Format, Opts) when is_list(L) ->
    [from_proplist_impl(Value, Format, Opts) || Value <- L];
from_proplist_impl(T, _Format, _Opts) when ?IS_SIMPLE_JSON_TERM(T) ->
    T;
from_proplist_impl(T, Format, Opts) ->
    erlang:error(badarg, [T, Format, Opts]).


-spec from_map_impl(json_term(), format(), jsn_options()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc implement `from_map/2' with the format extracted
%%------------------------------------------------------------------------------
from_map_impl(M, map, Opts) when is_map(M) ->
    maps:map(fun(Key, Value) when ?IS_JSON_KEY(Key) ->
                 from_map_impl(Value, map, Opts);
                (Key, Value) -> erlang:error(badarg, [Key, Value]) 
             end,
             M);
from_map_impl(M, Format, Opts) when is_map(M) ->
    P = maps:fold(fun(Key, Value, Acc) when ?IS_JSON_KEY(Key) ->
                      [{Key, from_map_impl(Value, Format, Opts)} | Acc];
                     (Key, Value, Acc) -> erlang:error(badarg, [Key, Value, Acc])
                  end,
                  [],
                  M),
    case Format of
        proplist -> P;
        struct -> {struct, P};
        eep18 -> {P}
    end;
from_map_impl(L, Format, Opts) when is_list(L) ->
    [from_map_impl(Value, Format, Opts) || Value <- L];
from_map_impl(T, _Format, _Opts) when ?IS_SIMPLE_JSON_TERM(T) ->
    T;
from_map_impl(T, Format, Opts) ->
    erlang:error(badarg, [T, Format, Opts]).


-spec get_format(jsn_options()) -> format().
%%------------------------------------------------------------------------------
%% @private given jsn options, get the selected format
%%------------------------------------------------------------------------------
get_format(Options) ->
    case lists:keyfind(format, 1, Options) of
        {_, map}      -> map;
        {_, proplist} -> proplist;
        {_, eep18}    -> eep18;
        {_, struct}   -> struct;
        false         -> ?DEFAULT_FORMAT;
        _ ->
            erlang:error(badarg, [Options])
    end.


-spec empty_object(jsn_options()) -> json_object().
%%------------------------------------------------------------------------------
%% @private create a new json_object in the specified format
%%------------------------------------------------------------------------------
empty_object(Options) ->
    case get_format(Options) of
        map      -> ?EMPTY_MAP;
        proplist -> ?EMPTY_PROPLIST;
        eep18    -> ?EMPTY_EEP18;
        struct   -> ?EMPTY_STRUCT
    end.


-spec keys_set(path_elements(),
               Object :: json_object() | json_array(), 
               Value :: json_term() | jsn__delete) ->
              json_object() | json_array().
%%------------------------------------------------------------------------------
%% @private given a path as list of binary json_keys, a json_object, and a
%% value, update the object at the location defined by the path to the given
%% to be value, and return the updated object
%%------------------------------------------------------------------------------
keys_set(Keys, Object, Value) when is_map(Object) ->
    keys_set(Keys, Object, Value, empty_object([{format, map}]));
keys_set(Keys, {P} = Object, Value) when is_list(P) ->
    keys_set(Keys, Object, Value, empty_object([{format, eep18}]));
keys_set(Keys, {struct, P} = Object, Value) when is_list(P) ->
    keys_set(Keys, Object, Value, empty_object([{format, struct}]));
keys_set(Keys, P, Value) when is_list(P) ->
    keys_set(Keys, P, Value, empty_object([{format, proplist}]));
keys_set(_Keys, Term, _Value) ->
    throw({error, {not_an_object, Term}}).


-spec keys_set(path_elements(),
               Object :: json_object() | json_array(), 
               Value :: json_term() | jsn__delete,
               Empty :: json_object()) -> json_object() | json_array().
%%------------------------------------------------------------------------------
%% @private implements `keys_set/3', using the given empty object for generating
%% new objects if none is present in an intermediate layer of the key list
%%------------------------------------------------------------------------------
keys_set([], _Object, Value, _Empty) ->
    Value;
keys_set(Keys, {struct, P}, Value, Empty) when is_list(P) ->
    {struct, keys_set(Keys, P, Value, Empty)};
keys_set(Keys, {P}, Value, Empty) when is_list(P) ->
    {keys_set(Keys, P, Value, Empty)};
keys_set([Key | Rest], Object, Value, Empty)
  when is_binary(Key), (is_list(Object) orelse is_map(Object)) ->
    case key_get(Key, Object, jsn__undefined) of
        E when Value =:= jsn__delete, (E =:= jsn__undefined orelse E =:= Empty) ->
            return_if_object(Object, Empty);
        E when E =:= jsn__undefined; E =:= Empty ->
            key_set(Key, Object, keys_set(Rest, Empty, Value, Empty)); 
        SubValue ->
            key_set(Key, Object, keys_set(Rest, SubValue, Value, Empty))
    end;
keys_set([Index | Rest], A, Value, Empty) when is_integer(Index); 
                                               Index =:= first;
                                               Index =:= last ->
    set_nth(Index, A, keys_set(Rest, get_nth(Index, A, Empty), Value, Empty)).


-spec key_set(binary(),
              Object :: json_proplist(), 
              Value :: json_term() | jsn__delete) -> json_proplist().
%%------------------------------------------------------------------------------
%% @private given a key, a json_object, and a value, store the value in the
%% object at the key; if the value is a deletion, remove the key, value pair, 
%% if it exists
%%------------------------------------------------------------------------------
key_set(Key, M, jsn__delete) when is_binary(Key) andalso is_map(M) ->
    maps:remove(Key, maps:remove(safe_binary_to_atom(Key), M));
key_set(Key, M, Value) when is_binary(Key) andalso is_map(M) -> 
    maps:put(Key, Value, M);
key_set(Key, [], jsn__delete) when is_binary(Key) -> 
    [];
key_set(Key, [{_,_}|_] = P, jsn__delete) when is_binary(Key) -> 
    lists:keydelete(Key, 1, lists:keydelete(safe_binary_to_atom(Key), 1, P));
key_set(Key, [], Value) when is_binary(Key) -> 
    [{Key, Value}];
key_set(Key, [{_,_}|_] = P, Value) when is_binary(Key), is_list(P) -> 
    lists:keystore(Key, 1, P, {Key, Value});
key_set(_Key, Term, _Value) ->
    throw({error, {not_an_object, Term}}).


-spec set_nth(Index :: json_array_index(),
              Array :: json_array(),
              Value :: json_term() | jsn__delete) -> json_array().
%%------------------------------------------------------------------------------
%% @private given a json array index, a json array, and a value to set, replace
%% the element with the given value; if the index is one greater than the
%% array length, append a new entry
%%------------------------------------------------------------------------------
set_nth(_Index, [{Tag, _}|_] = P, _V) when Tag =/= struct ->
    throw({error, {not_an_array, P}});
set_nth(First, [_H|T], jsn__delete) when First =:= 1; First =:= first ->
    T;
set_nth(First, [_H|T], V) when First =:= 1; First =:= first ->
    [V|T];
set_nth(last, A, jsn__delete) when is_list(A) ->
    lists:reverse(tl(lists:reverse(A)));
set_nth(last, A, V) when is_list(A)  ->
    lists:reverse([V|tl(lists:reverse(A))]);
set_nth(Index, A, jsn__delete) when Index > 0, Index =< length(A) ->
    {A1, A2} = lists:split(Index - 1, A),
    lists:concat([A1, tl(A2)]);
set_nth(Index, A, V) when Index > 0, Index =< length(A) ->
    {A1, A2} = lists:split(Index - 1, A),
    lists:concat([A1, [V|tl(A2)]]);
set_nth(Index, A, V) when Index - 1 =:= length(A) ->
    lists:reverse([V|lists:reverse(A)]); 
set_nth(_Index, Term, _V) when not(is_list(Term)) ->
    throw({error, {not_an_array, Term}});
set_nth(Index, _A, _V) -> 
    throw({error, {invalid_array_index, Index}}).


-spec return_if_object(MaybeObject :: json_object(),
                       Empty :: json_object()) -> json_object().
%%------------------------------------------------------------------------------
%% @private given a MaybeObject and the Empty object for the format,
%% return the object if it is one, or else throw a `not_an_object' error
%%------------------------------------------------------------------------------
return_if_object(M, _Empty) when is_map(M) ->
    M;
return_if_object([{_,_}|_] = P, _Empty) ->
    P;
return_if_object(Empty, Empty) ->
    Empty;
return_if_object(Term, _Empty) ->
    throw({error, {not_an_object, Term}}).


-spec keys_get(path_elements(),
               json_term(),
               Default :: term()) -> json_term() | term().
%%------------------------------------------------------------------------------
%% @private given a list of path elements, and a json term, iteratively 
%% lookup the path elements in the object and return the value found at the
%% last key, if present; otherwise, return the default.
%%------------------------------------------------------------------------------
keys_get([], Object, _Default) ->
    Object;
keys_get([Key | Rest], Object, Default) ->
    keys_get(Rest, key_get(Key, Object, Default), Default).


-spec key_get(path_element(),
              json_term(),
              Default :: term()) -> json_term() | term().
%%------------------------------------------------------------------------------
%% @private get the value of a key from a json object or json proplist object,
%% or return undefined; this is function does not support nested keys (i.e., 
%% paths), only single, flat keys
%%------------------------------------------------------------------------------
key_get(Key, M, Default) when is_map(M) ->
    case maps:find(Key, M) of
        error ->
            case maps:find(safe_binary_to_atom(Key), M) of
                error -> Default;
                {ok, Value0} -> Value0
            end;
        {ok, Value} -> Value
    end;
key_get(Key, {P}, Default) when is_list(P) ->
    key_get(Key, P, Default);
key_get(Key, {struct, P}, Default) when is_list(P) ->
    key_get(Key, P, Default);
key_get(Index, A, Default) when is_integer(Index); Index =:= first; Index =:= last ->
    get_nth(Index, A, Default);
key_get(Key, [{_,_}|_] = P, Default) when is_binary(Key) ->
    case lists:keyfind(Key, 1, P) of
        false -> 
            case lists:keyfind(safe_binary_to_atom(Key), 1, P) of
                false -> Default;
                {_, Value0} -> Value0
            end;
        {_, Value} -> Value
    end;
key_get(_Key, _List, Default) ->
    Default.


-spec get_nth(Index :: json_array_index(),
              Array :: json_term(),
              Default :: term()) -> json_term() | term().
%%------------------------------------------------------------------------------
%% @private given a json array index, a json array, and a default value, get
%% the element at the given index, or return the default
%%------------------------------------------------------------------------------
get_nth(_Index, [], Default) ->
    Default;
get_nth(first, [First|_], Default) ->
    get_if_valid(First, Default);
get_nth(last, A, Default) when is_list(A) ->
    get_if_valid(lists:last(A), Default);
get_nth(Index, A, Default) when Index > 0, Index =< length(A) ->
    get_if_valid(lists:nth(Index, A), Default);
get_nth(_Index, _A, Default) -> Default.


-spec get_if_valid(Element :: json_term(),
                   Default :: term()) -> json_term() | term().
%%------------------------------------------------------------------------------
%% @private helper function for get_nth/3. given an element of an array, if it
%% is an element that can be an array member, return it. Otherwise, return the
%% Default.
%%------------------------------------------------------------------------------
get_if_valid(T, _Default) when ?IS_SIMPLE_JSON_TERM(T) -> T;
get_if_valid(M, _Default) when is_map(M) -> M;
get_if_valid(P, _Default) when is_list(P) -> P;
get_if_valid({P} = S, _Default) when is_list(P) -> S;
get_if_valid({struct, P} = S, _Default) when is_list(P) -> S;
get_if_valid(_, Default) -> Default.


-spec path_equal(Path :: path(),
                 Original :: json_object() | json_array(),
                 Other :: json_object() | json_array(),
                 Mode :: soft | hard) -> ok | {error, BinPath::binary()}.
%%------------------------------------------------------------------------------
%% @private given a field and two json_objects (original and other), return ok
%% if the field has an equivalent value in both json_objects; if not, return
%% `{error, Path}'' with the binary string form of the field
%%
%% if mode is soft, a mismatch is allowed if the value is missing from
%% OtherObject.
%%------------------------------------------------------------------------------
path_equal(Path, OriginalObject, OtherObject, soft) ->
    case get(Path, OtherObject) of
        undefined -> ok;
        _ -> path_equal(Path, OriginalObject, OtherObject, hard)
    end;
path_equal(Path, OriginalObject, OtherObject, _HardMode) ->
    case get(Path, OtherObject) == get(Path, OriginalObject) of
        true -> ok;
        false -> {error, path_to_binary(Path)}
    end.


-spec object_equal
     (Paths :: paths(),
      Original :: json_object() | json_array(),
      Other :: json_object() | json_array(),
      Mode :: soft | hard) -> ok | {error, {not_equal, Message :: binary()}}.
%%------------------------------------------------------------------------------
%% @private given a list of fields, an original json_object, and another 
%% json_object, verify that each path in each of the other object has the same
%% value as the original does at the same path, for each path in the list of 
%% paths; if so, return ok; otherwise, return an error tuple with the error
%% type and a summary of mismatches for the first mismatched object
%%
%% if mode is soft, a mismatch is allowed if the value is missing from the new
%% object.
%%------------------------------------------------------------------------------
object_equal(Paths, OriginalObject, OtherObject, Mode) ->
    Errors = lists:foldl(fun(Path, MismatchedPaths) ->
        case path_equal(Path, OriginalObject, OtherObject, Mode) of
            ok -> MismatchedPaths;
            {error, MismatchedPath} -> [MismatchedPath|MismatchedPaths]
        end
    end, [], Paths),
    case Errors of
        [] -> ok;
        _ ->
            MismatchedPaths = binary_join(lists:reverse(Errors), <<", ">>),
            ErrMsg = <<"mismatch of requested and existing field(s): ", 
                       MismatchedPaths/binary>>,
            {error, {not_equal, ErrMsg}}
    end.


-spec compare_maps
    (Keys :: [json_key()],
     ComparisonFun :: fun((maps:map(), maps:map()) -> boolean()),
     A :: maps:map(),
     B :: maps:map()) ->
    boolean().
%%------------------------------------------------------------------------------
%% @private compare the values of 2 Erlang maps for the given keys using the
%% given comparison function
%%------------------------------------------------------------------------------
compare_maps([], _Fun, _A, _B) ->
    true;
compare_maps([Key | Keys], CompareFun, A, B) ->
    maps:is_key(Key, B) andalso
        CompareFun(maps:get(Key, A), maps:get(Key, B)) andalso
            compare_maps(Keys, CompareFun, A, B).


-spec safe_binary_to_atom(binary()) -> atom() | binary().
%%------------------------------------------------------------------------------
%% @private try to convert a binary to an atom, if the atom exists; otherwise,
%% return the existing atom
%%------------------------------------------------------------------------------
safe_binary_to_atom(Binary) when is_binary(Binary) ->
    try
        binary_to_existing_atom(Binary, utf8)
    catch
        error:badarg -> Binary
    end.


-spec to_binary(atom() | binary()) -> binary().
%%------------------------------------------------------------------------------
%% @private convert the input to binary
%%------------------------------------------------------------------------------
to_binary(B) when is_binary(B) -> B;
to_binary(A) when is_atom(A)   -> atom_to_binary(A, utf8);
to_binary(I) when is_integer(I)   -> integer_to_binary(I).


-spec path_to_binary(path()) -> binary().
%%------------------------------------------------------------------------------
%% @private convert the path to binary
%%------------------------------------------------------------------------------
path_to_binary(Path) when is_binary(Path); is_atom(Path) -> to_binary(Path);
path_to_binary(T) when is_tuple(T) -> path_to_binary(tuple_to_list(T)); 
path_to_binary([]) -> <<>>;
path_to_binary([H|T]) -> path_to_binary(T, H).

path_to_binary([], Acc) -> Acc;
path_to_binary([Key0|Rest], Acc) -> 
    Key = to_binary(Key0),
    path_to_binary(Rest, <<Acc/binary, ".", Key/binary>>).


-spec binary_join([binary()], Sep :: binary()) -> binary().
%%------------------------------------------------------------------------------
%% @private join the list of binary strings together using the separator
%%------------------------------------------------------------------------------
binary_join([], _Sep) ->
    <<>>;
binary_join([H|T], Sep) ->
    binary_join(T, Sep, H).

binary_join([], _Sep, Acc) -> Acc;
binary_join([H|T], Sep, Acc) when is_binary(H) -> 
    binary_join(T, Sep, <<Acc/binary, Sep/binary, H/binary>>).
