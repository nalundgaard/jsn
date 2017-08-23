%%------------------------------------------------------------------------------
%% jsn - Functions for interacting with decoded JSON objects 
%%
%% @author Nicholas Lundgaard <nlundgaard@alertlogic.com>
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
    set/3, set_list/2,
    delete/2, delete_list/2,
    delete_if_equal/3,
    copy/3, copy/4,
    transform/2,
    path_transform/2,
    path_elements/1
]).
%% sort functions
-export([sort/1, sort_keys/1]).
%% comparison functions
-export([equal/3, equal/4]).
-export([sort_equal/2]).
-export([is_equal/2]).
-export([is_subset/2]).
%% DEPRECATED: JSON encode/decode
-export([encode/1, decode/1, decode/2]).
%% object format conversion
-export([as_proplist/1, from_proplist/1, from_proplist/2]).

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


-define(DEFAULT_FORMAT, proplist).

%% guard for matching a JSON string, boolean, number, null
-define(IS_SIMPLE_JSON_TERM(X), 
    is_binary(X);
    is_boolean(X);
    is_number(X); 
    X =:= null ).


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
copy(_PathList, _Src, _Dst, _Mutator) ->
    erlang:error(badarg).


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
        Transforms
    ).


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
                Transforms).


-spec path_elements(path()) -> path_elements().
%%------------------------------------------------------------------------------
%% @doc given a path, parse it into an ordered list of json keys (binary) and/or 
%% array indexes
%%------------------------------------------------------------------------------
path_elements(Path) when is_binary(Path); is_atom(Path)  ->
    binary:split(to_binary(Path), <<".">>, [global]);
path_elements(Path) when is_list(Path) ->
    path_elements(list, lists:reverse(Path), []);
path_elements(Path) when is_tuple(Path) ->
    path_elements(tuple, lists:reverse(tuple_to_list(Path)), []);
path_elements(_Path) -> erlang:error(badarg).


path_elements(_Type, [], Acc) ->
    Acc;
path_elements(Type, [Key | Rest], Acc) when is_binary(Key) ->
    path_elements(Type, Rest, [Key | Acc]);
path_elements(list, [Key | Rest], Acc) when is_atom(Key)  ->
    path_elements(list, Rest, [to_binary(Key) | Acc]);
path_elements(tuple, [Index | Rest], Acc) when is_integer(Index), Index > 0 ->
    path_elements(tuple, Rest, [Index | Acc]);
path_elements(tuple, [Index | Rest], Acc) when Index =:= first; Index =:= last ->
    path_elements(tuple, Rest, [Index | Acc]);
path_elements(_Type, _Path, _Acc) -> erlang:error(badarg).


%%==============================================================================
%% sort API
%%==============================================================================


-spec sort(json_term()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc given a json term, recursively sort the keys and arrays in the object,
%% if they are present; note that this will sort arrays in the object, not just
%% the keys
%%------------------------------------------------------------------------------
sort(L) when is_list(L) ->
    lists:sort([sort(V) || V <- L]);
sort({K, V}) ->
    {K, sort(V)}; %% Handles mochijson {'struct', V} object format as well.
sort({V}) ->
    {sort(V)};
sort(V) ->
    V.


-spec sort_keys(json_term()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc given a json term or json proplist term, recursively sort keys in 
%% all objects at all levels
%%------------------------------------------------------------------------------

sort_keys({struct, V}) ->
    {struct, sort_keys(V)};
sort_keys({V}) ->
    {sort_keys(V)};
sort_keys([{K, _V}|_] = P) when is_binary(K); is_atom(K), K =/= struct ->
    lists:sort([{Key, sort_keys(Value)} || {Key, Value} <- P]);
sort_keys(L) when is_list(L) ->
    [sort_keys(V) || V <- L];
sort_keys(V) when ?IS_SIMPLE_JSON_TERM(V) ->
    V.


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


-spec sort_equal(json_term(), json_term()) -> boolean().
%%------------------------------------------------------------------------------
%% @doc given two json terms, return true if they are equal after sorting
%% all contained lists (including, but not limited to, proplists)
%% 
%% unlike equal/3,4 this function can be used to check equality of large complex
%% objects, but only when order of list items doesn't matter; note that this 
%% only works if both terms are in the same format: e.g., if one is a proplist
%% and the other is eep18, this will always return false.
%%------------------------------------------------------------------------------
sort_equal(A, B) ->
    sort(A) =:= sort(B).


-spec is_equal(json_term(), json_term()) -> boolean().
%%------------------------------------------------------------------------------
%% @doc given 2 json terms A and B in any format (eep18, struct, proplist),
%% return true if they are equivalent
%%------------------------------------------------------------------------------
is_equal({A}, B) when is_list(A) ->
    is_equal(A, B);
is_equal({struct, A}, B) when is_list(A) ->
    is_equal(A, B);
is_equal(A, {B}) when is_list(B) ->
    is_equal(A, B);
is_equal(A, {struct, B}) when is_list(B) ->
    is_equal(A, B);
is_equal([{K0, V}|T] = _A, B) when is_binary(K0); is_atom(K0), K0 =/= struct ->
    K = to_binary(K0),
    is_equal(V, key_get(K, B)) andalso is_equal(T, keys_set([K], B, jsn__delete));
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
    is_subset(A, B);
is_subset({struct, A}, B) when is_list(A) ->
    is_subset(A, B);
is_subset(A, {B}) when is_list(B) ->
    is_subset(A, B);
is_subset(A, {struct, B}) when is_list(B) ->
    is_subset(A, B);
is_subset([] = _A, B) when is_list(B) ->
    true;
is_subset([{K, V}|T] = _A, B) when is_binary(K); is_atom(K), K =/= struct ->
    is_subset(V, key_get(K, B)) andalso is_subset(T, B);
is_subset([H|T] = _A, B) when is_list(B)  ->
    lists:any(fun(X) -> is_subset(H, X) end, B) andalso is_subset(T, B);
is_subset(A, A) when ?IS_SIMPLE_JSON_TERM(A) ->
    true;
is_subset(_A, _B) ->
    false.


%%==============================================================================
%% JSON encode/decode
%%==============================================================================


-spec encode(json_term()) -> Json :: binary().
%%------------------------------------------------------------------------------
%% @doc encode a json_term into a JSON binary string
%% @deprecated Please use jiffy, jsone, or jsx instead.
%%------------------------------------------------------------------------------
encode(Object) ->
    try jsonx:encode(Object) of
        Json when is_binary(Json) -> Json;
        Error -> throw({error, Error})
    catch
        _:Reason3 ->  throw({error, Reason3})
    end.


-spec decode(Json :: iolist()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc decode a JSON string into a json_term
%% @deprecated Please use jiffy, jsone, or jsx instead.
%%------------------------------------------------------------------------------
decode(Json) ->
    decode(Json, []).


-spec decode(Json :: iolist(), jsn_options()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc decode a JSON string into a jsn object using the given options
%% @deprecated Please use jiffy, jsone, or jsx instead.
%%------------------------------------------------------------------------------
decode(Json, Options) when is_list(Json) ->
    decode(list_to_binary(Json), Options);
decode(Json, Options) when is_binary(Json) ->
    try jsonx:decode(Json, [{format, get_format(Options)}]) of
        {error, Reason, Location} -> throw({error, {Reason, Location}});
        {error, Reason2}          -> throw({error, Reason2});
        Object -> Object
    catch
        _:Reason3 ->  throw({error, Reason3})
    end;
decode(Json, _Options) ->
    throw({error, {invalid_input, Json}}).


%%==============================================================================
%% conversion API
%%==============================================================================


-spec as_proplist(json_term()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc convert a jsn object (or list of them) into a proplist
%%------------------------------------------------------------------------------
as_proplist({struct, List}) when is_list(List) ->
    as_proplist(List);
as_proplist({List}) when is_list(List) ->
    as_proplist(List);
as_proplist([{struct, H}|T]) when is_list(H) ->
    [as_proplist(H) | as_proplist(T)];
as_proplist([{H}|T]) when is_list(H) ->
    [as_proplist(H) | as_proplist(T)];
as_proplist([{Key, Value}|T]) ->
    [{to_binary(Key), as_proplist(Value)} | as_proplist(T)];
as_proplist(List) when is_list(List) ->
     [as_proplist(Value) || Value <- List];
as_proplist(Value) -> Value.


-spec from_proplist(json_term()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc convert a json_proplist (that may have atom keys) into a json_proplist
%% with only binary keys
%%------------------------------------------------------------------------------
from_proplist(Object) ->
    from_proplist(Object, []).


-spec from_proplist(json_term(), jsn_options()) -> json_term().
%%------------------------------------------------------------------------------
%% @doc convert a (maybe nested) proplist that is JSON-compliant (e.g.,
%% from erlson decoding) into json_object using the given options
%%------------------------------------------------------------------------------
from_proplist([{_,_}|_] = P0, Options) ->
    P = [{to_binary(Key), from_proplist(Value, Options)} || {Key, Value} <- P0],
    case get_format(Options) of
        proplist -> P;
        struct -> {struct, P};
        eep18 -> {P}
    end;
from_proplist(List, Options) when is_list(List) ->
    [from_proplist(Value, Options) || Value <- List];
from_proplist(X, _Options) -> X.


%%==============================================================================
%% internal functions
%%==============================================================================


-spec get_format(jsn_options()) -> format().
%%------------------------------------------------------------------------------
%% @private given jsn options, get the selected format
%%------------------------------------------------------------------------------
get_format(Options) ->
    case lists:keyfind(format, 1, Options) of
        false         -> ?DEFAULT_FORMAT;
        {_, proplist} -> proplist;
        {_, eep18}    -> eep18;
        {_, struct}   -> struct;
        _ -> erlang:error(badarg)
    end.


-spec empty_object(jsn_options()) -> json_object().
%%------------------------------------------------------------------------------
%% @private create a new json_object in the specified format
%%------------------------------------------------------------------------------
empty_object(Options) ->
    case get_format(Options) of
        proplist -> ?EMPTY_PROPLIST;
        eep18 -> ?EMPTY_EEP18;
        struct -> ?EMPTY_STRUCT 
    end.


-spec keys_set(path_elements(),
               Object :: json_object() | json_array(), 
               Value :: json_term() | jsn__delete) ->
              json_object() | json_array().
%%------------------------------------------------------------------------------
%% @private given a path as list of binary json_keys, a json_object, and a
%% value, upsate the object at the location defined by the path to the given
%% to be value, and return the updated object
%%------------------------------------------------------------------------------
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
keys_set([Key | Rest], P, Value, Empty) when is_binary(Key), is_list(P) ->
    case key_get(Key, P, jsn__undefined) of
        E when E =:= jsn__undefined; E =:= Empty ->
            key_set(Key, P, keys_set(Rest, Empty, Value, Empty)); 
        SubValue ->
            key_set(Key, P, keys_set(Rest, SubValue, Value, Empty))
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
set_nth(_Index, [{_,_}|_] = P, _V) ->
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
              json_term()) -> json_term() | undefined.
%%------------------------------------------------------------------------------
%% @private identical to `key_get/3', with a default of `undefined'.
%%------------------------------------------------------------------------------
key_get(Key, Object) ->
    key_get(Key, Object, undefined).


-spec key_get(path_element(),
              json_term(),
              Default :: term()) -> json_term() | term().
%%------------------------------------------------------------------------------
%% @private get the value of a key from a json object or json proplist object,
%% or return undefined; this is function does not support nested keys (i.e., 
%% paths), only single, flat keys
%%------------------------------------------------------------------------------
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
get_nth(first, [H|_], _Default) when ?IS_SIMPLE_JSON_TERM(H) ->
    H;
get_nth(last, [H|_] = A, _Default) when ?IS_SIMPLE_JSON_TERM(H) ->
    hd(lists:reverse(A));
get_nth(Index, A, _Default) when Index > 0, Index =< length(A) ->
    lists:nth(Index, A);
get_nth(_Index, _A, Default) -> Default. 


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
