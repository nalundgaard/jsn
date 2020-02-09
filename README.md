# jsn

[![Build Status](https://secure.travis-ci.org/nalundgaard/jsn.png?branch=master)](http://travis-ci.org/nalundgaard/jsn)

jsn is a tool for working with JSON representations in erlang--complex, nested
JSON objects in particular.

In the spirit of [ej][ej], it supports the common formats output by JSON
decoders such as [jsone][jsone], [jiffy][jiffy], and [mochijson2][mochijson2].
Unlike [ej][ej], however, it supports _all four_ common JSON representations
in Erlang:

* `map` (**default**)(common to [jsone][jsone], [jiffy][jiffy], and [jsx][jsx])
* `proplist` (used by [jsonx][jsonx])
* `eep18` (common to [jiffy][jiffy], [jsone][jsone], and [jsonx][jsonx])
* `struct` (common to [mochijson2][mochijson2])

In addition to supporting the additional `proplist` and `map` formats, jsn's
path input structure is somewhat more flexible, allowing for input of
period-delimited binary strings or atoms to indicate a path through a
deeply nested structure. This support is similar to [kvc][kvc]'s path format,
and also likely to be familiar to users of [erlson][erlson].

This code base was originally developed as a wrapper around [ej][ej], adding
support for the 'syntactic sugar' of the period-delimited keys. However, a
need arose for the library to be proplist-compatible, then map-compatible, so
it has been refactored to be a nearly standalone library.

## Caveats & known issues

### Proplist format concerns

It should be noted that the `proplist` format supported by jsn is compatible with
the [abandoned](#encode-decode) [jsonx][jsonx] library, and is not compatible
with the `proplist` format used in [jsx][jsx] and [jsone][jsone]. Specifically,
jsn uses the empty list (`[]`) like [jsonx][jsonx] to represent an empty object,
whereas [jsx][jsx] and [jsone][jsone] use an empty tuple in a list (`[{}]`) to
represent an empty object.

[jsx][jsx] and [jsone][jsone] use this representation to disambiguate empty JSON
objects from empty arrays, which cannot be distinguished in the [jsonx][jsonx]
`proplist` format used by jsn. jsn is incompatible with this format. While the
getter (`jsn:get/2,3`) functions are generally functional, most other library
functions are not, and may result in unpredictable behaviors.

jsn does not plan to support a [jsx][jsx] and [jsone][jsone] compatible
`proplist` format; long-term, clients are strongly encouraged to use the `map`
format instead. It a vastly more performant data structure that maps naturally
to JSON objects without ambiguity.

## Roadmap

Future improvements to this library are TBD at this time.

## Changelog

9 February 2020 - 2.1.4

* Fix nested key deletion ([#34](https://github.com/nalundgaard/jsn/pull/34))

19 August 2019 - 2.1.3

* Refactor utility functions to use maps as the native format ([#32](https://github.com/nalundgaard/jsn/pull/32))

18 August 2019 - 2.1.2

* Remove unneeded rebar.config.script ([#31](https://github.com/nalundgaard/jsn/pull/31))

27 February 2018 - 2.1.1

* Resolved #24, jsn:new fails while making an array of objects in struct ( resolved by PR #25)
* Improved jsn:get_nth behavior for arrays of objects (#26)

6 November 2017 - 2.1.0

* Implement `from_map/1,2` and `as_map/1` functions; these work similarly to the from/as_proplist suite.
* Improve `from_proplist/1,2` converter:
  *  Only extract the format from `Opts` input one time instead of N times.
  * Stop converting keys to binary: This makes the function work more similarly to from/as_map, which doesn’t transform keys (allowing us to modify maps using `maps:map/2` instead of `maps:fold/3`)
* Improve guards and error outputs on unexpected inputs in a number of  functions throughout the library: more consistent `badarg` errors instead of `case_clause`, `function_clause`, etc.
* Add README examples of conversion functions
* Fix README examples overlooked in change to map as the default format.

6 October 2017 - 2.0.0

* New default format: map
* Remove compatibility with Erlang 17 and below.
* Remove encode/decode functionality (no more jsonx dependency)
* Remove key sorting functions

3 October 2017 - 1.2.1

* The `equal/3,4` functions were missing maps support due to a missing function clause.
* Add `select/2,3` function documentation to README

26 September 2017 - 1.2.0

* fix rebar 2.x compatibility (#11)
* adds select(#13)
* improved errors (#14)

24 August 2017 - 1.1.0

* Introduce support for new map format
* Deprecate Erlang 17 and lower
* Deprecate jsn:sort/1, jsn:sort_keys/1, and jsn:sort_equal/2

23 August 2017 - 1.0.3

* Add hex metadata to .app.src
* Deprecate encode/decode
* Include jsonx as OTP application dependency for rebar3 release compatibility

22 August 2017 - 1.0.2

* Adds Erlang 20 support:
  * Adds support for building with Erlang 20, with compiler warnings addressed (primarily in test module)
  * Relaxes `require_otp_vsn` so that it will not explicitly exclude future Erlang versions

6 November 2016 - 1.0.1

* Adds Erlang 19 support:
  * Spec format fixes, primarily, as well as a few spec clarifications.

5 February 2016 - 1.0.0

* Initial release of the jsn library


## Running 

To run this library locally, build the source code:

```sh
make
```

Then start an erlang shell:

```sh
make start
```

jsn is an OTP library, and does not really need to be started as such.

## Application integration

`jsn` has been published to [hex.pm](https://hex.pm/packages/jsn); to
add `jsn` to your Erlang OTP application, simply add it to your
`rebar.config`:

```erlang
{deps, [
    %% ... your deps ...
    jsn
]}.
```

and your applications `src/<application>.app.src` or `ebin/<application>.app`
file:

```erlang
{application, <application>, [
    {description, "An application that uses nested JSON interaction"},
    {applications, [
        kernel,
        stdlib,
        %% ... your deps ...
        jsn
    ]}
]}
```

After you re-compile, you will have full access to jsn from your local console.

## Paths & indexes

Paths are pointers into a (potentially nested) jsn object. An object may contain
sub-objects or arrays at any layer, and as such, a path may include both keys
(as binary strings and sometimes atoms) as well as array indexes. Indexes can be 
provided as positive integers (i.e., `1` is the first element of an array) or as
the shortcut atoms `'first'` and `'last'`.

There are 3 different supported path styles, each with different tradeoffs:

1. List of binary/atom keys, e.g. `[<<"person">>, 'id']`.
   Mixing and matching atom and binary string keys is supported, but using
   binary keys only is most performant (and matches the 'native' path format).
   However, this format **does not** support array indexes.
2. Tuple of binary keys and array indexes, e.g., `{<<"users">>, last}`.
   Atom keys are not supported due to potential ambiguity. This is the
   only possible path format to use if you want to leverage the array
   index feature.
3. Period-delimited atom or binary string, e.g. `<<"user.id">>` or `'user.id'`.
   This format is the most compact and readable, but only supports keys
   (no array indexes).

## Library functions

jsn provides functions to create, append, delete, and transform objects in all
supported formats (`map`, `proplist`, `eep18`, and `struct`). This section
contains a reference for the primary library functions available.

### `new/0,1,2` - Create a new object

* `new()` - Create an empty object in the default format.
* `new(TupleOrTupleList)` - Given a `{Path, Value}` tuple or a list of such
  tuples, return a new object (in the default format) containing the given
  path(s) and value(s).
* `new(TupleOrTupleList, Options)` - Identical to new/1, but with the addition
  of Options, which allow passing a specific format.

#### Examples

```erlang
% create an empty object
jsn:new().
% #{}

% create an object using a single path, value pair.
jsn:new({'user.id', <<"123">>}).
% #{<<"user">> => #{<<"id">> => <<"123">>}}

% create an object using a list of path, value pairs.
jsn:new([{'user.id', <<"123">>}, {<<"user.name">>, <<"John">>}]).
% #{<<"user">> => #{<<"id">> => <<"123">>,
%                   <<"name">> => <<"John">>}}

% create a jsn object in proplist format
jsn:new([{'user.id', <<"123">>},
         {<<"user.name">>, <<"John">>}], [{format, proplist}]).
% [{<<"user">>, [{<<"id">>,<<"123">>},
%                {<<"name">>,<<"John">>}]}]

% create a jsn object in eep18 format
jsn:new([{'user.id', <<"123">>},
         {<<"user.name">>, <<"John">>}], [{format, eep18}]).
% {[{<<"user">>, {[{<<"id">>,<<"123">>},
%                  {<<"name">>,<<"John">>}]}}]}

% create a jsn object in struct (mochijson2) format
jsn:new([{'user.id', <<"123">>},
         {<<"user.name">>, <<"John">>}], [{format, struct}]).
% {struct, [{<<"user">>, {struct, [{<<"id">>,<<"123">>},
%                                  {<<"name">>,<<"John">>}]}}]}
```

### `get/2,3`, `get_list/2,3`, `find/3,4`, and `select/2,3` - Extract data from objects

* `get(Path, Object)` - Return the value at Path in the Object, or `undefined`
  if it is missing.
* `get(Path, Object, Default)` - Identical to `get/2`, but returns `Default`
  instead of undefined.
* `get_list(PathList, Object)` - Identical to `get/2`, but expects a list of
  Paths, and returns a corresponding list of values.
* `get_list(PathList, Object, Default)` - Identical to `get/3`, but expects a
  list of Paths, and returns a corresponding list of values.
* `find(Path, SearchTerm, Objects)` - Find all the elements of the Objects list
  where the given Path in the element matches the SearchTerm, and return them.
* `find(Path, SubPath, SearchTerm, Object)` - Get the Path from the given
  Object (should be a list of objects), and find all the elements in the list
  where the given SubPath in the element matches SearchTerm, and return them.
* `select(Selection, Objects)` - Apply the given selection(s) to the `Objects`
  list, returning a list of selection results correspondent to the given list.
  See [below](#selections-conditions) for more information about `Selections`.
* `select(Selection, Conditions, Objects)` - Apply the given condition(s) and
  selection(s) to the `Objects` list, returning a list of selection results
  correspondent to the given list, with elements that do not meet the given
  conditions filtered out. See [below](#selections-conditions) for more
  information about `Selections` and `Conditions`.

#### <a name="selections-conditions"/>Selections and Conditions in `select/2,3`

The functions `select/2` and `select/3` accept selection and conditional
specifications defined in [jsn.hrl](include/jsn.hrl).

`Selections` can be passed singularly or as a list in `select/2` and
`select/3`. If it is passed as a list of selections, the output result from
the `select` call will contain a symmetrically ordered list of results for
each element in the input list. A `Selection` is one of the following:

* `{value, Path}` - select the value at the given path, or `undefined` if
  it is missing
* `{value, Path, Default}` - select the value at the given path, or `Default`
  if it is missing
* `identity` - select the whole object

`Conditions` can be passed singularly or as a list in `select/3`. A `Conditioon`
is one of the following:

* {Path, Value} - include an element if the value at the given `Path` in the
  element is equivalent to the given `Value`.
* `{Path, fun((Value) -> Boolean)}` - include an element if the given function
  returns `true` with the input of the value at the given `Path`
* `fun((Element) -> Boolean)` - include an element if the given function
  returns `true` with the input of the whole element.

See [below](#extract-examples) for examples.

#### <a name="extract-examples"/>Examples

```erlang
User = jsn:new([{'user.id', <<"123">>},
                {'user.activated', true},
                {'user.name.first', <<"Jane">>},
                {'user.name.last', <<"Doe">>}]).
% #{<<"user">> => #{<<"activated">> => true,
%                   <<"id">> => <<"123">>,
%                   <<"name">> => #{<<"first">> => <<"Jane">>,
%                                   <<"last">> => <<"Doe">>}}}

% get the user id
UserId = jsn:get('user.id', User).
% <<"123">>

% get a non-existant field, with and without a custom default
jsn:get(<<"user.deleted">>, User).
% undefined
jsn:get([<<"user">>, <<"deleted">>], User, false).
% false

% get several fields in a single call:
jsn:get_list(['user.name.first', 'user.name.last', 'user.name.middle'], User).
% [<<"Jane">>,<<"Doe">>,undefined]
jsn:get_list(['user.activated', 'user.deleted'], User, false).
% [true,false]

User2 = jsn:new([{'user.id', <<"456">>},
                 {'user.name.first', <<"Eve">>},
                 {'user.name.middle', <<"L.">>},
                 {'user.name.last', <<"Doer">>}]).
% #{<<"user">> => #{<<"id">> => <<"456">>,
%                   <<"name">> => #{<<"first">> => <<"Eve">>,
%                                    <<"last">> => <<"Doer">>,
%                                    <<"middle">> => <<"L.">>}}}

% find the first user by id:
[User] = jsn:find({<<"user">>, <<"id">>}, <<"123">>, [User, User2]).
% [#{<<"user">> => #{<<"activated">> => true,
%                    <<"id">> => <<"123">>,
%                    <<"name">> => #{<<"first">> => <<"Jane">>,
%                                    <<"last">> => <<"Doe">>}}}]

% select the first name from the users:
jsn:select({value, <<"user.name.first">>}, [User, User2]).
% [<<"Jane">>,<<"Eve">>]

% select the user id and whole object from the users:
jsn:select([{value, [<<"user">>, <<"id">>]}, identity], [User, User2]).
% [[<<"123">>, #{<<"user">> =>
%                    #{<<"activated">> => true,
%                      <<"id">> => <<"123">>,
%                      <<"name">> => #{<<"first">> => <<"Jane">>,
%                                      <<"last">> => <<"Doe">>}}}],
%  [<<"456">>, #{<<"user">> =>
%                    #{<<"id">> => <<"456">>,
%                      <<"name">> => #{<<"first">> => <<"Eve">>,
%                                      <<"last">> => <<"Doer">>,
%                                      <<"middle">> => <<"L.">>}}}]]

% select the user id and first name from the users whose last name is <<"Doe">>:
jsn:select([{value, [<<"user">>, <<"id">>]},
            {value, [<<"user">>, <<"name">>, <<"first">>]}],
           {<<"user.name.last">>, <<"Doe">>},
           [User, User2]).
% [[<<"123">>,<<"Jane">>]]

% select the user id from the users whose middle name is missing:
jsn:select({value, [<<"user">>, <<"id">>]},
           {<<"user.name.middle">>, fun(undefined) -> true; (_) -> false end},
           [User, User2]).
% [<<"123">>]

% select the user id from the users whose whose first name is < 4 characters
% and whose last name is > 3 characters
ConditionFun = fun(Object) ->
                   [First, Last] = jsn:get_list([<<"user.name.first">>,
                                                 <<"user.name.last">>],
                                                Object),
                   (byte_size(First) < 4) andalso (byte_size(Last) > 3)
                end.
jsn:select({value, [<<"user">>, <<"id">>]},
           [ConditionFun],
           [User, User2]).
% [<<"456">>]
```

### `set/3` and `set_list/2` - Add to and update existing objects

* `set(Path, Object, Value)` - Append (or update) the Object by setting Path
  to Value, and return the result.
* `set_list(TupleList, Object)` - Given a list of `{Path, Value}` tuples, apply
  the `set/3` function to the each Path and Value using the given object, and
  return the result.

#### Examples

```erlang
User = jsn:new([{'user.id', <<"123">>},
                {'user.activated', true},
                {'user.name.first', <<"Jane">>},
                {'user.name.last', <<"Doe">>}]).
% #{<<"user">> => #{<<"activated">> => true,
%                   <<"id">> => <<"123">>,
%                   <<"name">> => #{<<"first">> => <<"Jane">>,
%                                   <<"last">> => <<"Doe">>}}}

% Set Jane's middle name
jsn:set([<<"user">>, <<"name">>, <<"middle">>], User, <<"Jacqueline">>).
% #{<<"user">> => #{<<"activated">> => true,
%                   <<"id">> => <<"123">>,
%                   <<"name">> => #{<<"first">> => <<"Jane">>,
%                                   <<"last">> => <<"Doe">>,
%                                   <<"middle">> => <<"Jacqueline">>}}}

% Deactivate Jane's User, and change her middle name
jsn:set_list([{'user.activated', false},
              {'user.name.middle', <<"Jay">>}], User).
% #{<<"user">> => #{<<"activated">> => false,
%                   <<"id">> => <<"123">>,
%                   <<"name">> => #{<<"first">> => <<"Jane">>,
%                                   <<"last">> => <<"Doe">>,
%                                   <<"middle">> => <<"Jay">>}}}
```

### `delete/2`, `delete_list/2`, and `delete_if_equal/2` - Remove data from existing objects

* `delete(Path, Object)` - Given a Path, remove it from the Object, if it
  exists, and return the result.
* `delete_list(PathList, Object)` - Given a list of paths, apply `delete/2` to
  Object and return the result.
* `delete_if_equal(Paths, ValueOrValues, Object)` - Given a list of paths and a
  Value or list of Values, check each path Value/Values, and if equal, remove the
  matching Path, Value pair from the object, and return the result.

#### Examples

```erlang
Company = jsn:new([{'company.name', <<"Foobar, Inc.">>},
                   {'company.created.by', <<"00000000">>},
                   {'company.created.at', 469778436},
                   {'company.location', <<"U.S. Virgin Islands">>},
                   {{<<"company">>, <<"employees">>}, []},
                   {{<<"company">>, <<"employees">>, 1, <<"id">>}, <<"00000000">>},
                   {{<<"company">>, <<"employees">>, 1, <<"name">>}, <<"Alice">>},
                   {{<<"company">>, <<"employees">>, 1, <<"position">>}, <<"CEO">>},
                   {{<<"company">>, <<"employees">>, 2, <<"id">>}, <<"00000001">>},
                   {{<<"company">>, <<"employees">>, 2, <<"name">>}, <<"Bob">>},
                   {{<<"company">>, <<"employees">>, 2, <<"position">>}, <<"CTO">>}]).
% #{<<"company">> =>
%       #{<<"created">> => #{<<"at">> => 469778436,
%                            <<"by">> => <<"00000000">>},
%         <<"employees">> => [#{<<"id">> => <<"00000000">>,
%                               <<"name">> => <<"Alice">>,
%                               <<"position">> => <<"CEO">>},
%                             #{<<"id">> => <<"00000001">>,
%                               <<"name">> => <<"Bob">>,
%                               <<"position">> => <<"CTO">>}],
%         <<"location">> => <<"U.S. Virgin Islands">>,
%         <<"name">> => <<"Foobar, Inc.">>}}

% remove the location from Company
jsn:delete('company.location', Company).
% #{<<"company">> =>
%       #{<<"created">> => #{<<"at">> => 469778436,
%                            <<"by">> => <<"00000000">>},
%         <<"employees">> => [#{<<"id">> => <<"00000000">>,
%                               <<"name">> => <<"Alice">>,
%                               <<"position">> => <<"CEO">>},
%                             #{<<"id">> => <<"00000001">>,
%                               <<"name">> => <<"Bob">>,
%                               <<"position">> => <<"CTO">>}],
%         <<"name">> => <<"Foobar, Inc.">>}}

% delete Bob and the location in one call
jsn:delete_list(['company.location', {<<"company">>, <<"employees">>, last}], Company).
% #{<<"company">> =>
%       #{<<"created">> => #{<<"at">> => 469778436,
%                            <<"by">> => <<"00000000">>},
%         <<"employees">> => [#{<<"id">> => <<"00000000">>,
%                               <<"name">> => <<"Alice">>,
%                               <<"position">> => <<"CEO">>}],
%         <<"name">> => <<"Foobar, Inc.">>}}

% conditionally delete the company Location
SecretLocations = [<<"Nevada">>, <<"Luxembourg">>, <<"U.S. Virgin Islands">>].
jsn:delete_if_equal('company.location', SecretLocations, Company).
% #{<<"company">> =>
%       #{<<"created">> => #{<<"at">> => 469778436,
%                            <<"by">> => <<"00000000">>},
%         <<"employees">> => [#{<<"id">> => <<"00000000">>,
%                               <<"name">> => <<"Alice">>,
%                               <<"position">> => <<"CEO">>},
%                             #{<<"id">> => <<"00000001">>,
%                               <<"name">> => <<"Bob">>,
%                               <<"position">> => <<"CTO">>}],
%         <<"name">> => <<"Foobar, Inc.">>}}
```

### `copy/3,4` and `transform/2` - Re-shaping existing objects

* `copy(PathList, Src, Dst)` - Given a list of paths, a source object, and one
  or more destination objects, copy the paths and values from Src to the
  destination(s). This function always returns a list of objects.
* `copy(PathList, Src, Dst, Mutator)` - Like above, but pass the value through
  a function/1 which mutates the set value.
* `transform(Transforms, Object)` - Given a list of `{Path, fun/1}` tuples,
  apply the fun to the value at path and modify the given object. Return the
  new object.

#### Examples

```erlang
Source = jsn:new([{'key1', <<"value1">>},
                  {'key2', <<"value2">>},
                  {'key3', <<"value3">>},
                  {'key3', <<"value3">>}]).
% #{<<"key1">> => <<"value1">>,
%   <<"key2">> => <<"value2">>,
%   <<"key3">> => <<"value3">>}

Destination = jsn:new({'key4', <<"value4">>}).
% #{<<"key4">> => <<"value4">>}

% copy some of the paths from source to destination
[NewDestination] = jsn:copy(['key1', 'key2'], Source, Destination).
% [#{<<"key1">> => <<"value1">>,
%    <<"key2">> => <<"value2">>,
%    <<"key4">> => <<"value4">>}]

T1 = fun(<<"value", N/binary>>) -> N end.

% transform all the keys of NewDestination
jsn:transform([{key1, T1},{key2, T1},{key4, T1}], NewDestination).
% #{<<"key1">> => <<"1">>,
%   <<"key2">> => <<"2">>,
%   <<"key4">> => <<"4">>}
```

### `equal/3,4` - Path-wise object comparison

* `equal(Paths, OriginalObject, OtherObjectOrObjects)` - Given a list of paths,
  an Original object, and a single or list of objects, verify that each path
  in each of the other object(s) has the same value as the original does at the
  same path, for each path in the list of paths. If so, return ok; otherwise,
  return an error tuple with the error type and a summary of mismatches for the
  first mismatched object.
* `equal(Paths, OriginalObject, OtherObjectOrObjects, Mode)` - Identical to
  `equal/3`, but with Mode explicitly passed in. There are two modes available:
  * `hard` - paths that do not exist in the objects to be tested are treated as
    errors.  This is the default.
  * `soft`  - missing paths are ignored. Only values that exist are checked for
    equality.

#### Examples

```erlang
Object1 = jsn:new([{<<"path1">>, <<"thing1">>},
                   {<<"path2">>, <<"thing2">>},
                   {<<"path3">>, <<"thing3">>}]).
% #{<<"path1">> => <<"thing1">>,
%   <<"path2">> => <<"thing2">>,
%   <<"path3">> => <<"thing3">>}

Object2 = jsn:new([{<<"path1">>, <<"thing1">>},
                   {<<"path2">>, <<"notthing2">>}]).
% #{<<"path1">> => <<"thing1">>, <<"path2">> => <<"notthing2">>}

% by path1, these objects are equal
jsn:equal([<<"path1">>], Object1, Object2).
% ok

% by path2, not so much
jsn:equal([<<"path2">>], Object1, Object2).
% {error,{not_equal,<<"mismatch of requested and existing field(s): path2">>}}

% same story with path3
jsn:equal([<<"path3">>], Object1, Object2).
% {error,{not_equal,<<"mismatch of requested and existing field(s): path3">>}}

% but if you use soft mode, path 3 works (because it's missing in Object2)
jsn:equal([<<"path3">>], Object1, Object2, soft).
% ok

% you can also use a list of objects for the 3rd argument.
jsn:equal([<<"path1">>, <<"path3">>], Object1, [Object1, Object2], soft).
% ok
```

### `is_equal/2` and `is_subset/2` - Boolean object comparison

* `is_equal(A, B)` - Compare a pair of objects (in proplist, eep18, or struct
  format), returning `true` if all values and/or key-value pairs are matched
  across both objects.
* `is_subset(A, B)` - Compare a pair of objects or json terms (in proplist,
  eep18, or struct format), returning `true` if all values, key-value pairs,
  and array members in the first input are present in the second input.

#### Examples

```erlang
Object1 = jsn:new([{<<"path1">>, <<"thing1">>},
                   {<<"path2">>, <<"thing2">>}]).
% #{<<"path1">> => <<"thing1">>,<<"path2">> => <<"thing2">>}

Object2 = jsn:new([{<<"path1">>, <<"thing1">>},
                   {<<"path2">>, <<"thing2">>}], [{format, struct}]).
% {struct,[{<<"path1">>,<<"thing1">>},
%          {<<"path2">>,<<"thing2">>}]}

Object3 = [{path1, <<"thing1">>},
           {path2, <<"thing2">>}].
% [{path1,<<"thing1">>},{path2,<<"thing2">>}]

jsn:is_equal(Object1, Object2).
% true

jsn:is_equal(Object1, Object3).
% true

Object4 = jsn:set(path1, Object1, 1).
% #{<<"path1">> => 1,<<"path2">> => <<"thing2">>}

jsn:is_equal(Object1, Object4).
% false

Object5 = jsn:set_list([{path3, <<"thing3">>}], Object1).
% #{<<"path1">> => <<"thing1">>,
%   <<"path2">> => <<"thing2">>,
%   <<"path3">> => <<"thing3">>}

Object6 = jsn:set_list([{path3, <<"thing3">>}], Object2).
% {struct,[{<<"path3">>,<<"thing3">>},
%          {<<"path1">>,<<"thing1">>},
%          {<<"path2">>,<<"thing2">>}]}

jsn:is_subset(Object1, Object1).
% true

jsn:is_subset(Object1, Object2).
% true

jsn:is_subset(Object1, Object3).
% true

jsn:is_subset(Object3, Object1).
% false
```

### `as_map/1`, `from_map/1,2`, `as_proplist/1`, and `from_proplist/1,2` - Object format conversion

* `as_map(Term)` - Convert any JSON objects in the input JSON term into map format.
* `from_map(Term)` and `from_map(Term, Options)` - Convert a JSON term with map-format JSON
  objects into an identical JSON term with all of the JSON objects converted into the default
  format (or the one specified in Options, if given).
* `as_proplist(Term)` - Convert any JSON objects in the input JSON term into proplist format.
* `from_proplist(Term)` and `from_proplist(Term, Options)` - Convert a JSON term with
  proplist-format JSON objects into an identical JSON term with all of the JSON objects converted
  into the default object format (or the one specified in Options, if given).

#### Examples

```erlang
Object1 = jsn:new([{<<"path1">>, <<"thing1">>},
                   {<<"path2">>, <<"thing2">>}]).
% #{<<"path1">> => <<"thing1">>,<<"path2">> => <<"thing2">>}

Object2 = jsn:as_proplist(Object1).
% [{<<"path2">>,<<"thing2">>},{<<"path1">>,<<"thing1">>}]

Object3 = jsn:from_proplist(Object2, [{format, struct}]).
% {struct,[{<<"path2">>,<<"thing2">>},
%          {<<"path1">>,<<"thing1">>}]}

Object1 = jsn:from_proplist(Object2).
% #{<<"path1">> => <<"thing1">>,<<"path2">> => <<"thing2">>}

Object1 = jsn:as_map(Object2).
% #{<<"path1">> => <<"thing1">>,<<"path2">> => <<"thing2">>}

Object2 = jsn:from_map(Object1, [{format, proplist}]).
% [{<<"path2">>,<<"thing2">>},{<<"path1">>,<<"thing1">>}]

Object3 = jsn:from_map(Object1, [{format, struct}]).
% {struct,[{<<"path2">>,<<"thing2">>},
%          {<<"path1">>,<<"thing1">>}]}
```

[ej]: https://github.com/seth/ej
[kvc]: https://github.com/etrepum/kvc
[erlson]: https://github.com/alavrik/erlson
[jsx]: https://github.com/talentdeficit/jsx
[jsone]: https://github.com/sile/jsone
[jsonx]: https://github.com/iskra/jsonx
[jiffy]: https://github.com/davisp/jiffy
[mochijson2]: https://github.com/mochi/mochiweb/blob/master/src/mochijson2.erl
