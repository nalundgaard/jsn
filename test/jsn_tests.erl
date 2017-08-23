
-module(jsn_tests).

-ifdef(TEST).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

-include("jsn.hrl").

%%==============================================================================
%% random number compatibility
%%==============================================================================

-spec rand_uniform(Min :: integer(), Max :: integer()) -> integer().
-ifdef(has_rand).
rand_uniform(Min, Max) when Min =< Max ->
    rand:uniform(Max - Min + 1) + Min - 1.
-else.
rand_uniform(Min, Max) when Min =< Max ->
    crypto:rand_uniform(Min, Max).
-endif.

%%==============================================================================
%% json object generation
%%==============================================================================

generate_json_object(Depth, Options) ->
    PairNumber = rand_uniform(5, 10),
    Pairs = [
        { generate_json_key(6), generate_json_value(Depth-1, Options)}
        || _ <- lists:seq(1,PairNumber)
    ],
    jsn:new(Pairs, Options).


generate_json_key(I) ->
    JsonString = generate_json_string(I),
    case rand_uniform(1, 2) of
        1 -> JsonString;
        2 -> binary_to_atom(JsonString, utf8)
    end.


generate_json_string(I) ->
    Letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
              "0123456789!@#$%^&*()_+-=[]{}/\\|<>,;'\"`",
    Length = rand_uniform(1, I),
    list_to_binary([
        lists:nth(rand_uniform(1, length(Letters)), Letters)
        || _ <- lists:seq(1,Length)
    ]).


generate_json_value(X, _Options) when X =< 0 ->
    generate_json_number();

generate_json_value(Depth, Options) ->
    case rand_uniform(1, 6) of
        1 -> generate_json_object(Depth, Options);
        2 -> generate_json_array(Depth, Options);
        3 -> generate_json_string(6);
        4 -> generate_json_number();
        5 -> generate_json_boolean();
        6 -> 'null'
    end.


generate_json_array(Depth, Options) ->
    Length = rand_uniform(0, 10),
    [ generate_json_value(Depth-1, Options) || _ <- lists:seq(1, Length) ].


generate_json_number() ->
    generate_json_number(1000).


generate_json_number(N) ->
    case rand_uniform(1, 2) of
        1 -> rand_uniform(1, 2 * N) - N;
        2 ->
            M = rand_uniform(1, 9999999999999),
            rand_uniform(1, N * M) / (M)
    end.


generate_json_boolean() ->
    Values = ['true', 'false'],
    lists:nth(rand_uniform(1, length(Values)), Values).


%%==============================================================================
%% Unit Tests
%%==============================================================================


new_0_test() ->
    ?assertEqual([], jsn:new()).


new_1_test_() ->
    Object1 = [{<<"foo">>, <<"bar">>}],
    Object2 = [{<<"foo">>, <<"bar">>}, {<<"baz">>, 42}],
    Object3 = [{<<"foo">>, [{<<"bar">>, <<"hoge">>}]}],
    Object4 = [{<<"foo">>, [{<<"bar">>, [1,2,3]}]}],
    [?_assertEqual(?EMPTY_PROPLIST, jsn:new([])),
     ?_assertEqual(Object1, jsn:new({<<"foo">>, <<"bar">>})),
     ?_assertEqual(Object1, jsn:new({foo, <<"bar">>})),
     ?_assertEqual(Object1, jsn:new([{<<"foo">>, <<"bar">>}])),
     ?_assertEqual(Object2, jsn:new([{<<"foo">>, <<"bar">>}, {<<"baz">>, 42}])),
     ?_assertEqual(Object2, jsn:new([{foo, <<"bar">>}, {baz, 42}])),
     ?_assertEqual(Object3, jsn:new({'foo.bar', <<"hoge">>})),
     ?_assertEqual(Object3, jsn:new({<<"foo.bar">>, <<"hoge">>})),
     ?_assertEqual(Object3, jsn:new({[<<"foo">>, <<"bar">>], <<"hoge">>})),
     ?_assertEqual(Object4, jsn:new({{<<"foo">>, <<"bar">>}, [1,2,3]})),
     ?_assertEqual(Object4, jsn:new([{{<<"foo">>, <<"bar">>}, [9,9,9]},
                                     {{<<"foo">>, <<"bar">>, first}, 1},
                                     {{<<"foo">>, <<"bar">>, last}, 3},
                                     {{<<"foo">>, <<"bar">>, 2}, 2}]))].


new_2_test_() ->
    Object1 = [{<<"foo">>, <<"bar">>}],
    Object1Eep18 = {[{<<"foo">>, <<"bar">>}]},
    Object1Struct = {struct, [{<<"foo">>, <<"bar">>}]},
    Object2 = [{<<"foo">>, [{<<"bar">>, <<"hoge">>}]}],
    Object2Eep18 = {[{<<"foo">>, {[{<<"bar">>, <<"hoge">>}]}}]},
    Object2Struct = {struct, [{<<"foo">>, {struct, [{<<"bar">>, <<"hoge">>}]}}]},
    [?_assertEqual(?EMPTY_PROPLIST, jsn:new([])),
     ?_assertEqual(?EMPTY_EEP18, jsn:new([], [{format, eep18}])),
     ?_assertEqual(?EMPTY_STRUCT, jsn:new([], [{format, struct}])),
     ?_assertEqual(Object1, jsn:new({<<"foo">>, <<"bar">>}, [{format, proplist}])),
     ?_assertEqual(Object1, jsn:new({foo, <<"bar">>}, [])),
     ?_assertEqual(Object1Eep18, jsn:new({foo, <<"bar">>}, [{format, eep18}])),
     ?_assertEqual(Object1Eep18, jsn:new([{<<"foo">>, <<"bar">>}], [{format, eep18}])),
     ?_assertEqual(Object1Struct, jsn:new({foo, <<"bar">>}, [{format, struct}])),
     ?_assertEqual(Object1Struct, jsn:new([{<<"foo">>, <<"bar">>}], [{format, struct}])),
     ?_assertEqual(Object2, jsn:new({'foo.bar', <<"hoge">>}, [])),
     ?_assertEqual(Object2, jsn:new({'foo.bar', <<"hoge">>}, [{format, proplist}])),
     ?_assertEqual(Object2Eep18, jsn:new({<<"foo.bar">>, <<"hoge">>}, [{format, eep18}])),
     ?_assertEqual(Object2Struct, jsn:new({'foo.bar', <<"hoge">>}, [{format, struct}])),
     ?_assertError(badarg, jsn:new([], [{format, random}]))].


get_test_() ->
    Src = jsn:new([{<<"foo">>, <<"bar">>},
                   {<<"qux.lux">>, 99},
                   {<<"baz">>, [0, 10, 20]}]),
    [?_assertEqual(<<"bar">>, jsn:get(<<"foo">>, Src)),
     ?_assertEqual(<<"bar">>, jsn:get(foo, Src)),
     ?_assertEqual(99, jsn:get('qux.lux', Src)),
     ?_assertEqual(99, jsn:get([<<"qux">>, lux], Src)),
     ?_assertEqual(99, jsn:get({<<"qux">>, <<"lux">>}, Src)),
     ?_assertEqual(0, jsn:get({<<"baz">>, 1}, Src)),
     ?_assertEqual(0, jsn:get({<<"baz">>, first}, Src)),
     ?_assertEqual(10, jsn:get({<<"baz">>, 2}, Src)),
     ?_assertEqual(20, jsn:get({<<"baz">>, 3}, Src)),
     ?_assertEqual(20, jsn:get({<<"baz">>, last}, Src)),
     ?_assertEqual(undefined, jsn:get({<<"baz">>, 99}, Src)),
     ?_assertEqual(undefined, jsn:get(<<"bar">>, Src))].


get_list_test_() ->
    Src = jsn:new([{<<"foo">>, <<"bar">>},
                   {<<"qux">>, 99},
                   {<<"baz">>, <<"hoge">>},
                   {[a, b, c], <<"d">>}]),
    R1 = [<<"bar">>, 99],
    R2 = [<<"bar">>, undefined],
    R3 = [99, <<"d">>],
    [?_assertEqual(R1, jsn:get_list([foo, qux], Src)),
     ?_assertEqual(R2, jsn:get_list([<<"foo">>, mop], Src)),
     ?_assertEqual(R3, jsn:get_list([[<<"qux">>], [a,<<"b">>,c]], Src)),
     ?_assertEqual(R3, jsn:get_list([<<"qux">>, 'a.b.c'], Src)),
     ?_assertEqual(R3, jsn:get_list([qux, [<<"a">>,b,<<"c">>]], Src))].


find_test_() ->
    Src0 = jsn:new([
            {<<"foo">>, <<"bar">>},
            {<<"qux">>, 99},
            {<<"baz">>, <<"hoge">>}
    ]),
    Src1 = jsn:set(<<"moo">>, Src0, <<"cow">>),
    Src2 = jsn:set(<<"foo">>, Src0, <<"kaboom">>),
    Objects = [Src0, Src1, Src2],
    Haystack = jsn:new({<<"weeble">>, Objects}),
    Needle = <<"bar">>,
    [?_assertEqual([Src0, Src1], jsn:find(foo, Needle, Objects)),
     ?_assertEqual([], jsn:find(mop, Needle, Objects)),
     ?_assertEqual([Src1], jsn:find(moo, <<"cow">>, Objects)),
     ?_assertEqual([], jsn:find(moo, cow, Objects)),
     ?_assertEqual([Src0, Src1], jsn:find(weeble, foo, Needle, Haystack)),
     ?_assertEqual([], jsn:find(weeble, mop, Needle, Haystack)),
     ?_assertEqual([Src1], jsn:find(weeble, moo, <<"cow">>, Haystack)),
     ?_assertEqual([], jsn:find(weeble, moo, cow, Haystack))].


set_test_() ->
    Path = <<"foo.bar">>,
    Object1 = jsn:new({Path, <<"baz">>}),
    Object2 = jsn:new({Path, [1, 2, 3]}),
    [?_assertEqual([{<<"foo">>, <<"bar">>}], jsn:set(<<"foo">>, jsn:new(), <<"bar">>)),
     ?_assertEqual(Object1, jsn:set(Path, Object1, <<"baz">>)),
     ?_assertEqual(Object2, jsn:set(Path, Object1, [1, 2, 3])),
     ?_assertEqual([99, 2, 3], jsn:get(Path, jsn:set({<<"foo">>, <<"bar">>, first}, Object2, 99))),
     ?_assertEqual([99, 2, 3], jsn:get(Path, jsn:set({<<"foo">>, <<"bar">>, 1}, Object2, 99))),
     ?_assertEqual([1, 99, 3], jsn:get(Path, jsn:set({<<"foo">>, <<"bar">>, 2}, Object2, 99))),
     ?_assertEqual([1, 2, 99], jsn:get(Path, jsn:set({<<"foo">>, <<"bar">>, 3}, Object2, 99))),
     ?_assertEqual([1, 2, 99], jsn:get(Path, jsn:set({<<"foo">>, <<"bar">>, last}, Object2, 99))),
     ?_assertThrow({error, {not_an_object, _}}, jsn:set(<<"k">>, [1,2,3], <<"v">>)),
     ?_assertThrow({error, {not_an_object, _}}, jsn:set(<<"k">>, <<"a">>, <<"v">>)),
     ?_assertThrow({error, {not_an_object, _}}, jsn:set(<<"k">>, 0, <<"v">>)),
     ?_assertThrow({error, {not_an_object, _}}, jsn:set(<<"k">>, {[0]}, <<"v">>)),
     ?_assertThrow({error, {not_an_array, _}}, jsn:set({1}, [{<<"k">>, 1}], <<"v">>)),
     ?_assertThrow({error, {not_an_array, _}}, jsn:set({<<"k">>, 1}, [{<<"k">>, 1}], <<"v">>))].


set_list_test_() ->
    EmptyObject = jsn:new(),
    StartObject = [{<<"foo">>, <<"bar">>}],
    EndObject1 = [{<<"foo">>, <<"bar">>}, {<<"baz">>, 42}],
    EndObject2 = [{<<"foo">>, <<"bar">>}, {<<"baz">>, 42}, {<<"hoge">>, <<"qux">>}],
    [?_assertEqual(EmptyObject, jsn:set_list([], EmptyObject)),
     ?_assertEqual(StartObject, jsn:set_list([{<<"foo">>, <<"bar">>}], EmptyObject)),
     ?_assertEqual(StartObject, jsn:set_list([], StartObject)),
     ?_assertEqual(EndObject1, jsn:set_list([{<<"baz">>, 42}], StartObject)),
     ?_assertEqual(EndObject2, jsn:set_list([{<<"baz">>, 42}, {<<"hoge">>, <<"qux">>}], StartObject)),
     ?_assertEqual(EndObject2, jsn:set_list([{<<"hoge">>, <<"qux">>}], EndObject1)),
     ?_assertEqual(<<"qux">>, jsn:get(<<"hoge">>, jsn:set_list([{<<"hoge">>, <<"qux">>}], EndObject1)))].


delete_test_() ->
    Path1 = <<"foo.bar">>,
    Path2 = <<"qux.lux">>,
    Object1 = jsn:new([{Path1, <<"baz">>}, {Path2, [1,2,3]}]),
    [?_assertEqual([], jsn:delete(<<"foo">>, [{<<"foo">>, <<"bar">>}])),
     ?_assertEqual(jsn:new({Path2, [1,2,3]}), jsn:delete(foo, Object1)),
     ?_assertEqual(jsn:new({Path1, <<"baz">>}), jsn:delete(qux, Object1)),
     ?_assertEqual([2, 3], jsn:get(Path2, jsn:delete({<<"qux">>, <<"lux">>, first}, Object1))),
     ?_assertEqual([2, 3], jsn:get(Path2, jsn:delete({<<"qux">>, <<"lux">>, 1}, Object1))),
     ?_assertEqual([1, 3], jsn:get(Path2, jsn:delete({<<"qux">>, <<"lux">>, 2}, Object1))),
     ?_assertEqual([1, 2], jsn:get(Path2, jsn:delete({<<"qux">>, <<"lux">>, 3}, Object1))),
     ?_assertEqual([1, 2], jsn:get(Path2, jsn:delete({<<"qux">>, <<"lux">>, last}, Object1))),
     ?_assertThrow({error, {not_an_object, _}}, jsn:delete(<<"k">>, [1,2,3])),
     ?_assertThrow({error, {not_an_object, _}}, jsn:delete(<<"k">>, <<"a">>)),
     ?_assertThrow({error, {not_an_object, _}}, jsn:delete(<<"k">>, 0)),
     ?_assertThrow({error, {not_an_object, _}}, jsn:delete(<<"k">>, {[0]})),
     ?_assertThrow({error, {not_an_array, _}}, jsn:delete({1}, [{<<"k">>, 1}])),
     ?_assertThrow({error, {not_an_array, _}}, jsn:delete({<<"k">>, 1}, [{<<"k">>, 1}]))].

delete_list_test_() ->
    Base = jsn:new([{<<"foo">>, <<"bar">>},
                    {<<"qux">>, 99},
                    {<<"baz">>, <<"hoge">>}]),
    [?_assertEqual(jsn:new(), jsn:delete_list([<<"foo">>, <<"qux">>, <<"baz">>],  Base)),
     ?_assertEqual(Base, jsn:delete_list([],  Base))].


delete_if_equal_test_() ->
    Base = jsn:new([{<<"foo">>, null},
                    {<<"qux">>, 99},
                    {<<"baz">>, <<"hoge">>}]),
    [?_assertEqual(jsn:new([{<<"foo">>, null},
                            {<<"baz">>, <<"hoge">>}]),
                   jsn:delete_if_equal(<<"qux">>, [null, 99], Base)),
     ?_assertEqual(Base, jsn:delete_if_equal(<<"baz">>, null, Base))].

copy_test_() ->
    Src = jsn:new([{<<"foo">>, <<"bar">>},
                    {<<"qux">>, 99},
                    {<<"baz">>, <<"hoge">>}]),
    R1 = [jsn:new([{<<"foo">>, <<"bar">>}])],
    R2 = [jsn:new([{<<"foo">>, <<"bar">>},
                   {<<"mop">>, undefined}], [{format, eep18}])],
    R3 = [jsn:new([{<<"foo">>, <<"bar">>},
                   {<<"mop">>, undefined}], [{format, struct}])],
    [?_assertEqual([Src], jsn:copy([<<"foo">>], Src, Src)),
     ?_assertEqual(R1, jsn:copy([<<"foo">>], Src, jsn:new())),
     ?_assertEqual(R1, jsn:copy([<<"foo">>], Src, [jsn:new()])),
     ?_assertEqual(R2, jsn:copy([<<"foo">>, <<"mop">>], Src, jsn:new([], [{format, eep18}]))),
     ?_assertEqual(R2, jsn:copy([<<"foo">>, <<"mop">>], Src, [jsn:new([], [{format, eep18}])])),
     ?_assertEqual(R3, jsn:copy([<<"foo">>, <<"mop">>], Src, jsn:new([], [{format, struct}]))),
     ?_assertEqual(R3, jsn:copy([<<"foo">>, <<"mop">>], Src, [jsn:new([], [{format, struct}])])),
     ?_assertError(badarg, jsn:copy([<<"foo">>, <<"mop">>], Src, jsn:new(), gg))].

transform_test_() ->
    Src = jsn:new([{<<"foo">>, <<"bar">>},
                   {<<"qux">>, 99}]),
    F = fun(Value) ->
            case Value of
                undefined -> false;
                        _ -> true
            end
        end,
    R1 = jsn:new([{<<"foo">>, true},
                  {<<"qux">>, 99}]),
    R2 = jsn:new([{<<"foo">>, true},
                  {<<"qux">>, 99},
                  {<<"mop">>, false}]),
    T1 = [{<<"foo">>, F}],
    T2 = [{<<"foo">>, F}, {<<"mop">>, F}],
    [?_assertEqual(R1, jsn:transform(T1, Src)),
     ?_assertEqual(R2, jsn:transform(T2, Src))].

path_transform_test_() ->
    Src = jsn:new([{<<"foo">>, <<"bar">>},
                   {<<"qux">>, 99}]),
    R1 = jsn:new([{<<"qux">>, 99},
                  {<<"baz">>, <<"bar">>}]),
    R2 = jsn:new([{<<"qux">>, 99},
                  {<<"baz">>, <<"bar">>}]),
    T1 = [{<<"foo">>, <<"baz">>}],
    T2 = [{<<"foo">>, <<"baz">>}, {<<"map">>, <<"mop">>}], 
    [?_assertEqual(R1, jsn:path_transform(T1, Src)),
     ?_assertEqual(R2, jsn:path_transform(T2, Src))].

copy_mutate_test_() ->
    Src = jsn:new([{<<"foo">>, <<"bar">>},
                   {<<"qux">>, 99},
                   {<<"baz">>, <<"hoge">>}]),
    R1 = [ jsn:new([ {<<"foo">>, true} ]) ],
    R2 = [ jsn:new([ {<<"foo">>, true}, {<<"mop">>, false} ]) ],
    F = fun(Value) ->
        case Value of
            undefined ->
                false;
            _ ->
                true
        end
    end,
    [?_assertEqual(R1, jsn:copy([<<"foo">>], Src, jsn:new(), F)),
     ?_assertEqual(R2, jsn:copy([<<"foo">>, <<"mop">>], Src, [jsn:new()], F))].


path_elements_test_() ->
    Keys = [<<"foo">>, <<"bar">>],
    [?_assertEqual(Keys, jsn:path_elements('foo.bar')),
     ?_assertEqual(Keys, jsn:path_elements(<<"foo.bar">>)),
     ?_assertEqual(Keys, jsn:path_elements([foo, bar])),
     ?_assertEqual(Keys, jsn:path_elements([<<"foo">>, <<"bar">>])),
     ?_assertEqual(Keys, jsn:path_elements({<<"foo">>, <<"bar">>})),
     ?_assertEqual(Keys ++ [1], jsn:path_elements({<<"foo">>, <<"bar">>, 1})),
     ?_assertEqual([99 | Keys], jsn:path_elements({99, <<"foo">>, <<"bar">>})),
     ?_assertEqual([first | Keys], jsn:path_elements({first, <<"foo">>, <<"bar">>})),
     ?_assertEqual([<<"a">>, 8, <<"c">>], jsn:path_elements({<<"a">>, 8, <<"c">>})),
     ?_assertEqual([<<"a">>, 8, <<"c">>], jsn:path_elements({<<"a">>, 8, <<"c">>})),
     ?_assertEqual([<<"a">>, last, <<"c">>], jsn:path_elements({<<"a">>, last, <<"c">>})),
     ?_assertError(badarg, jsn:path_elements({"foo", "bar"})),
     ?_assertError(badarg, jsn:path_elements(["foo", "bar"])),
     ?_assertError(badarg, jsn:path_elements("foo.bar")),
     ?_assertError(badarg, jsn:path_elements({foo, bar})),
     ?_assertError(badarg, jsn:path_elements({-99})),
     ?_assertError(badarg, jsn:path_elements({0})),
     ?_assertError(badarg, jsn:path_elements(0)),
     ?_assertError(badarg, jsn:path_elements([10, <<"foo">>]))].


sort_test_() ->
    Proplist = [{c, [{z, 1},{x, 2}]},
                {a, [3,2,1]},
                {b, null}],
    SortedProplist = [{a, [1,2,3]},
                      {b, null},
                      {c, [{x, 2},{z, 1}]}],
    Eep18 = jsn:from_proplist(Proplist, [{format, eep18}]),
    Struct = jsn:from_proplist(Proplist, [{format, struct}]),
    SortedEep18 = jsn:from_proplist(SortedProplist, [{format, eep18}]),
    SortedStruct = jsn:from_proplist(SortedProplist, [{format, struct}]),
    [?_assertEqual(SortedProplist, jsn:sort(Proplist)),
     ?_assertEqual(SortedEep18, jsn:sort(Eep18)),
     ?_assertEqual(SortedStruct, jsn:sort(Struct))].


sort_keys_test_() ->
    Proplist = [{c, [{z, 1},{x, 2}]},
                {a, [3,2,1]},
                {b, [{s, [<<"a">>,3,2]},{r, true}]}],
    SortedProplist = [{a, [3,2,1]},
                      {b, [{r, true},{s, [<<"a">>,3,2]}]},
                      {c, [{x, 2},{z, 1}]}],
    Eep18 = jsn:from_proplist(Proplist, [{format, eep18}]),
    Struct = jsn:from_proplist(Proplist, [{format, struct}]),
    SortedEep18 = jsn:from_proplist(SortedProplist, [{format, eep18}]),
    SortedStruct = jsn:from_proplist(SortedProplist, [{format, struct}]),
    [?_assertEqual(SortedProplist, jsn:sort_keys(Proplist)),
     ?_assertEqual(SortedEep18, jsn:sort_keys(Eep18)),
     ?_assertEqual(SortedStruct, jsn:sort_keys(Struct))].

equal_test_() ->
    Field1 = <<"my field">>,
    Field2 = <<"my great field">>,
    Params = jsn:new([{Field1, false},
                      {Field2, <<"42">>}]),
    EqualParams = jsn:new([{Field2, <<"42">>},
                           {Field1, false}]),
    EqualStructParams = jsn:new([{Field2, <<"42">>},
                                 {Field1, false}], [{format, struct}]),
    EqualEep18Params = jsn:new([{Field2, <<"42">>},
                                {Field1, false}], [{format, eep18}]),
    MisMatchedParams = jsn:new([{Field1, false},
                                {Field2, <<"451">>}]),
    Nested = jsn:new([{Field1, false},
                      {Field2, jsn:new([{Field1, true},
                                        {Field2, <<"42">>}])}]),
    EqualNested = jsn:new([{Field1, false},
                           {Field2, jsn:new([{Field2, <<"42">>},
                                             {Field1, true}])}]),
    List = jsn:new([{Field1, [jsn:new([{Field1, true},
                                       {Field2, <<"42">>}]),
                              jsn:new([{Field2, <<"42">>},
                                       {Field1, false}])]}]),
    EqualList = jsn:new({Field1, [jsn:new([{Field2, <<"42">>},
                                           {Field1, false}]),
                                  jsn:new([{Field2, <<"42">>},
                                           {Field1, true}])]}),
    Struct = jsn:new([{Field1, false},
                      {[Field2,Field1], true},
                      {[Field2, Field2], <<"42">>}], [{format, struct}]),
    EqualStruct = jsn:new([{[Field2, Field2], <<"42">>},
                           {[Field2, Field1], true},
                           {Field1, false}], [{format, struct}]),
    MismatchedStruct = jsn:new([{Field1, true},
                                {[Field2, Field2], <<"42">>},
                                {[Field2, Field1], false}], [{format, struct}]),
    [?_assertEqual(ok, jsn:equal([Field1, Field2], Params, Params, hard)),
     ?_assertEqual(ok, jsn:equal([Field1, Field2], Params, [Params, Params], hard)),
     ?_assertEqual(ok, jsn:equal([Field1, Field2], Params, Params, soft)),
     ?_assertEqual(ok, jsn:equal([Field1, Field2], Params, [Params, jsn:new()], soft)),
     ?_assertEqual(ok, jsn:equal([Field2], Params, Params)),
     ?_assertEqual(ok, jsn:equal([Field1], Params, MisMatchedParams)),
     ?_assertEqual(ok, jsn:equal([Field2], Params, [Params, jsn:new()], soft)),
     ?_assertMatch({error, {not_equal, _}},
                   jsn:equal([Field2], Params, jsn:new(), hard)),
     ?_assertMatch({error, {not_equal, _}},
                   jsn:equal([Field2], Params, [EqualParams, jsn:new()], hard)),
     ?_assertMatch({error, {not_equal, _}},
                   jsn:equal([Field1, Field2], Params, MisMatchedParams)),
     ?_assertMatch({error, {not_equal, _}},
                   jsn:equal([Field2], Params, [Params, jsn:new(), MisMatchedParams], soft)),
     ?_assertEqual(ok, jsn:equal([Field1, Field2], Params, EqualParams)),
     ?_assertEqual(ok, jsn:equal([Field2, Field1], Params, EqualParams)),
     ?_assertEqual(ok, jsn:equal([Field1], Params, EqualParams)),
     ?_assertEqual(ok, jsn:equal([Field2], Params, EqualParams)),
     ?_assertEqual(ok, jsn:equal([Field1], Params, EqualStructParams)),
     ?_assertEqual(ok, jsn:equal([Field2], Params, EqualStructParams)),
     ?_assertEqual(ok, jsn:equal([Field1], Params, EqualEep18Params)),
     ?_assertEqual(ok, jsn:equal([Field2], Params, EqualEep18Params)),
     ?_assertEqual(true, jsn:sort_equal(Params, EqualParams)),
     ?_assertEqual(false, jsn:sort_equal(MisMatchedParams, Params)),
     ?_assertMatch({error, {not_equal, _}},
                   jsn:equal([Field2], Nested, EqualNested)),
     ?_assertEqual(true, jsn:sort_equal(Nested, EqualNested)),
     ?_assertMatch({error, {not_equal, _}},
                   jsn:equal([Field1], List, EqualList)),
     ?_assertEqual(true, jsn:sort_equal(List, EqualList)),
     ?_assertEqual(ok, jsn:equal([Field1], Struct, EqualStruct)),
     ?_assertEqual(true, jsn:sort_equal(Struct, EqualStruct)),
     ?_assertMatch({error, {not_equal, _}},
                   jsn:equal([Field1], Struct, MismatchedStruct)),
     ?_assertEqual(false, jsn:sort_equal(Struct, MismatchedStruct))].


path_equal_test_() ->
    Field1 = <<"cat meow">>,
    Field2 = 'dog.bark.bark',
    Params = jsn:new([{Field1, 123},
                      {Field2, <<"42">>}]),
    MisMatchedParams = jsn:new([{Field1, 123},
                                {Field2, <<"451">>}]),
    [?_assertEqual(ok, jsn:path_equal(Field1, Params, Params, soft)),
     ?_assertEqual(ok, jsn:path_equal(Field2, Params, Params, hard)),
     ?_assertEqual(ok, jsn:path_equal(Field1, Params, MisMatchedParams, hard)),
     ?_assertEqual(ok, jsn:path_equal(Field1, Params, jsn:new(), soft)),
     ?_assertEqual({error, Field1}, jsn:path_equal(Field1, Params, jsn:new(), hard)),
     ?_assertEqual({error, jsn:to_binary(Field2)}, jsn:path_equal(Field2, Params, MisMatchedParams, soft)),
     ?_assertEqual({error, jsn:to_binary(Field2)}, jsn:path_equal(Field2, Params, MisMatchedParams, soft))].


is_equal_test_() ->
    TestParams = [{'foo.bar.baz', null},
                  {<<"foo.fum">>, <<"ok">>},
                  {<<"foo.q.e.d">>, true},
                  {<<"abc">>, [1,2,3]}],
    Extra = [{'foo.extra', <<"extra">>}],
    Proplist = jsn:new(TestParams, [{format, proplist}]),
    Eep18 = jsn:new(TestParams, [{format, eep18}]),
    Struct = jsn:new(TestParams, [{format, struct}]),
    ExtraProplist = jsn:new(Extra ++ TestParams, [{format, proplist}]),
    ExtraEep18 = jsn:new(TestParams ++ Extra, [{format, eep18}]),
    ExtraStruct = jsn:new(TestParams ++ Extra, [{format, struct}]),
    [?_assert(jsn:is_equal(<<"a">>, <<"a">>)),
     ?_assert(jsn:is_equal([1,2], [1,2])),
     ?_assert(jsn:is_equal(jsn:new(), jsn:new())),
     ?_assert(jsn:is_equal(Proplist, Proplist)),
     ?_assert(jsn:is_equal(Proplist, Struct)),
     ?_assert(jsn:is_equal(Proplist, Eep18)),
     ?_assert(jsn:is_equal(Proplist, Struct)),
     ?_assert(jsn:is_equal(Eep18, Proplist)),
     ?_assert(jsn:is_equal(Eep18, Eep18)),
     ?_assert(jsn:is_equal(Eep18, Struct)),
     ?_assert(jsn:is_equal(Struct, Proplist)),
     ?_assert(jsn:is_equal(Struct, Eep18)),
     ?_assert(jsn:is_equal(Struct, Struct)),
     ?_assert(jsn:is_equal(null, null)),
     ?_assertNot(jsn:is_equal(true, false)),
     ?_assertNot(jsn:is_equal(Proplist, ExtraProplist)),
     ?_assertNot(jsn:is_equal(Proplist, ExtraEep18)),
     ?_assertNot(jsn:is_equal(Proplist, ExtraStruct)),
     ?_assertNot(jsn:is_equal(Eep18, ExtraProplist)),
     ?_assertNot(jsn:is_equal(Eep18, ExtraEep18)),
     ?_assertNot(jsn:is_equal(Eep18, ExtraStruct)),
     ?_assertNot(jsn:is_equal(Struct, ExtraProplist)),
     ?_assertNot(jsn:is_equal(Struct, ExtraEep18)),
     ?_assertNot(jsn:is_equal(Struct, ExtraStruct)),
     ?_assertNot(jsn:is_equal(undefined, undefined)),
     ?_assertNot(jsn:is_equal([1,undefined], [1,undefined]))].


is_subset_test_() ->
    [?_assert(jsn:is_subset(<<"a">>, <<"a">>)),
     ?_assert(jsn:is_subset([1,2], [3,4,2,1])),
     ?_assert(jsn:is_subset(jsn:new(), jsn:new({a, 99}))),
     ?_assert(jsn:is_subset(null, null)),
     ?_assertNot(jsn:is_subset(undefined, undefined)),
     ?_assertNot(jsn:is_subset([1,undefined], [1,undefined])),
     ?_assertNot(jsn:is_subset(true, false))].


is_equal() ->
    Eep18 = generate_json_object(7, [{format, eep18}]),
    Proplist = jsn:as_proplist(Eep18),
    Struct = jsn:from_proplist(Proplist, [{format, struct}]),
    NotEqualProplist = [{qqqqqqqqqqqq, 9} | Proplist],
    ?assert(jsn:is_equal(Proplist, Proplist)),
    ?assert(jsn:is_equal(Proplist, Eep18)),
    ?assert(jsn:is_equal(Proplist, Struct)),
    ?assert(jsn:is_equal([Proplist, Struct, 3], [Proplist, Eep18, 3])),
    ?assertNot(jsn:is_equal(NotEqualProplist, Proplist)),
    ?assertNot(jsn:is_equal(NotEqualProplist, Struct)),
    ?assertNot(jsn:is_equal(NotEqualProplist, Eep18)).


is_subset() ->
    Eep18 = generate_json_object(7, [{format, eep18}]),
    Proplist = jsn:as_proplist(Eep18),
    Struct = jsn:from_proplist(Proplist, [{format, struct}]),
    SuperProplist = [{qqqqqqqqqqqq, 9} | Proplist],
    ?assert(jsn:is_subset(Proplist, SuperProplist)),
    ?assert(jsn:is_subset(Eep18, SuperProplist)),
    ?assert(jsn:is_subset(Struct, SuperProplist)),
    ?assert(jsn:is_subset([Struct], [6, SuperProplist])),
    ?assertNot(jsn:is_subset(SuperProplist, Proplist)),
    ?assertNot(jsn:is_subset(SuperProplist, Eep18)),
    ?assertNot(jsn:is_subset(SuperProplist, Struct)),
    ?assertNot(jsn:is_subset([SuperProplist, 4], [Struct, 4])).

%% run the above tests several times
is_equal_subset_test_() ->
    {setup,
        fun() -> crypto:start() end,
        fun(_) -> ok end,
        [fun is_equal/0 || _ <- lists:seq(1, 5)] ++
        [fun is_subset/0 || _ <- lists:seq(1, 5)]}.


encode_decode_test_() ->
    Paths = [
        {<<"test.object.key1">>, <<"value">>},
        {<<"test.object.key2">>, true}
    ],
    Object = jsn:new(Paths),
    StructOpt = [{format, struct}],
    ObjectStruct = jsn:new(Paths, StructOpt),
    [?_assertEqual(jsn:new(), jsn:decode("{}")),
     ?_assertEqual(jsn:new([], StructOpt), jsn:decode("{}", StructOpt)),
     ?_assertEqual(jsn:new({a,1}), jsn:decode("{\"a\": 1}")),
     ?_assertEqual(jsn:new({a,1}), jsn:decode(["{","\"a\":",[" 1}"]])),
     ?_assertEqual(jsn:new({a,1}), jsn:decode(<<"{\"a\": 1}">>)),
     ?_assertEqual(Object, jsn:decode(jsn:encode(Object))),
     ?_assertEqual(Object, jsn:decode(jsn:encode(ObjectStruct))),
     ?_assertEqual(ObjectStruct, jsn:decode(jsn:encode(Object), StructOpt)),
     ?_assertEqual(ObjectStruct, jsn:decode(jsn:encode(ObjectStruct), StructOpt)),
     ?_assertThrow({error, _}, jsn:decode("{}", [{format, random}])),
     ?_assertThrow({error, _}, jsn:decode(0)),
     ?_assertThrow({error, _}, jsn:decode({[]})),
     ?_assertThrow({error, _}, jsn:decode("{ ")),
     ?_assertThrow({error, _}, jsn:decode(<<"{} /dfg">>)),
     ?_assertThrow({error, _}, jsn:decode("{ ", StructOpt)),
     ?_assertThrow({error, _}, jsn:encode({error, bogus, object}))].


from_as_proplist() ->
    Eep18 = generate_json_object(7, [{format, eep18}]),
    Proplist = jsn:as_proplist(Eep18),
    Proplist = jsn:from_proplist(Proplist),
    Proplist = jsn:from_proplist(Proplist, [{format, proplist}]),
    Eep18 = jsn:from_proplist(Proplist, [{format, eep18}]),
    Struct = jsn:from_proplist(Proplist, [{format, struct}]),
    ?assertEqual(Eep18, jsn:from_proplist(Proplist, [{format, eep18}])),
    ?assertEqual(Proplist, jsn:as_proplist(Proplist)),
    ?assertEqual(Proplist, jsn:as_proplist(Eep18)),
    ?assertEqual(Proplist, jsn:as_proplist(Struct)),
    ?assert(jsn:is_equal(Proplist, Eep18)),
    ?assert(jsn:is_equal(Proplist, Struct)).

%% run the above test several times
from_as_proplist_test_() ->
    {setup,
        fun() -> crypto:start() end,
        fun(_) -> crypto:stop() end,
        [fun from_as_proplist/0 || _ <- lists:seq(1, 5)]}.


set_nth_test_() ->
    [?_assertEqual([<<"X">>, <<"b">>, <<"c">>], jsn:set_nth(1, [<<"a">>, <<"b">>, <<"c">>], <<"X">>)),
     ?_assertEqual([<<"X">>, <<"b">>, <<"c">>], jsn:set_nth(first, [<<"a">>, <<"b">>, <<"c">>], <<"X">>)),
     ?_assertEqual([<<"a">>, <<"X">>, <<"c">>], jsn:set_nth(2, [<<"a">>, <<"b">>, <<"c">>], <<"X">>)),
     ?_assertEqual([<<"a">>, <<"b">>, <<"X">>], jsn:set_nth(3, [<<"a">>, <<"b">>, <<"c">>], <<"X">>)),
     ?_assertEqual([<<"a">>, <<"b">>, <<"X">>], jsn:set_nth(last, [<<"a">>, <<"b">>, <<"c">>], <<"X">>)),
     ?_assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"X">>], jsn:set_nth(4, [<<"a">>, <<"b">>, <<"c">>], <<"X">>)),
     ?_assertEqual([<<"b">>, <<"c">>], jsn:set_nth(1, [<<"a">>, <<"b">>, <<"c">>], jsn__delete)),
     ?_assertEqual([<<"b">>, <<"c">>], jsn:set_nth(first, [<<"a">>, <<"b">>, <<"c">>], jsn__delete)),
     ?_assertEqual([<<"a">>, <<"c">>], jsn:set_nth(2, [<<"a">>, <<"b">>, <<"c">>], jsn__delete)),
     ?_assertEqual([<<"a">>, <<"b">>], jsn:set_nth(3, [<<"a">>, <<"b">>, <<"c">>], jsn__delete)),
     ?_assertEqual([<<"a">>, <<"b">>], jsn:set_nth(last, [<<"a">>, <<"b">>, <<"c">>], jsn__delete)),
     ?_assertThrow({error, {not_an_array, _}}, jsn:set_nth(1, [{<<"a">>, 1}], <<"X">>)),
     ?_assertThrow({error, {not_an_array, _}}, jsn:set_nth(1, q, <<"X">>)),
     ?_assertThrow({error, {invalid_array_index, _}}, jsn:set_nth(30, [1,2,3], <<"X">>)),
     ?_assertThrow({error, {invalid_array_index, _}}, jsn:set_nth(0, [1,2,3], <<"X">>)),
     ?_assertThrow({error, {invalid_array_index, _}}, jsn:set_nth(-99, [1,2,3], <<"X">>)),
     ?_assertThrow({error, {invalid_array_index, _}}, jsn:set_nth(<<"3">>, [1,2,3], <<"X">>))].


key_get_test_() ->
    V1 = <<"value1">>,
    V2 = [1,2,3],
    Proplist = [{<<"k1">>, V1},{<<"k2">>, V2}],
    Eep18 = jsn:new(Proplist),
    Struct = jsn:new(Proplist, [{format, struct}]),
    [?_assertEqual(V1, jsn:key_get(<<"k1">>, Proplist)),
     ?_assertEqual(V1, jsn:key_get(<<"k1">>, Eep18)),
     ?_assertEqual(V1, jsn:key_get(<<"k1">>, Struct)),
     ?_assertEqual(V2, jsn:key_get(<<"k2">>, Proplist)),
     ?_assertEqual(V2, jsn:key_get(<<"k2">>, Eep18)),
     ?_assertEqual(V2, jsn:key_get(<<"k2">>, Struct)),
     ?_assertEqual(undefined, jsn:key_get(<<"k3">>, Proplist)),
     ?_assertEqual(undefined, jsn:key_get(<<"k3">>, Eep18)),
     ?_assertEqual(undefined, jsn:key_get(<<"k3">>, Struct)),
     ?_assertEqual(undefined, jsn:key_get(<<"k1">>, [])),
     ?_assertEqual(undefined, jsn:key_get(<<"k1">>, jsn:new())),
     ?_assertEqual(undefined, jsn:key_get(<<"k1">>, jsn:new([], [{format, eep18}]))),
     ?_assertEqual(undefined, jsn:key_get(<<"k1">>, jsn:new([], [{format, struct}]))),
     ?_assertEqual(undefined, jsn:key_get(<<"k1">>, f)),
     ?_assertEqual(undefined, jsn:key_get(<<"k1">>, 99))].


get_nth_test_() ->
    [?_assertEqual(<<"a">>, jsn:get_nth(1, [<<"a">>, <<"b">>, <<"c">>], undefined)),
     ?_assertEqual(<<"a">>, jsn:get_nth(first, [<<"a">>, <<"b">>, <<"c">>], undefined)),
     ?_assertEqual(<<"b">>, jsn:get_nth(2, [<<"a">>, <<"b">>, <<"c">>], undefined)),
     ?_assertEqual(<<"c">>, jsn:get_nth(3, [<<"a">>, <<"b">>, <<"c">>], undefined)),
     ?_assertEqual(<<"c">>, jsn:get_nth(last, [<<"a">>, <<"b">>, <<"c">>], undefined)),
     ?_assertEqual(<<"a">>, jsn:get_nth(1, [<<"a">>], undefined)),
     ?_assertEqual(<<"a">>, jsn:get_nth(first, [<<"a">>], undefined)),
     ?_assertEqual(<<"a">>, jsn:get_nth(last, [<<"a">>], undefined)),
     ?_assertEqual(undefined, jsn:get_nth(0, [<<"a">>], undefined)),
     ?_assertEqual(undefined, jsn:get_nth(first, [], undefined)),
     ?_assertEqual(undefined, jsn:get_nth(last, [], undefined)),
     ?_assertEqual(undefined, jsn:get_nth(10, [], undefined)),
     ?_assertEqual(undefined, jsn:get_nth(-99, [], undefined)),
     ?_assertEqual(undefined, jsn:get_nth(q, [], undefined)),
     ?_assertEqual(undefined, jsn:get_nth(<<"a">>, [], undefined))].


path_to_binary_test_() ->
    [?_assertEqual(<<"foo.bar">>, jsn:path_to_binary(<<"foo.bar">>)),
     ?_assertEqual(<<"foo.bar">>, jsn:path_to_binary('foo.bar')),
     ?_assertEqual(<<"foo.bar">>, jsn:path_to_binary([<<"foo">>,<<"bar">>])),
     ?_assertEqual(<<"foo.bar">>, jsn:path_to_binary([<<"foo">>,<<"bar">>])),
     ?_assertEqual(<<"foo.bar">>, jsn:path_to_binary({<<"foo">>,<<"bar">>})),
     ?_assertEqual(<<"foo.9.bar">>, jsn:path_to_binary({<<"foo">>,9, <<"bar">>})),
     ?_assertEqual(<<"foo">>, jsn:path_to_binary([<<"foo">>])),
     ?_assertEqual(<<>>, jsn:path_to_binary([])),
     ?_assertEqual(<<>>, jsn:path_to_binary(<<"">>))].


binary_join_test_() ->
    [?_assertEqual(<<"foo bar">>, jsn:binary_join([<<"foo">>,<<"bar">>], <<" ">>)),
     ?_assertEqual(<<"foo">>, jsn:binary_join([<<"foo">>], <<" ">>)),
     ?_assertEqual(<<>>, jsn:binary_join([], <<" ">>))].


-endif.
