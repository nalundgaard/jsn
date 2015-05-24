%%%-----------------------------------------------------------------------------
%% @doc jsn.hrl 
%%
%% @author Nicholas Lundgaard <nlundgaard@alertlogic.com>
%%%-----------------------------------------------------------------------------
-ifndef(json_hrl).
-define(json_hrl, 1).

%%=============================================================================
%% jsn types
%%=============================================================================

-type json_string()      :: binary().
-type json_key()         :: json_string() | atom().
-type json_number()      :: integer() | float().
-type json_boolean()     :: true | false.
-type json_null()        :: null.
-type json_array()       :: [json_term()].
-type json_array_index() :: first | last | pos_integer().
-type json_proplist()    :: [{json_key(), json_term()}].
-type json_eep18()       :: {json_proplist()}.
-type json_struct()      :: {struct, json_proplist()}.
-type json_object()      :: json_proplist() | json_eep18() | json_struct().

-type json_term()        :: json_string() | json_number() | json_array() |
                            json_object() | json_null() | json_boolean().


%% JSN OPTIONS
%%
%% JSN options are passed to the new/2 function when building jsn objects
%% from scratch. Currently, the only option is format, which can be either:
%% 
%% * proplist (default)
%% * eep18 (a.k.a EJSON) 
%% * struct (mochijson2 format)
%%
-type format()      :: proplist | eep18 | struct.
-type jsn_option()  :: {format, format()}.
-type jsn_options() :: [ jsn_option() ].

%% A path is a either a list of json keys (representing nesting from left
%% to right), a tuple of json keys and/or json array indexes (also nested
%% left to right), or a single period-delimited binary/atom, where periods
%% indicate nesting of keys; period-delimited binary/atom values are mapped
%% into the list representation; as such, the list of json keys is the most
%% performant. Note that json array index values can ONLY be passed using
%% tuple paths.
-type path() :: binary() | atom() | 
                [ json_key() ] | 
                tuple().  %% tuple containing binary() | json_array_index()

-type path_value_tuple() :: { path(),  Value :: json_term() }.
-type path_value_tuples() :: [ path_value_tuple() ].

-type path_element() ::  binary() | json_array_index().
-type path_elements() :: [ path_element() ].

%%=============================================================================
%% jsn constants
%%=============================================================================

-define(EMPTY_PROPLIST, []).

-define(EMPTY_EEP18, {[]}).

-define(EMPTY_STRUCT, {struct, []}).

-endif.