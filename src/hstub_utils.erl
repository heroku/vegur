-module(hstub_utils).

-export([get_interface_module/1
         ,parse_header/2
         ,add_or_append_header/4
         ,add_if_missing_header/4
         ,add_or_replace_header/3]).

-spec get_interface_module(Config) -> Module | no_return() when
      Config :: [{atom(), term()}]|[],
      Module :: module().
get_interface_module(Config) ->
    case proplists:get_value(interface_module, Config) of
        undefined ->
            error(missing_hstub_interface_module);
        Module when is_atom(Module) ->
            Module
    end.

-spec parse_header(binary(), cowboy_req:req()) ->
                          {[]|[binary()|undefined], cowboy_req:req()}.
parse_header(Key, Req) ->
    case cowboy_req:parse_header(Key, Req) of
        {ok, L, Req0} when is_list(L) -> {L, Req0};
        {ok, Term, Req0} -> {[Term], Req0};
        {undefined, L, Req0} when is_list(L) -> {L, Req0};
        {undefined, _, Req0} -> {[], Req0}
    end.

-spec add_or_append_header(Key, Value, Headers, Req) ->
                                  {Headers, Req} when
      Key :: iodata(),
      Value :: iodata(),
      Headers :: [{iodata(), iodata()}]|[],
      Req :: cowboy_req:req().
add_or_append_header(Key, Val, Headers, Req) ->
    case cowboy_req:header(Key, Req) of
        {undefined, Req2} ->
            {Headers ++ [{Key, Val}], Req2};
        {CurrentVal, Req2} ->
            {lists:keyreplace(Key, 1, Headers, {Key, [CurrentVal, ", ", Val]}),
             Req2}
    end.

-spec add_if_missing_header(Key, Value, Headers, Req) ->
                                   {Headers, Req} when
      Key :: iodata(),
      Value :: iodata(),
      Headers :: [{iodata(), iodata()}]|[],
      Req :: cowboy_req:req().
add_if_missing_header(Key, Val, Headers, Req) ->
    {NewVal, Req2} = 
        case cowboy_req:header(Key, Req) of
            {undefined, Req1} ->
                {Val, Req1};
            {CurrentVal, Req1} ->
                {CurrentVal, Req1}
        end,
    {Headers ++ [{Key, NewVal}], Req2}.

-spec add_or_replace_header(Key, Value, Headers) ->
                                    Headers when
      Key :: iodata(),
      Value :: iodata(),
      Headers :: [{iodata(), iodata()}]|[].
add_or_replace_header(Key, Value, Headers) ->
    lists:keystore(Key, 1, Headers, {Key, Value}).
