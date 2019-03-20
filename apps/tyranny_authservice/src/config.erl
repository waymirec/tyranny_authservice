-module(config).

-export([key/1]).

key(Input) ->
  Elements = application:get_all_env(tyranny_authservice),
  Parts = binary:split(Input, [<<".">>], [global]),
  find(Elements, Parts).

find(undefined, _) ->
  undefined;

find(Element, [H | T]) ->
  NextElement = proplists:get_value(binary_to_atom(H, utf8), Element),
  find(NextElement, T);

find(Element, []) ->
  Element.

