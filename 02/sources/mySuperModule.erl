-module(mySuperModule).
-export([add/2, subtr/2, mult/2, divis/2]).
-author("Haru Atari").

add(X, Y) ->
  X + Y.

subtr(X, Y) ->
  X - Y.

mult(X, Y) ->
  X * Y.

divis(X, Y) ->
  X / Y.

