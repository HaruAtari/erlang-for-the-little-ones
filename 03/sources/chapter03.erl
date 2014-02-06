-module(chapter03).
-export([
    greet/2, first/1, 
    second/1, 
    same/2, 
    bmi_tell/1, 
    lucky_number/1, 
    lucky_atom/1,
    safe_division/2
]).
-author("Haru Atari").

greet(male, Name) ->
    io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
    io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s!", [Name]).

first([X|_])->
    X.

second([_,X|_]) ->
    X.

same(X,X) -> 
    true;
same(_,_) ->
    false.

bmi_tell(Bmi) when Bmi =< 18.5 ->
    "You're underweight.";
bmi_tell(Bmi) when Bmi =< 25 ->
   "You're supposedly normal.";
bmi_tell(Bmi) when Bmi =< 30 ->
   "You're fat.";
bmi_tell(_) ->
    "You're very fat.".

lucky_number(X) when 10 < X, X < 20 ->
    true;
lucky_number(_) ->
    false.

lucky_atom(X) when X == atom1; X == atom2 ->
    true;
lucky_atom(_) ->
    false.

safe_division(X, Y) when is_number(X), is_number(Y), Y /= 0 ->
    X / Y;
safe_division(_, _) ->
    false.
