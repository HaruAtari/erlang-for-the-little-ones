-module(chapter03).
-export([greet/2, first/1, second/1, same/2]).
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
