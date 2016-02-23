-module(bs02).
-export([words/1]).

words(Bin) ->
	words(Bin,<<>>).

words(<<>>,Acc) ->
	[Acc];
words(<<" ",Bin/binary>>,Acc) ->
	[Acc|words(Bin,<<>>)];
words(<<X,Bin/binary>>,Acc) ->
	words(Bin,<<Acc/binary,X>>).
