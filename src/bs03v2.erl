-module(bs03v2).
-export([split/2]).

split(Bin,Del) ->
	split(Bin,Del,Del,<<>>).

split(<<H,Rem/binary>>,[H|[]],Del,Acc) ->
	[Acc|split(Rem,Del,Del,<<>>)];
split(<<H,Rem/binary>>,[H|T],Del,Acc) ->
	split(Rem,T,Del,Acc);
split(<<X,Rem/binary>>,Del,Del,Acc) ->
	split(Rem,Del,Del,<<Acc/binary,X>>);
split(<<>>,_,_,Acc) ->
	[Acc].
