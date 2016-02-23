-module(bs03v2).
-export([split/2]).

split(Bin,Del) ->
	split(Bin,Del,Del,<<>>).

split(<<H,Rem/binary>>,[H|T],Del,Acc) ->
	split(Rem,T,Del,Acc).
