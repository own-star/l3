-module(bs03).
-export([split/2]).

split(Bin,D) ->
	split(Bin,D,<<>>,D).

split(<<X,Bin/binary>>,[H|T],Acc,D) ->
	case X of
		H when T =:= [] -> [Acc|split(Bin,D,<<>>,D)];
		H -> split(Bin,T,Acc,D);
		X -> split(Bin,[H|T],<<Acc/binary,X>>,D)
	end;
split(<<>>,_,Acc,_) ->
	[Acc].


