-module(bs03v1).
-export([split/2]).

split(Bin,D) ->
	Bd = list_to_binary(D),
	S = size(Bd),
	split(Bin,Bd,S,<<>>).

split(Bin,Bd,S,Acc) ->
	case Bin of
		<<Bd:S/binary,Remain/binary>> ->
			[Acc|split(Remain,Bd,S,<<>>)];
		<<X,Remain/binary>> ->
			split(Remain,Bd,S,<<Acc/binary,X>>);
		<<>> -> [Acc]
	end.
