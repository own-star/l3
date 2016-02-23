-module(bs03v1).
-export([split/2]).

split(Bin,Del) ->
	BDel = list_to_binary(Del),
	S = size(BDel),
	split(Bin,BDel,S,<<>>).

split(Bin,BDel,S,Acc) ->
	case Bin of
		<<BDel:S/binary,Remain/binary>> ->
			[Acc|split(Remain,BDel,S,<<>>)];
		<<X,Remain/binary>> ->
			split(Remain,BDel,S,<<Acc/binary,X>>);
		<<>> -> [Acc]
	end.
