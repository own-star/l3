-module(bs01).
-export([first_word/1]).


first_word(Bin) ->
	first_word(Bin,<<>>).

first_word(<<" ",_/binary>>,Acc) ->
	Acc;
first_word(<<X,Bin/binary>>,Acc) ->
	first_word(Bin,<<Acc/binary,X>>).
