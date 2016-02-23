-module(bs01v1).
-export([first_word/1]).

first_word(<<" ",_/binary>>) ->
	"End".
