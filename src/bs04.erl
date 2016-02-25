-module(bs04).
-export([decode_xml/1]).


decode_xml(Bin) ->
	decode_xml(Bin,<<>>,[]).

decode_xml(Bin,Acc,Stack) ->
	case Bin of
		<<"</",Rest/binary>> ->
	io:format("decode stop Rest: ~p, Acc: ~p~n", [Rest,Acc]),
			stop_tag(Rest,<<>>,Stack);
		<<"<",Rest/binary>> ->
	io:format("decode start Rest: ~p, Acc: ~p~n", [Rest,Acc]),
			start_tag(Rest,<<>>,Stack);
		<<>> -> []
	end.

stop_tag(<<">",Rest/binary>>,Acc,[Acc|Stack]) ->
	decode_xml(Rest,<<>>,Stack);
stop_tag(<<X,Rest/binary>>,Acc,Stack) ->
	io:format("stop Rest: ~p, Acc: ~p~n", [Rest,Acc]),
	stop_tag(Rest,<<Acc/binary,X>>,Stack).

start_tag(<<"</",Rest/binary>>,Acc,[Tag|Stack]) ->
	{Tag, [], [Acc|stop_tag(Rest,<<>>,[Tag|Stack])]};
start_tag(<<"<",Rest/binary>>,Acc,[Tag|Stack]) ->
	{Tag, [], [Acc|decode_xml(<<"<",Rest/binary>>,<<>>,[Tag|Stack])]};
start_tag(<<">",Rest/binary>>,Acc,Stack) ->
	start_tag(Rest,<<>>,[Acc|Stack]);
start_tag(<<X,Rest/binary>>,Acc,Stack) ->
	io:format("Rest: ~p, Acc: ~p~n", [Rest,Acc]),
	start_tag(Rest,<<Acc/binary,X>>,Stack).
