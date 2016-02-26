-module(bs04).
-export([decode_xml/1]).

decode_xml(Bin) ->
	decode_xml(Bin,[]).

decode_xml(Bin,Stack) ->
	case Bin of
		<<"</",Rest/binary>> ->
			stop_tag(Rest,<<>>,Stack);
		<<"<",Rest/binary>> ->
			start_tag(Rest,<<>>,Stack);
		<<>> -> []
	end.

stop_tag(<<">",Rest/binary>>,Acc,[Acc|Stack]) ->
	decode_xml(Rest,Stack);
stop_tag(<<X,Rest/binary>>,Acc,Stack) ->
	stop_tag(Rest,<<Acc/binary,X>>,Stack).

start_tag(<<"</",Rest/binary>>,Acc,[Tag|Stack]) ->
	[{Tag, [], [Acc]}|stop_tag(Rest,<<>>,[Tag|Stack])];
start_tag(<<"<",Rest/binary>>,<<>>,[Tag|Stack]) ->
	{Tag, [], decode_xml(<<"<",Rest/binary>>,[Tag|Stack])};
start_tag(<<">",Rest/binary>>,Acc,Stack) ->
	start_tag(Rest,<<>>,[Acc|Stack]);
start_tag(<<X,Rest/binary>>,Acc,Stack) ->
	start_tag(Rest,<<Acc/binary,X>>,Stack).
