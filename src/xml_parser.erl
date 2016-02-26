-module(xml_parser).
-export([decode_xml/1]).

decode_xml(Bin) ->
	decode_xml(Bin,[],[]).

decode_xml(Bin,Stack,AStack) ->
	case Bin of
		<<"</",Rest/binary>> ->
			stop_tag(Rest,<<>>,Stack,AStack);
		<<"<!--",Rest/binary>> ->
			start_comment(Rest,Stack,AStack);
		<<"<",Rest/binary>> ->
			start_tag(Rest,<<>>,Stack,AStack);
		<<>> -> []
	end.

stop_tag(<<">",Rest/binary>>,Acc,[Acc|Stack],AStack) ->
	decode_xml(Rest,Stack,AStack);
stop_tag(<<X,Rest/binary>>,Acc,Stack,AStack) ->
	stop_tag(Rest,<<Acc/binary,X>>,Stack,AStack).

start_tag(<<"</",Rest/binary>>,Acc,[Tag|Stack],AStack) ->
	[{Tag, get_attr(Tag,AStack), [Acc]}|stop_tag(Rest,<<>>,[Tag|Stack],AStack)];
start_tag(<<"<",Rest/binary>>,<<>>,[Tag|Stack],AStack) ->
	{Tag, get_attr(Tag,AStack), decode_xml(<<"<",Rest/binary>>,[Tag|Stack],AStack)};
start_tag(<<" />",Rest/binary>>,Acc,Stack,AStack) ->
	[{Acc, get_attr(Acc,AStack), []}|decode_xml(Rest,Stack,AStack)];
start_tag(<<" ",Rest/binary>>,Acc,Stack,AStack) ->
	start_attr(Rest,Acc,Stack,AStack,<<>>);
start_tag(<<">",Rest/binary>>,Acc,Stack,AStack) ->
	start_tag(Rest,<<>>,[Acc|Stack],AStack);
start_tag(<<X,Rest/binary>>,Acc,Stack,AStack) ->
	start_tag(Rest,<<Acc/binary,X>>,Stack,AStack).

start_comment(<<"-->",Rest/binary>>,Stack,AStack) ->
	decode_xml(Rest,Stack,AStack);
start_comment(<<_,Rest/binary>>,Stack,AStack) ->
	start_comment(Rest,Stack,AStack).

start_attr(<<" />",_/binary>>=Rest,Tag,Stack,AStack,_) ->
	start_tag(Rest,Tag,Stack,AStack);
start_attr(<<">",_/binary>>=Rest,Tag,Stack,AStack,_) ->
	start_tag(Rest,Tag,Stack,AStack);
start_attr(<<" ",Rest/binary>>,Tag,[open|_]=Stack,AStack,Acc) ->
	start_attr(Rest,Tag,Stack,AStack,<<Acc/binary," ">>);
start_attr(<<" ",Rest/binary>>,Tag,Stack,AStack,Acc) ->
	start_attr(Rest,Tag,Stack,AStack,Acc);
start_attr(<<"'",Rest/binary>>,Tag,[_|Stack],[H|AStack],Acc) ->
	start_attr(Rest,Tag,Stack,[{Tag, H, Acc}|AStack],<<>>);
start_attr(<<"='",Rest/binary>>,Tag,Stack,AStack,Acc) ->
	start_attr(Rest,Tag,[open|Stack],[Acc|AStack],<<>>);
start_attr(<<X,Rest/binary>>,Tag,Stack,AStack,Acc) ->
	start_attr(Rest,Tag,Stack,AStack,<<Acc/binary,X>>).

get_attr(Tag,Stack) ->
	[{Key, Val} || {T, Key, Val} <- Stack, T =:= Tag].
