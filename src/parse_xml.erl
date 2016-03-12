-module(parse_xml).
-export([decode_xml/1]).

decode_xml(Path) ->
	{ok, Bin} = file:read_file(Path),
	trim(Bin,<<>>).

decode_xml(Bin,Stack,AStack) ->
	case Bin of
		<<" ",Rest/binary>> ->
			decode_xml(Rest,Stack,AStack);
		<<"</",Rest/binary>> ->
			stop_tag(Rest,<<>>,Stack,AStack);
		<<"<?",Rest/binary>> ->
			start_prolog(Rest,Stack,AStack);
		<<"<!--",Rest/binary>> ->
			start_comment(Rest,Stack,AStack);
		<<"<!",Rest/binary>> ->
			start_doctype(Rest,Stack,AStack);
		<<"<",Rest/binary>> ->
			start_tag(Rest,<<>>,Stack,AStack);
		<<>> -> []
	end.

stop_tag(<<">",Rest/binary>>,Acc,[Acc|Stack],AStack) ->
	decode_xml(Rest,Stack,AStack);
stop_tag(<<X,Rest/binary>>,Acc,Stack,AStack) ->
	stop_tag(Rest,<<Acc/binary,X>>,Stack,AStack).

start_tag(<<" ",Rest/binary>>,Acc,Stack,[open|_]=AStack) ->
	start_tag(Rest,<<Acc/binary," ">>,Stack,AStack);
start_tag(<<" ",Rest/binary>>,Acc,Stack,AStack) ->
	start_attr(Rest,Acc,Stack,AStack,<<>>);
start_tag(<<"/>",Rest/binary>>,Acc,Stack,AStack) ->
	[{Acc, [], []}|decode_xml(Rest,Stack,AStack)];
start_tag(<<"</",Rest/binary>>,Acc,[Tag|Stack],[_|AStack]) ->
	[{Tag, get_attr(Tag,AStack,[]), [Acc]}|stop_tag(Rest,<<>>,[Tag|Stack],del_stack(Tag,AStack))];
start_tag(<<"<",Rest/binary>>,<<>>,[Tag|[]],[_|AStack]) ->
	{Tag, get_attr(Tag,AStack,[]), decode_xml(<<"<",Rest/binary>>,[Tag],del_stack(Tag,AStack))};
start_tag(<<"<",Rest/binary>>,<<>>,[Tag|Stack],[_|AStack]) ->
	[{Tag, get_attr(Tag,AStack,[]), decode_xml(<<"<",Rest/binary>>,[Tag|Stack],del_stack(Tag,AStack))}];
start_tag(<<"> ",Rest/binary>>,Acc,Stack,AStack) ->
	start_tag(<<">",Rest/binary>>,Acc,Stack,AStack);
start_tag(<<">",Rest/binary>>,Acc,Stack,AStack) ->
	start_tag(Rest,<<>>,[Acc|Stack],[open|AStack]);
start_tag(<<X,Rest/binary>>,Acc,Stack,AStack) ->
	start_tag(Rest,<<Acc/binary,X>>,Stack,AStack).

start_prolog(<<"?>",Rest/binary>>,Stack,AStack) ->
	decode_xml(Rest,Stack,AStack);
start_prolog(<<_,Rest/binary>>,Stack,AStack) ->
	start_prolog(Rest,Stack,AStack).

start_doctype(<<">",Rest/binary>>,Stack,AStack) ->
	decode_xml(Rest,Stack,AStack);
start_doctype(<<_,Rest/binary>>,Stack,AStack) ->
	start_doctype(Rest,Stack,AStack).

start_comment(<<"-->",Rest/binary>>,Stack,AStack) ->
	decode_xml(Rest,Stack,AStack);
start_comment(<<_,Rest/binary>>,Stack,AStack) ->
	start_comment(Rest,Stack,AStack).

start_attr(<<" />",Rest/binary>>,Tag,Stack,AStack,_) ->
	[{Tag, get_attr(Tag,AStack,[]), []}|decode_xml(Rest,Stack,del_stack(Tag,AStack))];
start_attr(<<"/>",Rest/binary>>,Tag,Stack,AStack,_) ->
	[{Tag, get_attr(Tag,AStack,[]), []}|decode_xml(Rest,Stack,del_stack(Tag,AStack))];
start_attr(<<">",_/binary>>=Rest,Tag,Stack,AStack,_) ->
	start_tag(Rest,Tag,Stack,AStack);
start_attr(<<" ",Rest/binary>>,Tag,[single|_]=Stack,AStack,Acc) ->
	start_attr(Rest,Tag,Stack,AStack,<<Acc/binary," ">>);
start_attr(<<" ",Rest/binary>>,Tag,[double|_]=Stack,AStack,Acc) ->
	start_attr(Rest,Tag,Stack,AStack,<<Acc/binary," ">>);
start_attr(<<"'",Rest/binary>>,Tag,[double|_]=Stack,AStack,Acc) ->
	start_attr(Rest,Tag,Stack,AStack,<<Acc/binary,"'">>);
start_attr(<<" ",Rest/binary>>,Tag,Stack,AStack,Acc) ->
	start_attr(Rest,Tag,Stack,AStack,Acc);

start_attr(<<"'",Rest/binary>>,Tag,Stack,AStack,<<>>) ->   %%Match single quote start attribute value
	start_attr(Rest,Tag,[single|Stack],AStack,<<>>);
start_attr(<<"'",Rest/binary>>,Tag,[_|Stack],[H|AStack],Acc) ->   %Single quote stop attr value
	start_attr(Rest,Tag,Stack,[{Tag, H, Acc}|AStack],<<>>);

start_attr(<<34,Rest/binary>>,Tag,[double|Stack],[H|AStack],Acc) -> %%Stop doubel quoted value
	start_attr(Rest,Tag,Stack,[{Tag, H, Acc}|AStack],<<>>);
start_attr(<<34,Rest/binary>>,Tag,Stack,AStack,<<>>) ->    %%Match double quote start value
	start_attr(Rest,Tag,[double|Stack],AStack,<<>>);
start_attr(<<34,Rest/binary>>,Tag,[single|_]=Stack,AStack,Acc) ->  %%Siingle quoted double quote 
	start_attr(Rest,Tag,Stack,AStack,<<Acc/binary,34>>);

start_attr(<<"=",Rest/binary>>,Tag,Stack,AStack,Acc) ->      
	start_attr(Rest,Tag,Stack,[Acc|AStack],<<>>);
start_attr(<<X,Rest/binary>>,Tag,Stack,AStack,Acc) ->
	start_attr(Rest,Tag,Stack,AStack,<<Acc/binary,X>>).

get_attr(Tag,[{Tag,Key,Val}|Stack],Acc) ->
	get_attr(Tag,Stack,[{Key,Val}|Acc]);
get_attr(_,_,Acc) ->
	Acc.

del_stack(Tag,[{Tag,_,_}|Stack]) ->
	del_stack(Tag,Stack);
del_stack(Tag,[open|Stack]) ->
	del_stack(Tag,Stack);
del_stack(_,Stack) ->
	Stack.

trim(<<9,Rest/binary>>,Acc) ->
	trim(Rest,Acc);
trim(<<10,Rest/binary>>,Acc) ->
	trim(Rest,Acc);
trim(<<13,Rest/binary>>,Acc) ->
	trim(Rest,Acc);
trim(<<X,Rest/binary>>,Acc) ->
	trim(Rest,<<Acc/binary,X>>);
trim(<<>>,Acc) ->
	decode_xml(Acc,[],[]).
