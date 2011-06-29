%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : <OBJETIVO>
%% Criado: 06/26/11 15:11:48 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(json).
-author("elmiliox@gmail.com").
-vsn(1).

-define(NULL,  $n).
-define(TRUE,  $t).
-define(FALSE, $f).
%-----------------------------------------------------------------------------
-define(STRING_START, $\").
-define(STRING_END,   $\").
-define(REV_SOLIDUS, $\\).
-define(REV_REPLACE_CHAR, [189,191,239]).
%-----------------------------------------------------------------------------
-define(ARRAY_START, $[).
-define(ARRAY_END,   $]).
-define(ARRAY_SEP,   $,).
-define(ARRAY_FMT(Array), {array, Array}).
-define(ARRAY_EMPTY(TailStream), [?ARRAY_START|[?ARRAY_END|TailStream]]).
%-----------------------------------------------------------------------------
-define(OBJ_START, ${).
-define(OBJ_END,   $}).
-define(OBJ_SEP,   $,).
-define(OBJ_KV_SEP, $:).
-define(OBJ_FMT(Object), {object, Object}).
-define(OBJ_EMPTY(TailStream), [?OBJ_START|[?OBJ_END|TailStream]]).
%-----------------------------------------------------------------------------
-define(NEW_OBJ, []).
-define(STORE_OBJ(Key, Value, Obj), orddict:store(Key, Value, Obj)).
%-----------------------------------------------------------------------------
-define(EXP, $E).
-define(exp, $e).
-define(DOT, $.).
-define(ZERO, $0).
-define(ONE,  $1).
-define(NINE, $9).
-define(PLUS, $+).
-define(MINUS, $-).
%-----------------------------------------------------------------------------
-define(NULL_MATCH(Tail), [$n|[$u|[$l|[$l|Tail]]]]).
-define(TRUE_MATCH(Tail), [$t|[$r|[$u|[$e|Tail]]]]).
-define(FALSE_MATCH(Tail), [$f|[$a|[$l|[$s|[$e|Tail]]]]]).
%-----------------------------------------------------------------------------
-define(ERROR_ENCODED, erlang:throw(invalid_encoded)).
-define(ERROR_UNEXPECTED, erlang:throw(char_unexpected)).
%-----------------------------------------------------------------------------
-define(HACK_EXP(ReverseNumber), [?EXP|[?ZERO|[?DOT|ReverseNumber]]]).
-define(ZERO_FRAC, [?DOT, ?ZERO]).
%-----------------------------------------------------------------------------
-export([decode/1, encode/1]).
%-----------------------------------------------------------------------------
decode(Stream) when is_binary(Stream) ->
	decode(binary_to_list(Stream));
decode(Stream) when is_list(Stream) ->
	case catch(decode_partial(Stream)) of
		{Decoded, []} ->
			{ok, Decoded};
		_Error ->
			error
	end;
decode(_) ->
	error.
%-----------------------------------------------------------------------------
decode_partial([Char|_TailStream]=Stream) ->
	case Char of
		?NULL ->
			parse_null(Stream);
		?TRUE ->
			parse_true(Stream);
		?FALSE ->
			parse_false(Stream);
		Number when 
			Number >= ?ZERO andalso
			Number =< ?NINE ->
				parse_number(Stream);
		?MINUS ->
			parse_number(Stream);
		?STRING_START ->
			parse_string(Stream);
		?ARRAY_START ->
			parse_array(Stream);
		?OBJ_START ->
			parse_obj(Stream);
		_Invalid ->
			?ERROR_ENCODED
	end.
%-----------------------------------------------------------------------------
parse_null(?NULL_MATCH(Tail)) ->
	{null, Tail};
parse_null(_) ->
	?ERROR_UNEXPECTED.
%-----------------------------------------------------------------------------
parse_true(?TRUE_MATCH(Tail)) ->
	{true, Tail};
parse_true(_) ->
	?ERROR_UNEXPECTED.
%-----------------------------------------------------------------------------
parse_false(?FALSE_MATCH(Tail)) ->
	{false, Tail};
parse_false(_) ->
	?ERROR_UNEXPECTED.
%-----------------------------------------------------------------------------
parse_number([Operator|TailStream]=Stream) ->
	case Operator of
		?MINUS ->
			{Absolute, Tail} = parse_number_1(TailStream),
			Number = -Absolute,

			{Number, Tail};
		_->
			parse_number_1(Stream)
	end;
parse_number(_) ->
	?ERROR_ENCODED.
%-----------------------------------------------------------------------------
parse_number_1([Char|TailStream]) ->
	case Char of
		?ZERO ->
			parse_number_2(TailStream);
		Digit when
			Digit >= ?ONE andalso
			Digit =< ?NINE ->
				parse_number_digit(TailStream, [Digit]);
		_ ->
			?ERROR_ENCODED
	end;
parse_number_1(_) ->
	?ERROR_UNEXPECTED.
%-----------------------------------------------------------------------------
parse_number_2([]) ->
	{0.0, []};
parse_number_2([Char|TailStream]) ->
	case Char of
		?DOT ->
			parse_number_frac(TailStream, ?ZERO_FRAC);
		E when E == ?EXP orelse E == ?exp ->
			parse_number_exp(TailStream, ?HACK_EXP([?ZERO]));
		_ ->
			?ERROR_UNEXPECTED
	end;
parse_number_2(_) ->
	?ERROR_UNEXPECTED.
%-----------------------------------------------------------------------------
parse_number_digit([Char|TailStream], RevNumber) ->
	case Char of
		Digit when
			Digit >= ?ZERO andalso
			Digit =< ?NINE ->
				parse_number_digit(TailStream, [Digit|RevNumber]);
		?DOT ->
			parse_number_frac(TailStream, [?DOT|RevNumber]);
		E when E == ?EXP orelse E == ?exp ->
			parse_number_exp(TailStream, ?HACK_EXP(RevNumber));
		_ ->
			Number = to_int(RevNumber),
			Stream = [Char|TailStream],

			{Number, Stream}
	end;
parse_number_digit([], RevNumber) ->
	Number = to_int(RevNumber),

	{Number, []}.
%-----------------------------------------------------------------------------
parse_number_frac([], RevNumber) ->
	Number = to_float(RevNumber),
	
	{Number, []};
parse_number_frac([Char|TailStream], RevNumber) ->
	case Char of
		Digit when
			Digit >= ?ZERO andalso
			Digit =< ?NINE ->
				parse_number_frac(TailStream, [Digit|RevNumber]);
		E when E == ?EXP orelse E == ?exp ->
			parse_number_exp(TailStream, [?EXP|RevNumber]);
		_ ->
			io:format([Char]),
			Number = to_float(RevNumber),
			Stream = [Char|TailStream],

			{Number, Stream}
	end;
parse_number_frac(_, _) ->
	erlang:erro(badarg).
%-----------------------------------------------------------------------------
parse_number_exp([Char|TailStream], RevNumber) ->
	case Char of
		?MINUS ->
			parse_number_exp_1(TailStream, [?MINUS|RevNumber]);
		?PLUS ->
			parse_number_exp_1(TailStream, [?PLUS|RevNumber]);
		Digit when
			Digit >= ?ZERO andalso
			Digit =< ?NINE ->
				parse_number_exp_1(TailStream, [Digit|RevNumber]);
		_ ->
			?ERROR_ENCODED
	end;
parse_number_exp(_, _) ->
	?ERROR_UNEXPECTED.
%-----------------------------------------------------------------------------
parse_number_exp_1([], RevNumber) ->
	Number = to_float(RevNumber),
	{Number, []};
parse_number_exp_1([Char|TailStream], RevNumber) ->
	case Char of
		Digit when
			Digit >= ?ZERO andalso
			Digit =< ?NINE ->
				parse_number_exp_1(TailStream, [Digit|RevNumber]);
		_ ->
			Number = to_float(RevNumber),
			Stream = [Char|TailStream],

			{Number, Stream}
	end;
parse_number_exp_1(_, _) ->
	?ERROR_UNEXPECTED.
%-----------------------------------------------------------------------------
to_float(RevArray) ->
	list_to_float(lists:reverse(RevArray)).
to_int(RevArray) ->
	RevFloat = [?ZERO|[?DOT|RevArray]],
	to_float(RevFloat).
%-----------------------------------------------------------------------------
parse_string([?STRING_START|TailStream]) ->
	parse_string(TailStream, []);
parse_string(_) ->
	?ERROR_UNEXPECTED.
%-----------------------------------------------------------------------------
parse_string([?STRING_END|TailStream],RevString) ->
	String = lists:reverse(RevString),

	{String, TailStream};
parse_string([?REV_SOLIDUS|Stream], RevString) ->
	[EscapedChar|TailStream] = Stream,
	case EscapedChar of
		% Escaped Character that are used as Control Char
		$"  -> parse_string(TailStream,  [$"|RevString]);
		$/  -> parse_string(TailStream,  [$/|RevString]);
		$\\ -> parse_string(TailStream, [$\\|RevString]);

		% Valid Control Char
		$b -> parse_string(TailStream,  [$\b|RevString]);
		$f -> parse_string(TailStream,  [$\f|RevString]);
		$n -> parse_string(TailStream,  [$\n|RevString]);
		$r -> parse_string(TailStream,  [$\r|RevString]);
		$t -> parse_string(TailStream,  [$\t|RevString]);

		% Unicode Character Escape
		$u -> 
			{RevUnicoded, Tail} = parse_string_unicode(TailStream, 0, 4),
			parse_string(Tail, RevUnicoded ++ RevString)
	end;
parse_string([Char|TailStream], RevString) ->
	parse_string(TailStream, [Char|RevString]);
parse_string(_,_) ->
	?ERROR_UNEXPECTED.
%-----------------------------------------------------------------------------
parse_string_unicode(TailStream, CodeValue, Len) when Len =< 0 ->
	% Warning: Low Level Operation ahead
	% Please! visit www.fileformat.info/info/unicode/utf8.html
	String = case CodeValue of
		ASCII when ASCII > 0 andalso ASCII < 16#80 ->
			[ASCII];
		U07FF when U07FF >= 16#80 andalso U07FF < 16#800 ->
			<<H:5,L:6>> = <<CodeValue:11>>, % 11Bits

			First  = 2#11000000 bor H, % 110xxxxx (5 Higher Bits, 7~11)
			Second = 2#10000000 bor L, % 10xxxxxx (6 Lower  Bits, 1~06)

			[Second, First];
		UFFFF when UFFFF >= 16#800 andalso UFFFF < 16#ffff ->
			<<H:4,M:6,L:6>> = <<CodeValue:16>>, % 16Bits

			First  = 2#11100000 bor H, %1110xxxx (4 Higher Bits, 13~16)
			Second = 2#10000000 bor M, %10xxxxxx (Middle,  Bits 07~12)
			Third  = 2#10000000 bor L, %10xxxxxx (6 Lower Bits, 01~06)

			[Third, Second, First];
		_ ->
			?ERROR_UNEXPECTED
	end,
	{String, TailStream};
parse_string_unicode([Hex|TailStream], OldCodeValue, Len) ->
	Value = case Hex of
		Decimal when Decimal >= $0 andalso Decimal =< $9 ->
			Hex - $0;
		UpHex when UpHex >= $A andalso UpHex =< $F ->
			(Hex - $A) + 10;
		LowHex when LowHex >= $a andalso LowHex =< $f ->
			(Hex - $a) + 10;
		_ ->
			?ERROR_UNEXPECTED
	end,
	NewCodeValue = (OldCodeValue * 16) + Value,

	parse_string_unicode(TailStream, NewCodeValue, Len-1).
%-----------------------------------------------------------------------------
parse_array(?ARRAY_EMPTY(TailStream)) ->
	{?ARRAY_FMT([]), TailStream};
parse_array([?ARRAY_START|TailStream]) ->
	parse_array(TailStream, []);
parse_array(_) ->
	?ERROR_UNEXPECTED.
%-----------------------------------------------------------------------------
parse_array(Stream, RevArray) ->
	{Item, TailStream} = decode_partial(Stream),
	parse_array_sep(TailStream, [Item|RevArray]).
%-----------------------------------------------------------------------------
parse_array_sep([?ARRAY_SEP|TailStream], RevArray) ->
	parse_array(TailStream, RevArray);
parse_array_sep([?ARRAY_END|TailStream], RevArray) ->
	Array = lists:reverse(RevArray),
	{?ARRAY_FMT(Array), TailStream};
parse_array_sep(_,_) ->
	?ERROR_UNEXPECTED.
%-----------------------------------------------------------------------------
parse_obj(?OBJ_EMPTY(TailStream)) ->
	{?OBJ_FMT(?NEW_OBJ), TailStream};
parse_obj([?OBJ_START|TailStream]) ->
	parse_obj(TailStream, ?NEW_OBJ);
parse_obj(_) ->
	?ERROR_ENCODED.
%-----------------------------------------------------------------------------
parse_obj(Stream, Object) ->
	{Key, KeyTailStream} = parse_string(Stream),

	[?OBJ_KV_SEP|ValueStream] = KeyTailStream,

	{Value, TailStream} = decode_partial(ValueStream),

	parse_obj_sep(TailStream, ?STORE_OBJ(Key,Value, Object)).
%-----------------------------------------------------------------------------
parse_obj_sep([?OBJ_SEP|TailStream], Object) ->
	parse_obj(TailStream, Object);
parse_obj_sep([?OBJ_END|TailStream], Object) ->
	{?OBJ_FMT(Object), TailStream};
parse_obj_sep(_,_) ->
	?ERROR_ENCODED.
%-----------------------------------------------------------------------------
encode(Element) ->
	case catch(encode_partial(Element)) of
		Decoded when is_list(Decoded) ->
			{ok, Decoded};
		{'EXIT', {Reason, _}} ->
			{error, Reason};
		_Error ->
			{error, badformat}
	end.
%-----------------------------------------------------------------------------
encode_partial(Element) ->
	case Element of
		true ->
			"true";
		false ->
			"false";
		null ->
			"null";
		String when is_list(String) ->
			encode_string(String);
		ByteString when is_binary(ByteString) ->
			encode_string(ByteString);
		Number when is_number(Number) ->
			encode_number(Number);
		?ARRAY_FMT(Array) ->
			encode_array(Array);
		?OBJ_FMT(Object) ->
			encode_object(Object);
		_ ->
			erlang:error(badarg)
	end.
%-----------------------------------------------------------------------------
encode_number(Number) ->
	Float = Number / 1.0,
	float_to_list(Float).
%-----------------------------------------------------------------------------
encode_string(ByteString) when is_binary(ByteString) ->
	encode_string(binary_to_list(ByteString));
encode_string(String) when is_list(String) ->
	encode_string(String, [?STRING_START]);
encode_string(_) ->
	erlang:error(badarg).
%-----------------------------------------------------------------------------
encode_string([], RevString) ->
	lists:reverse([?STRING_END|RevString]);
encode_string([Char|TailString], RevString) ->
	case Char of
		$"  -> encode_string(TailString,  "\"\\" ++ RevString);
		$/  -> encode_string(TailString,  "/\\"  ++ RevString);
		$\\ -> encode_string(TailString,  "\\\\" ++ RevString);

		$\b -> encode_string(TailString,  "b\\" ++ RevString);
		$\f -> encode_string(TailString,  "f\\" ++ RevString);
		$\n -> encode_string(TailString,  "n\\" ++ RevString);
		$\r -> encode_string(TailString,  "r\\" ++ RevString);
		$\t -> encode_string(TailString,  "t\\" ++ RevString);
		
		ControlChar when 
			ControlChar >= 0 andalso 
			ControlChar < 32 ->
				erlang:error(controlchar);
		16#ff ->
			encode_string(TailString, 
				?REV_REPLACE_CHAR ++ RevString);
		Character when 
			Character >= 32 andalso 
			Character < 256 ->
				encode_string(TailString, [Character|RevString]);
		_ ->
			erlang:error(badarg)
	end.
%-----------------------------------------------------------------------------
encode_array([]) ->
	"[]";
encode_array([Item|Array]) ->
	JsonItem = encode_array_item(Item),
	encode_array(
		Array, 
		lists:reverse([?ARRAY_START|JsonItem])
	);
encode_array(_) ->
	erlang:error(badarg).
%-----------------------------------------------------------------------------
encode_array([], RevJson) ->
	lists:reverse([?ARRAY_END|RevJson]);
encode_array([Item|Array], RevJson) ->
	JsonItem = encode_array_item(Item),
	encode_array(
		Array, 
		lists:reverse([?ARRAY_SEP|JsonItem],RevJson)
	);
encode_array(_,_) ->
	erlang:error(badarg).
%-----------------------------------------------------------------------------
encode_array_item(Item) ->
	encode_partial(Item).
%-----------------------------------------------------------------------------
encode_object([]) ->
	"{}";
encode_object([{Key,Value}|Object]) ->
	JsonElement = encode_object_element(Key, Value),
	encode_object(
		Object, 
		lists:reverse([?OBJ_START|JsonElement])
	);
encode_object(_) ->
	erlang:error(badarg).
%-----------------------------------------------------------------------------
encode_object([], RevJson) ->
	lists:reverse([?OBJ_END|RevJson]);
encode_object([{Key,Value}|Object], RevJson) ->
	JsonElement = encode_object_element(Key, Value),

	encode_object(
		Object, 
		lists:reverse([?OBJ_SEP|JsonElement],RevJson)
	);
encode_object(_, _) ->
	erlang:error(badarg).
%-----------------------------------------------------------------------------
encode_object_element(Key, Value) ->
	JsonKey = encode_string(Key),
	JsonValue = encode_partial(Value),

	JsonKey ++ ?OBJ_KV_SEP ++ JsonValue.
%-----------------------------------------------------------------------------
