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
-export([decode/1]).
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
	{0, []};
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
parse_string([Char|TailStream], RevString) ->
	parse_string(TailStream, [Char|RevString]);
parse_string(_,_) ->
	?ERROR_UNEXPECTED.
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
