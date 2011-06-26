%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : <OBJETIVO>
%% Criado: 06/26/11 15:11:48 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(json2erl).
-author("elmiliox@gmail.com").
-vsn(1).

-define(NULL,  $n).
-define(TRUE,  $t).
-define(FALSE, $f).
-define(STRING, $\").
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
		{_, _} ->
			{error, incomplete_parse};
		_Error ->
			{error, badarg}
	end;
decode(_) ->
	{error, invalid_stream}.
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
		?STRING ->
			parse_string(Stream);
		_TODO ->
			erlang:error(todo)
	end.
%-----------------------------------------------------------------------------
parse_null(?NULL_MATCH(Tail)) ->
	{null, Tail};
parse_null(_) ->
	erlang:error(badelement).
%-----------------------------------------------------------------------------
parse_true(?TRUE_MATCH(Tail)) ->
	{true, Tail};
parse_true(_) ->
	erlang:error(badelement).
%-----------------------------------------------------------------------------
parse_false(?FALSE_MATCH(Tail)) ->
	{false, Tail};
parse_false(_) ->
	erlang:error(badelement).
%-----------------------------------------------------------------------------
parse_number([Operator|TailStream]=Stream) ->
	case Operator of
		?MINUS ->
			{Number, Tail} = parse_number_1(TailStream),
			{-Number, Tail};
		_NotOperator ->
			parse_number_1(Stream)
	end;
parse_number(_) ->
	erlang:error(badelement).
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
			erlang:error(badarg)
	end;
parse_number_1(_) ->
	erlang:error(badarg).
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
			erlang:error(badarg)
	end;
parse_number_2(_) ->
	erlang:error(badarg).
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
			erlang:error(badarg)
	end;
parse_number_exp(_, _) ->
	erlang:error(badarg).
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
	erlang:error(badarg).
%-----------------------------------------------------------------------------
to_float(RevList) ->
	list_to_float(lists:reverse(RevList)).
to_int(RevList) ->
	RevFloat = [?ZERO|[?DOT|RevList]],
	to_float(RevFloat).
%-----------------------------------------------------------------------------
parse_string([?STRING|TailStream]) ->
	parse_string_1(TailStream, []);
parse_string(_) ->
	erlang:error(badarg).
%-----------------------------------------------------------------------------
parse_string_1([?STRING|TailStream],RevString) ->
	String = lists:reverse(RevString),

	{String, TailStream};
parse_string_1([Char|TailStream], RevString) ->
	parse_string_1(TailStream, [Char|RevString]);
parse_string_1(_,_) ->
	erlang:error(badarg).
