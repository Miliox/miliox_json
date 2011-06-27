%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : <OBJETIVO>
%% Criado: 06/26/11 15:13:03 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(json_tests).
-author("elmiliox@gmail.com").
-vsn(1).

-include_lib("eunit/include/eunit.hrl").
%-----------------------------------------------------------------------------
decode_test() ->
	random:seed(),

	Number = random:uniform(),
	NumberString = float_to_list(Number),

[
	%Primitive Test
	?assertEqual({ok, null}, json:decode("null")),
	?assertEqual({ok, true}, json:decode("true")),
	?assertEqual({ok, false},json:decode("false")),

	% Number Test
	?assertEqual({ok, Number}, json:decode(NumberString)),
	?assertEqual({ok, 540.0},  json:decode("54e+1")),
	?assertEqual({ok, -5.0}, json:decode("-5")),
	?assertEqual(error, json:decode("00")),
	?assertEqual(error, json:decode("0.e+3")),

	%Array Test
	?assertEqual({ok, {array, []}}, json:decode("[]")),
	?assertEqual({ok, {array, [1234.0,"teste"]}}, json:decode("[1234,\"teste\"]")),

	%Object Test
	?assertEqual({ok, {object, []}}, json:decode("{}"))
 ].
%-----------------------------------------------------------------------------
