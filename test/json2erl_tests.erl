%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : <OBJETIVO>
%% Criado: 06/26/11 15:13:03 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(json2erl_tests).
-author("elmiliox@gmail.com").
-vsn(1).

-include_lib("eunit/include/eunit.hrl").
%-----------------------------------------------------------------------------
decode_test() ->
[
	?assertEqual({ok, null}, json2erl:decode("null")),
	?assertEqual({ok, true}, json2erl:decode("true")),
	?assertEqual({ok, false},json2erl:decode("false")),
	?assertEqual({ok, 10000.0}, json2erl:decode("10000")),
	?assertEqual({ok, 1200.0}, json2erl:decode("1.2e+3")),
	?assertEqual({ok, -150.4}, json2erl:decode("-150.4e0")),
	?assertEqual({ok, 10000.0}, json2erl:decode("10000")),

	?assertEqual({ok, -5.0}, json2erl:decode("-5"))
 ].
%-----------------------------------------------------------------------------
