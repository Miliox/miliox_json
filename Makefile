# Author: Emiliano Carlos de Moraes Firmino @ 05/2011
SHELL=/bin/sh
.SUFFIXES:
.SUFFIXES: .beam .erl .hrl
.PHONY: clean test run #target that has no output file

#Erlang Configuration
PADIR=-pa ebin
MODULELIST=[]

run: all_test
	erl $(PADIR)

all_test: all
	cd test; erl -make

all: clean
	erl -make

clean: 
	rm -fv ebin/*
	rm -fv erl_crash.dump

test: all_test
	erl -noshell $(PADIR) \
	-eval "eunit:test($(MODULELIST),[verbose])" \
	-s init stop

