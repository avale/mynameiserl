ERLC=erlc
ERLCFLAGS=-o
ERL=erl

ERL_FlAGS= -noshell -s

ERL_FILES= src/*.erl
BEAM_FILES= bin/*.beam

BEAM_DIR= /bin
ERL_DIR= /src

all: clean compile

compile: 
	$(ERLC) $(ERLCFLAGS) $(BEAM_DIR) $(ERL_FILES)

clean:
	rm -f $(BEAM_FILES)
	rm -f $(BEAM_DIR)*.dump

