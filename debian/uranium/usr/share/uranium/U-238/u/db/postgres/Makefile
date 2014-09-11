prog=postgres

all: $(prog)

INCPG=/usr/include/postgresql
LIBPL=/usr/lib/swi-prolog/include

GCCFLAGS=-I$(LIBPL) -fpic -c
LDFLAGS=-g -I$(INCPG) -I$(LIBPL) -c -fpic

.SUFFIXES: .c

$(prog): $(prog).c
	gcc $(LDFLAGS) $(prog).c
	gcc -L$(INCPG) -lecpg -lpq -shared -o $(prog).so $(prog).o
clean:
	rm -f $(prog).so $(prog).o

