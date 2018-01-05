ALL = ex

all: compile

asm: $(ALL).s

%.s : %.c
	gcc -S $< -o $@ -Wall -O3

compile: asm
	gcc $(ALL).s ../lib/builtins.s -o $(ALL) -O3 -Wall

clean:
	rm $(ALL)