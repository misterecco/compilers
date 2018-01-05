ALL = builtins.s

all: copy

asm: $(ALL)

%.s : %.c
	gcc -S $< -o $@ -Wall -O3

copy: asm
	cp $(ALL) ../lib