ALL = runtime.o

all: copy

asm: $(ALL)

%.s : %.o
	gcc $< -o $@ -Wall -O3

copy: asm
	cp $(ALL) ../lib