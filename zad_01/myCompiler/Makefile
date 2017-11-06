SOURCE = ./src
ALL = insc_llvm

all: $(ALL)

$(ALL): 
	make -C $(SOURCE)
	cd $(SOURCE) && cabal build && cd ..
	make -C $(SOURCE) distclean
	cp $(SOURCE)/dist/build/$@/$@ $@
	rm -rf $(SOURCE)/dist

clean:
	make -C $(SOURCE) distclean
	rm $(ALL)
