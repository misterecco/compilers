SOURCE = ./src
ALL = insc_jvm insc_llvm

all: common $(ALL) clean

$(ALL): 
	cp $(SOURCE)/dist/build/$@/$@ $@

common:
	make -C $(SOURCE)
	cd $(SOURCE) && cabal configure && cabal build && cd ..

clean:
	make -C $(SOURCE) distclean
	rm -rf $(SOURCE)/dist

mrproper: clean
	rm $(ALL)
