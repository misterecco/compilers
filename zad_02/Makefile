SOURCE = ./src
ARCH = x86_64
ALL = latc_$(ARCH)

all: common $(ALL) clean

$(ALL): 
	cp $(SOURCE)/dist/build/$@/$@ $@

common:
	# make -C $(SOURCE)
	cd $(SOURCE) && cabal configure && cabal build && cd ..

clean:
	# make -C $(SOURCE) distclean
	rm -rf $(SOURCE)/dist

delete_execs:
	find . -type f -executable -delete

mrproper: clean
	rm $(ALL)
