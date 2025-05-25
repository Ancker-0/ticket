all:
	chez --script ./expand.ss group

clean:
	chez --script ./expand.ss clean

build: all
	env -C ../cmake-build-debug/ ninja

.PHONY: all clean
