build: all
	env -C ../cmake-build-debug/ ninja

all:
	chez --script ./expand.ss group

clean:
	chez --script ./expand.ss clean

.PHONY: all clean
