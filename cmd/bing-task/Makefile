
default: dist

.PHONY: dist build clean

dist:
	stack install --pedantic --local-bin-path=build

build:
	stack build --fast --no-copy-bins

clean:
	stack clean && rm -rf build
