default: build

.PHONY: build clean

build:
	stack build --fast --no-copy-bins

clean:
	stack clean
