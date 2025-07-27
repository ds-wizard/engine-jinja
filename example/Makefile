ifeq ($(shell uname), Darwin)
	LIB_EXT = dylib
else ifeq ($(shell uname), Linux)
	LIB_EXT = so
endif

.PHONY: clean
clean:
	rm -rf ./lib
	rm -rf ./dist
	stack clean

.PHONY: copy-libs
copy-libs:
	mkdir -p ./lib
	cp ../dist/engine_jinja$$(python3-config --extension-suffix) ./lib
	cp ../dist/libjinja.${LIB_EXT} ./lib

.PHONY: build
build: copy-libs
	PYTHONPATH=$$(pwd)/lib:$$PYTHONPATH \
	LD_LIBRARY_PATH=$$(pwd)/lib:$$LD_LIBRARY_PATH \
	DYLD_LIBRARY_PATH=$$(pwd)/lib:$$DYLD_LIBRARY_PATH \
	stack build

.PHONY: test
test: copy-libs
	PYTHONPATH=$$(pwd)/lib:$$PYTHONPATH \
	LD_LIBRARY_PATH=$$(pwd)/lib:$$LD_LIBRARY_PATH \
	DYLD_LIBRARY_PATH=$$(pwd)/lib:$$DYLD_LIBRARY_PATH \
	stack test

.PHONY: run
run: build
	PYTHONPATH=$$(pwd)/lib:$$PYTHONPATH \
	LD_LIBRARY_PATH=$$(pwd)/lib:$$LD_LIBRARY_PATH \
	DYLD_LIBRARY_PATH=$$(pwd)/lib:$$DYLD_LIBRARY_PATH \
	stack run

.PHONY: dist
dist: build
	mkdir -p ./dist
	cp -r ./lib ./dist
	cp $$(stack path --local-install-root)/bin/engine-jinja-example dist/engine-jinja-example
