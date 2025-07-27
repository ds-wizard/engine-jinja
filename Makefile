ifeq ($(shell uname), Darwin)
	LIB_EXT = dylib
else ifeq ($(shell uname), Linux)
	LIB_EXT = so
endif

.PHONY: clean
clean:
	rm -rf dist

.PHONY: lib-python
lib-python:
	mkdir -p dist
	nuitka --module src/engine_jinja.py --output-dir=dist
	rm -rf dist/engine_jinja.build dist/engine_jinja.pyi

.PHONY: lib-wrapper
lib-wrapper:
	mkdir -p dist
	gcc -fPIC -shared -o dist/libjinja.${LIB_EXT} src/wrapper.c \
	    -I$$(python3-config --includes) -L$$(python3-config --prefix)/lib \
	    -lpython$$(python3 --version | cut -d ' ' -f 2 | cut -d '.' -f 1-2)

.PHONY: libs
libs: lib-python lib-wrapper
