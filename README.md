# engine-jinja

Jinja rendering engine for use in Wizard projects

## Local Build

1. Install dependencies:

```sh
pip install jinja2 nuitka
```

2. Build the library from Python source:

```sh
nuitka --module --output-dir=dist src/engine_jinja.py
```

3. Build wrapper with C API for FFI:

```sh
gcc -shared -o dist/libjinja.so -fPIC -I$(python3-config --includes) -L$(python3-config --prefix)/lib -lpython3.13 lib/wrapper.c
```

Here you need to know the Python version you are using, in this case it is Python 3.13. Also, on MacOS you may want to use .dylib instead of .so.

## Runtime

To use the library, you need to:

1. Have Python runtime installed (matching the version used to build the library).
2. Install the dependencies (`pip install jinja2`).
3. Link to the libraries correctly (using `LD_LIBRARY_PATH` or similar environment variables).

## License

This project is licensed under the Apache License 2.0. See the [LICENSE](LICENSE) file for details.
