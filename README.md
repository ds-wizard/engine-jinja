# engine-jinja

Jinja rendering engine for use in Wizard projects

## Local Build

1. Install dependencies:

```sh
pip install -r requirements.txt
pip install nuitka
```

2. Build the library from Python source:

```sh
make lib-python
```

3. Build wrapper with C API for FFI:

```sh
make lip-wrapper
```

Here you need to know the Python version you are using, in this case it is Python 3.13. Also, on MacOS you may want to use .dylib instead of .so.

## Runtime

To use the library, you need to:

1. Have Python runtime installed (matching the version used to build the library).
2. Copy the built shared library (`.so` and alt. `.dylib` files) to your project directory.
3. Link to the libraries correctly (using `LD_LIBRARY_PATH` or similar environment variables).

## License

This project is licensed under the Apache License 2.0. See the [LICENSE](LICENSE) file for details.

We bundle [Jinja](https://github.com/pallets/jinja) and [MarkupSafe](https://github.com/pallets/markupsafe) dependencies as part of the resulting shared library for convenience. See [NOTICE](NOTICE.md) for more details on the licenses of these components.
