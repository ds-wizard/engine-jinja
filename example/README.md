# Example Haskell application

An example Haskell application that demonstrates how to use the `engine-jinja` library. It does so as a very simple API accepting requests to render Jinja template (single or batch mode).

## Usage

### Requirements

- Stack (Haskell build tool)
- Python (3.10 or later)
- Installed dependencies in `requirements.txt` (e.g., `jinja2`)
- Compiled shared libraries as specified in [engine-jinja README](../README.md)

### Development

- `make build` - This will compile the Haskell application and link it with the `engine-jinja` shared library.
- `make test` - This will run the tests defined in the `test` directory.
- `make dist` - This will create a distributable package of the application.
- `make clean` - This will clean up the build artifacts.
- `Dockerfile` - This file is used to build a Docker image for the application, which includes all dependencies and the compiled binary (and relevant configuration of the runtime environment).

### Running the Application

- `make run` - This will start the application, which listens for requests to render Jinja templates.
- You can also use the `docker-compose.yml` file to run the application in a Docker container. Use `docker-compose up` to start the service.

### Example Request

To render a Jinja template with **single context**, you can send a POST request to the `localhost:3000/single` endpoint with the following JSON body:

```json
{
  "template": "Hello, {{ name }}!",
  "context": {
    "name": "World"
  }
}
```

To render a Jinja template in **batch (multiple contexts)**, you can send a POST request to the `localhost:3000/batch` endpoint with the following JSON body:

```json
{
  "template": "Hello, {{ name }}!",
  "contexts": [
    {"name": "Alice"},
    {"name": "Bob"},
    {"name": "Charlie"}
  ]
}
```

## License

This project is licensed under the Apache License 2.0. See the [LICENSE](LICENSE) file for details.
