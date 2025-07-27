#include <Python.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static int python_initialized = 0;

/**
 *  Initializes the Python interpreter and sets up the environment.
 *  This function should be called before any other Python-related functions.
 */
void init_python() {
    if (!Py_IsInitialized()) {
        python_initialized = 0;
    }

    if (!python_initialized) {
        Py_Initialize();

        // Initialize thread support (only prior to Python 3.9)
        // For Python 3.9+, the GIL is automatically initialized with Py_Initialize
        // PyEval_InitThreads();

        // Setup sys.path
        PyObject *sys_path = PySys_GetObject("path");
        PyObject *path = PyUnicode_FromString(".");
        PyList_Append(sys_path, path);
        Py_DECREF(path);

        // Let Python know GIL can be switched
        PyEval_SaveThread();
        python_initialized = 1;
    }
}

/**
 *  Renders a Jinja template using the provided input string.
 *  This function initializes Python, imports the "engine_jinja" module,
 *  and calls the "render_jinja" function with the input string.
 *
 *  @param input_str The input string to be rendered by Jinja.
 *  @return A dynamically allocated string containing the rendered output,
 *          or NULL if an error occurs. The caller is responsible for freeing this string.
 */
char* render_jinja(const char* input_str) {
    // Ensure Python is initialized
    init_python();

    // Acquire GIL for thread safety
    PyGILState_STATE gstate = PyGILState_Ensure();

    // Import the Python module "engine_jinja"
    PyObject *pModule = PyImport_ImportModule("engine_jinja");
    if (!pModule) {
        fprintf(stderr, "ERROR: Could not import engine_jinja\n");
        PyErr_Print();
        PyGILState_Release(gstate);
        return NULL;
    }

    // Get the "render_jinja" function from the module
    PyObject *pFunc = PyObject_GetAttrString(pModule, "render_jinja");
    if (!pFunc || !PyCallable_Check(pFunc)) {
        fprintf(stderr, "ERROR: render_jinja not callable\n");
        PyErr_Print();
        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
        PyGILState_Release(gstate);
        return NULL;
    }

    // Convert input_str (C string) to a Python string
    PyObject *py_input_str = PyUnicode_FromString(input_str);
    if (!py_input_str) {
        fprintf(stderr, "ERROR: Could not convert input to Python string\n");
        PyErr_Print();
        Py_DECREF(pFunc);
        Py_DECREF(pModule);
        PyGILState_Release(gstate);
        return NULL;
    }

    // Prepare the arguments for the function call
    PyObject *args = PyTuple_Pack(1, py_input_str);
    if (!args) {
        fprintf(stderr, "ERROR: Failed to pack arguments\n");
        PyErr_Print();
        Py_DECREF(py_input_str);
        Py_DECREF(pFunc);
        Py_DECREF(pModule);
        PyGILState_Release(gstate);
        return NULL;
    }

    // Call the function and get the result
    PyObject *result = PyObject_CallObject(pFunc, args);
    // Clean up references (args and py_input_str)
    Py_DECREF(args);
    Py_DECREF(py_input_str);

    // Check if the result is a Python string
    char *output = NULL;
    if (result) {
        // Convert the result to a C string
        const char *py_str = PyUnicode_AsUTF8(result);
        if (py_str) {
            // Allocate memory for the output string and copy the content
            // Note: strdup allocates memory, which should be freed later
            // This allows us to return a dynamically allocated string
            output = strdup(py_str);
        } else {
            fprintf(stderr, "ERROR: PyUnicode_AsUTF8 returned NULL\n");
            PyErr_Print();
        }
        // Clean up the result reference
        Py_DECREF(result);
    } else {
        fprintf(stderr, "ERROR: Call to render_jinja failed\n");
        PyErr_Print();
    }

    // Clean up references (function and module)
    Py_DECREF(pFunc);
    Py_DECREF(pModule);

    // Release the GIL
    PyGILState_Release(gstate);

    return output;
}

/**
 *  Frees a dynamically allocated string.
 *  This function should be called to free the memory allocated for the output string
 *  returned by render_jinja.
 *
 *  @param str The string to be freed.
 */
void free_string(char* str) {
    free(str);
}
