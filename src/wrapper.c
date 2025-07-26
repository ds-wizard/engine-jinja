#include <Python.h>
#include <stdlib.h>
#include <string.h>

static int python_initialized = 0;

void init_python() {
    if (!python_initialized) {
        Py_Initialize();
        PyObject *sys_path = PySys_GetObject("path");
        PyList_Append(sys_path, PyUnicode_FromString("."));
        python_initialized = 1;
    }
}

void finalize_python() {
    if (python_initialized) {
        Py_Finalize();
        python_initialized = 0;
    }
}

char* render_jinja(const char* input_str) {
    init_python();

    PyObject *pModule = PyImport_ImportModule("engine_jinja");
    if (!pModule) {
        fprintf(stderr, "ERROR: Could not import engine_jinja\n");
        return NULL;
    }

    PyObject *pFunc = PyObject_GetAttrString(pModule, "render_jinja");
    if (!pFunc || !PyCallable_Check(pFunc)) {
        fprintf(stderr, "ERROR: render_jinja not callable\n");
        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
        return NULL;
    }

    PyObject *py_input_str = PyUnicode_FromString(input_str);
    PyObject *args = PyTuple_Pack(1, py_input_str);
    PyObject *result = PyObject_CallObject(pFunc, args);
    Py_DECREF(args);
    Py_DECREF(py_input_str);

    char *output = NULL;

    if (result) {
        const char *py_str = PyUnicode_AsUTF8(result);
        if (py_str) {
            output = strdup(py_str);
        } else {
            fprintf(stderr, "ERROR: PyUnicode_AsUTF8 returned NULL\n");
        }
        Py_DECREF(result);
    } else {
    	fprintf(stderr, "ERROR: Call to render_jinja failed\n");
        PyErr_Print();
    }

    Py_DECREF(pFunc);
    Py_DECREF(pModule);

    return output;
}

// Optional: C-side free for Haskell
void free_string(char* str) {
    free(str);
}
