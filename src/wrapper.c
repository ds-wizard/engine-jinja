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
    char* output = NULL;

    init_python();
    PyObject *sys_path = PySys_GetObject("path");
    PyList_Append(sys_path, PyUnicode_FromString("."));

    PyObject *pModule = PyImport_ImportModule("wizardjinja");
    if (pModule) {
        PyObject *pFunc = PyObject_GetAttrString(pModule, "render_jinja");
        if (pFunc && PyCallable_Check(pFunc)) {
            PyObject *py_input_str = PyUnicode_FromString(input_str);
            PyObject *args = PyTuple_Pack(1, py_input_str);
            PyObject *result = PyObject_CallObject(pFunc, args);
            Py_DECREF(args);

            if (result) {
                const char* py_str = PyUnicode_AsUTF8(result);
                if (py_str) {
                    output = strdup(py_str);  // Allocates memory for Haskell to free
                }
                Py_DECREF(result);
            }
        }
        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
    }

    // Py_Finalize();
    return output;
}

// Optional: C-side free for Haskell
void free_string(char* str) {
    free(str);
}
