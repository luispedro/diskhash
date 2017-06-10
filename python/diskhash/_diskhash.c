// Copyright (C) 2017  Luis Pedro Coelho <luis@luispedro.org>
//
// License: MIT (see COPYING file)

#include <Python.h>

#include "../../src/diskhash.h"

typedef struct {
    PyObject_HEAD
    HashTable* ht;
} htObject;


PyObject* htLookup(htObject* self, PyObject* args) {
    const char* k;
    if (!PyArg_ParseTuple(args, "s", &k)) {
        return NULL;
    }
    void* data = dht_lookup(self->ht, k);
    if (!data) {
        Py_RETURN_NONE;
    }
    long long r;
    memcpy(&r, data, sizeof(r));
    return PyLong_FromLong(r);
}
PyObject* htReserve(htObject* self, PyObject* args) {
    int cap;
    if (!PyArg_ParseTuple(args, "i", &cap)) {
        return NULL;
    }
    long r = dht_reserve(self->ht, cap);
    if (r == 0) {
        const char* err = dht_geterror();
        if (!err) err = "Error in dht_reserve.";
        PyErr_SetString(PyExc_RuntimeError, err);
        return NULL;
    }
    return PyLong_FromLong(r);
}

PyObject* htInsert(htObject* self, PyObject* args) {
    const char* k;
    long v;
    if (!PyArg_ParseTuple(args, "sl", &k, &v)) {
        return NULL;
    }
    int r = dht_insert(self->ht, k, &v);
    if (r == -1) {
        const char* err = dht_geterror();
        if (!err) err = "Error in dht_insert.";
        PyErr_SetString(PyExc_RuntimeError, err);
        return NULL;
    }
    return PyLong_FromLong(r);
}

PyObject* htLen(htObject* self, PyObject* args) {
    long n = dht_size(self->ht);
    return PyLong_FromLong(n);
}



static PyMethodDef htMethods[] = {
    { "lookup", (PyCFunction)htLookup, METH_VARARGS, "" },
    { "reserve", (PyCFunction)htReserve, METH_VARARGS, "" },
    { "insert", (PyCFunction)htInsert, METH_VARARGS, "" },
    { "size", (PyCFunction)htLen, METH_VARARGS, "" },
    {NULL}  /* Sentinel */
};



static PyObject *
htNew(PyTypeObject *type, PyObject * args, PyObject * kwargs) {
    htObject *self;

    self = (htObject *)type->tp_alloc(type, 0);
    if (self != NULL) {
        self->ht = 0;
    }

    return (PyObject *)self;
}

static int
htInit(htObject *self, PyObject *args, PyObject *kwds) {
    const char* fpath;
    const char* mode;
    int maxi;
    if (!PyArg_ParseTuple(args, "sis", &fpath, &maxi, &mode)) {
        return -1;
    }

    HashTableOpts opts;
    opts.key_maxlen = maxi;
    opts.object_datalen = 8;

    self->ht = dht_open(fpath, opts, 66);

    if (!self->ht) {
        const char* err = dht_geterror();
        if (!err) err = "Error in dht_open.";
        PyErr_SetString(PyExc_RuntimeError, err);
        return -1;
    }
    return 0;
}


static void
htDealloc(htObject* ht) {
    if (ht->ht) dht_free(ht->ht);
}


static PyTypeObject htWrapperType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "diskhash.Str2int",        /* tp_name */
    sizeof(htObject),          /* tp_basicsize */
    0,                         /* tp_itemsize */
    htDealloc,                 /* tp_dealloc */
    0,                         /* tp_print */
    0,                         /* tp_getattr */
    0,                         /* tp_setattr */
    0,                         /* tp_reserved */
    0,                         /* tp_repr */
    0,                         /* tp_as_number */
    0,                         /* tp_as_sequence */
    0,                         /* tp_as_mapping */
    0,                         /* tp_hash  */
    0,                         /* tp_call */
    0,                         /* tp_str */
    0,                         /* tp_getattro */
    0,                         /* tp_setattro */
    0,                         /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT |
        Py_TPFLAGS_BASETYPE,   /* tp_flags */
    "Disk based str -> int mapping", /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,                         /* tp_richcompare */
    0,                         /* tp_weaklistoffset */
    0,                         /* tp_iter */
    0,                         /* tp_iternext */
    htMethods,                 /* tp_methods */
    0,                         /* tp_members */
    0,                         /* tp_getset */
    0,                         /* tp_base */
    0,                         /* tp_dict */
    0,                         /* tp_descr_get */
    0,                         /* tp_descr_set */
    0,                         /* tp_dictoffset */
    (initproc)htInit,          /* tp_init */
    0,                         /* tp_alloc */
    htNew,                     /* tp_new */
};

static PyModuleDef pydiskhash = {
    PyModuleDef_HEAD_INIT,
    "diskhash",
    "",
    -1,
    NULL, NULL, NULL, NULL, NULL
};

PyMODINIT_FUNC
PyInit__diskhash(void)
{
    PyObject* m;

    htWrapperType.tp_new = PyType_GenericNew;
    if (PyType_Ready(&htWrapperType) < 0)
        return NULL;

    m = PyModule_Create(&pydiskhash);
    if (m == NULL)
        return NULL;

    Py_INCREF(&htWrapperType);
    PyModule_AddObject(m, "Str2int", (PyObject *)&htWrapperType);
    return m;
}



