#!/usr/bin/python

import ctypes
from ctypes import *
from pyosaf.saAis import BYREF
from pyosaf.saEnumConst import Enumeration, Const

lib = cdll.LoadLibrary('./libarray.so')

SaInt8T = ctypes.c_char
SaUint16T = ctypes.c_ushort

saAis = Const()
saAis.SA_MAX_NAME_LENGTH = 256

class SaNameT(Structure):
	"""Contain names.
	"""
	_fields_ = [('length', SaUint16T),
		    ('value', (SaInt8T*saAis.SA_MAX_NAME_LENGTH))]

	def __init__(self, name=''):
		"""Construct instance with contents of 'name'.
		"""
		super(SaNameT, self).__init__(len(name), name)

lib.foo.argtypes = [POINTER(SaNameT)]
lib.foo.restype = c_int

name = SaNameT("hello")

lib.foo(BYREF(name))
