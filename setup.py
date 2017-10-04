# -*- coding: utf-8 -*-
# Copyright (C) 2017, Luis Pedro Coelho <luis@luispedro.org>
# vim: set ts=4 sts=4 sw=4 expandtab smartindent:
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to deal
#  in the Software without restriction, including without limitation the rights
#  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
#  all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
#  THE SOFTWARE.

from __future__ import division
try:
    import setuptools
except:
    print('''
setuptools not found.

On linux, the package is often called python-setuptools''')
    from sys import exit
    exit(1)
import os

exec(compile(open('python/diskhash/diskhash_version.py').read(),
             'python/diskhash/diskhash_version.py', 'exec'))

try:
    long_description = open('README.md', encoding='utf-8').read()
except:
    long_description = open('README.md').read()

undef_macros = []
define_macros = []
if os.environ.get('DEBUG'):
    undef_macros = ['NDEBUG']
    if os.environ.get('DEBUG') == '2':
        define_macros = [('_GLIBCXX_DEBUG','1')]


packages = setuptools.find_packages('python')


classifiers = [
'Intended Audience :: Developers',
'Topic :: Software Development :: Libraries',
'Programming Language :: Python',
'Programming Language :: Python :: 2',
'Programming Language :: Python :: 2.7',
'Programming Language :: Python :: 3',
'Programming Language :: Python :: 3.3',
'Programming Language :: Python :: 3.4',
'Programming Language :: Python :: 3.5',
'Programming Language :: Python :: 3.6',
'Operating System :: OS Independent',
'License :: OSI Approved :: MIT License',
]


setuptools.setup(name = 'diskhash',
      version = __version__,
      description = 'Disk-based hashtable',
      long_description = long_description,
      author = 'Luis Pedro Coelho',
      author_email = 'luis@luispedro.org',
      license = 'MIT',
      platforms = ['Any'],
      classifiers = classifiers,
      url = 'http://github.com/luispedro/diskhash',
      packages = packages,
      package_dir = {'':'python'},
      ext_modules = [setuptools.Extension('diskhash._diskhash', sources=['python/diskhash/_diskhash.c', 'src/diskhash.c'], depends=['src/diskhash.h'])],
      )

