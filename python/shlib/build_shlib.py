# -*- python -*-

# This software was produced by NIST, an agency of the U.S. government,
# and by statute is not subject to copyright in the United States.
# Recipients of this software assume all responsibilities associated
# with its operation, modification and maintenance. However, to
# facilitate maintenance we ask that before distributing modified
# versions of this software, you first contact the authors at
# oof_manager@nist.gov.

# Portions of this file were copied from
# distutils/command/build_clib.py and build_ext.py in the Python
# distribution.

# ----------------

## This file defines a distutils command, build_shlib, which builds
## shared libraries which are NOT importable python extension modules.
## It is expected that one or more extensions will link to the
## libraries.

## The build_shlib command is installed as a distutils command by
## importing the file setup_shlib.py.

## build_shlib is based on build_clib and build_ext, but is not
## derived from either one, because almost all of the main functions
## require some modification.  Lots of code is copied from both of
## them, though.

## build_shlib is invoked by calling distutils.core.setup with a
## "shlibs" argument.  shlibs is a list of SharedLibrary objects.  The
## arguments to the SharedLibrary constructor are:
##    'name'             the name of the library to be constructed, without
##                       the 'lib' prefix or any suffix.
##    'sources'          a list of C and C++ source files
##    'macros'           a list of macros to define
##    'include_dirs'     a list of directories to search for header files
##    'extra_compile_args' a list of extra arguments to pass to the compiler
##    'libraries'        a list of libraries to link to
##    'library_dirs'     a list of directories to search for those libraries
##    'extra_link_args'  a list of extra arguments to pass to the linker
##    'language'         either 'c++' or 'c', probably
## All arguments are optional, except for 'name' and 'sources'.

## Command line arguments to build_shlib apply to all shared
## libraries.  The arguments are listed in the usual manner by running
## "python setup.py --help build_shlib".

from distutils.core import Command
from distutils.errors import DistutilsSetupError
from distutils.sysconfig import get_config_vars
from distutils.sysconfig import customize_compiler
from distutils import log
import sys, os, string
from types import *

try:
    import warnings
except ImportError:
    warnings = None

class SharedLibrary:
    # analogous to (and copied from) the distutils.extension.Extension class
    def __init__(self, name, sources,
                 include_dirs=None,
                 macros=None,
                 extra_compile_args=None,
                 libraries=None,
                 library_dirs=None,
                 extra_link_args=None,
                 language=None,
                 **kwargs):

        self.name = name
        self.sources = sources
        self.include_dirs = include_dirs or []
        self.macros = macros or []
        self.extra_compile_args = extra_compile_args or []
        self.library_dirs = library_dirs or []
        self.libraries = libraries or []
        self.extra_link_args = extra_link_args or []
        self.language = language

        if len(kwargs):
            L = list(kwargs.keys())
            L.sort()
            L = map(repr, L)
            msg = "Unknown SharedLibrary options: " + ', '.join(L)
            if warnings is not None:
                warnings.warn(msg)
            else:
                sys.stderr.write(msg+'\n')
            

def show_compilers ():
    from distutils.ccompiler import show_compilers
    show_compilers()

class build_shlib(Command):
    description = "build C/C++ shared libraries used by Python extensions"
    sep_by = " (separated by '%s')" % os.pathsep
    user_options = [
        ('build-temp', 't', "directory to put temporary build by-products"),
        ('debug', 'g', "compile with debugging information"),
        ('force', 'f', "forcibly build everything (ignore file timestamps)"),
        ('compiler=', 'c', "specify the compiler type"),
        ('build-shlib=', None, 'library installation directory'),
        ('define=', 'D', "C preprocessor macros to define"),
        ('undef=', 'U', "C preprocessor macros to undefine"),
        ('include-dirs=', 'I',
         'directories to search for header files' + sep_by),
        ('libraries=', 'l',
         'external libraries to link with (eg "blas lapack")'),
        ('library-dirs=', 'L',
         "directories to search for external libraries" + sep_by),
        ('link-objects=', 'O', "extra explicit link objects to link with")
        ]
    boolean_options = ['debug', 'force']

    help_options = [
        ('help-compiler', None, "list available compilers", show_compilers),
        ]

    
    def initialize_options(self):
        self.build_temp = None
        self.build_shlib = None         # destination dir
        self.shlibs = None              # list of libraries to build
        self.define = None              # macros to define
        self.undef = None               # macros to undefine
        self.include_dirs = None        # header file search path
        self.libraries = None           # external libraries to link to
        self.library_dirs = None        # search path for external libraries
        self.link_objects = None
        self.debug = None
        self.force = 0
        self.compiler = None

    def finalize_options(self):
        self.set_undefined_options('build',
                                   ('build_temp', 'build_temp'),
                                   ('compiler', 'compiler'),
                                   ('debug', 'debug'),
                                   ('force', 'force'))

        # Much of the following is copied from build_ext
        if self.include_dirs is None:
            self.include_dirs = self.distribution.include_dirs or []
        if type(self.include_dirs) == str:
            self.include_dirs = string.split(self.include_dirs, os.pathsep)

        if self.libraries is None:
            self.libraries = []
        elif type(self.libraries) == str:
            self.libraries = self.libraries.split(' ')

        if self.library_dirs is None:
            self.library_dirs = []
        elif type(self.library_dirs) == str:
            self.library_dirs = string.split(self.library_dirs, os.pathsep)

        # The argument parsing will result in self.define being a string, but
        # it has to be a list of 2-tuples.  All the preprocessor symbols
        # specified by the 'define' option will be set to '1'.  Multiple
        # symbols can be separated with commas.

        if self.define:
            defines = string.split(self.define, ',')
            self.define = map(lambda symbol: (symbol, '1'), defines)

        # The option for macros to undefine is also a string from the
        # option parsing, but has to be a list.  Multiple symbols can also
        # be separated with commas here.
        if self.undef:
            self.undef = string.split(self.undef, ',')


        if self.build_shlib is None:
            self.build_shlib = os.path.join(self.build_temp, 'shlib')
        self.shlibs = self.distribution.shlibs

        if self.shlibs:
            self.check_library_list(self.shlibs)

    def run(self):
        # Copied from distutils/command/build_clib.py, except for the
        # darwin part.
        if not self.shlibs:
            return

        from distutils.ccompiler import new_compiler
        self.compiler = new_compiler(compiler=self.compiler,
                                     dry_run=self.dry_run,
                                     force=self.force)

        if sys.platform == "darwin":
            customize_compiler_darwin(self.compiler)
        else:
            # NOTE: This calls the sysconfig verison of
            # customize_compiler which is the right thing to do on
            # Linux and SGI. I haven't tested Windows or any other OS.
            customize_compiler(self.compiler)

        if self.debug:
            self.compiler.undefine_macro('NDEBUG')

        if self.include_dirs is not None:
            self.compiler.set_include_dirs(self.include_dirs)
        if self.define is not None:
            # 'define' option is a list of (name,value) tuples
            for (name,value) in self.define:
                self.compiler.define_macro(name, value)
        if self.undef is not None:
            for macro in self.undef:
                self.compiler.undefine_macro(macro)
        if self.libraries is not None:
            self.compiler.set_libraries(self.libraries)
        if self.library_dirs is not None:
            self.compiler.set_library_dirs(self.library_dirs)
        if self.link_objects is not None:
            self.compiler.set_library_dirs(self.link_objects)
                
        # Add the output directory to the library search path, since
        # if more than one shared lib is being built, it's likely that
        # the later ones will link to the earlier ones.
        self.compiler.add_library_dir(self.build_shlib)
        
        self.build_libraries(self.shlibs)

    def build_libraries (self, shlibs):
        for shlib in shlibs:
            sources = shlib.sources
            if sources is None or type(sources) not in (list, tuple):
                raise DistutilsSetupError("In SharedLibrary %s, 'sources' must be a list of file names" % shlib.name)
            sources = list(sources)

            log.info("building '%s' library", shlib.name)

            # First, compile the source code to object files in the library
            # directory.  (This should probably change to putting object
            # files in a temporary build directory.)
            language = shlib.language or self.compiler.detect_language(sources)

#            print "build_shlib.py: sources=", sources

            objects = self.compiler.compile(sources,
                                            output_dir=self.build_temp,
                                            macros=shlib.macros,
                                            include_dirs=shlib.include_dirs,
                                            extra_postargs=shlib.extra_compile_args,
                                            debug=self.debug)

#            print "build_shlib.py: objects=", objects
            
            self.compiler.link(
                target_desc=self.compiler.SHARED_LIBRARY,
                objects=objects,
                output_filename="lib"+
                    self.compiler.shared_object_filename(shlib.name),
                output_dir=self.build_shlib,
                libraries=shlib.libraries,
                library_dirs=shlib.library_dirs,
                extra_preargs=shlib.extra_compile_args,
                extra_postargs=shlib.extra_link_args,
                debug=self.debug,
                target_lang=language
                )

    def get_source_files(self):
        ## NOTE: This function is only used by sdist, as far as I can
        ## tell.  build_shlib only knows the source files that are
        ## explicitly compiled.  It doesn't know about C and C++
        ## header files, so there's no way that it can create a
        ## complete list for sdist.  Users will just have to provide a
        ## manifest file some other way.
        raise DistutilsSetupError("build_shlib requires a MANIFEST or MANIFEST.in file.")

    def check_library_list(self, shlibs):
        if type(shlibs) != list:
            raise DistutilsSetupError( "'shlibs' option must be a list of SharedLibrary objects")

        for shlib in shlibs:
            if not isinstance(shlib, SharedLibrary):
                raise DistutilsSetupError( "'shlibs' option must be a list of SharedLibrary objects")

##################

# sysconfig.customize_compiler sets flags for building a .so on
# darwin, but we want a .dylib.  This function is a hacked version of
# that function.

def customize_compiler_darwin(compiler):
    import sys
    (cc, cxx, cflags, opt, ccshared, ldshared, ldcxxshared) = \
        get_config_vars('CC', 'CXX', 'CFLAGS', 'OPT',
                        'CCSHARED', 'LDSHARED', 'LDCXXSHARED')
    if os.environ.has_key('CC'):
        cc = os.environ['CC']
    if os.environ.has_key('CXX'):
        cxx = os.environ['CXX']
    if os.environ.has_key('LDSHARED'):
        ldshared = os.environ['LDSHARED']
    if os.environ.has_key('LDCXXSHARED'):
        ldcxxshared = os.environ['LDCXXSHARED']
    if os.environ.has_key('CPP'):
        cpp = os.environ['CPP']
    else:
        cpp = cc + " -E"

    if ldcxxshared is None:
        ldcxxshared = ""

    if os.environ.has_key('LDFLAGS'):
        ldshared = ldshared + ' ' + os.environ['LDFLAGS']
        ldcxxshared = ldcxxshared + ' ' + os.environ['LDFLAGS']
    if os.environ.has_key('CFLAGS'):
        cflags = opt + ' ' + os.environ['CFLAGS']
        ldshared = ldshared + ' ' + os.environ['CFLAGS']
        ldcxxshared = ldcxxshared + ' ' + os.environ['CFLAGS']
    if os.environ.has_key('CPPFLAGS'):
        cpp = cpp + ' ' + os.environ['CPPFLAGS']
        cflags = cflags + ' ' + os.environ['CPPFLAGS']
        ldshared = ldshared + ' ' + os.environ['CPPFLAGS']
        ldcxxshared = ldcxxshared + ' ' + os.environ['CPPFLAGS']

    ldshared = ldshared.replace("-bundle", "-dynamiclib")
    ldcxxshared = ldcxxshared.replace("-bundle", "-dynamiclib")
    # Hack for python 2.6, or maybe just fink's 2.6? 
    if "-undefined" not in ldshared:
        ldshared = ldshared + " -undefined dynamic_lookup"
        ldcxxshared = ldcxxshared + " -undefined dynamic_lookup"

    # NOTE: This is correct for gcc 3.3 on OS X 10.4, at least.  10.1
    # (and 10.2?) require -undefined suppress instead.  I'm not sure
    # how to test for the os version number.

    ## ldflags ISN'T USED BY PYTHON 2.5 OR 2.6.  WTF?

    ## TODO: Find out how to pass these options into the linker with
    ## 2.5 or 2.6 distutils.  If python isn't built with -undefined
    ## dynamic_lookup, linking the swig .so files will fail.  

    ## TODO: The absence of these ldflags from the link arguments
    ## doesn't seem to cause a problem with python 2.6.  I don't know
    ## if it causes a problem with python 2.5, because they're present
    ## in the LDSHARED configuration option in fink's 2.5.  Is ldflags
    ## needed anymore?  Is -undefined dynamic_lookup not necessary
    ## with modern compilers?
#    ldflags = "-dynamiclib -undefined dynamic_lookup -headerpad_max_install_names"
    
#     if os.environ.has_key('LDFLAGS'):
#         ldflags = ldflags + ' ' + os.environ['LDFLAGS']
#     if basecflags:
#         opt = basecflags + ' ' + opt
#     if os.environ.has_key('CFLAGS'):
#         opt = opt + ' ' + os.environ['CFLAGS']
#         ldflags = ldflags + ' ' + os.environ['CFLAGS']
#     if os.environ.has_key('CPPFLAGS'):
#         cpp = cpp + ' ' + os.environ['CPPFLAGS']
#         opt = opt + ' ' + os.environ['CPPFLAGS']
#         ldflags = ldflags + '  ' + os.environ['CPPFLAGS']

#     cc_cmd = cc + ' ' + opt

    cc_cmd = cc + ' ' + cflags
    if not 'clang' in cc_cmd:
        cc_cmd = cc_cmd + ' -dynamiclib '

    compiler.set_executables(
        preprocessor = cpp,
        compiler = cc_cmd,
        compiler_cxx = cxx,
        compiler_so = cc_cmd + ccshared,
        linker_so = ldshared,
        )

    # Some systems, or at least one, have a distutils that uses
    # "compiler_so_cxx" and "linker_so_cxx" instead of "compiler_so"
    # and "linker_so" for C++ files.  On those systems, and only those
    # systems, we need to set those variables.  This is a hack to do
    # that.  It's a hack because it relies on internal details of the
    # CCompiler class, namely that the executable programs are stored
    # in a dict called "executables" and individual ones can be set by
    # set_executable().
    if "compiler_so_cxx" in compiler.executables:
        compiler.set_executable("compiler_so_cxx", cc_cmd + ccshared)
    if "linker_so_cxx" in compiler.executables:
        compiler.set_executable("linker_so_cxx", ldcxxshared)

    compiler.shared_lib_extension = ".dylib"
