# -*- python -*-

# This software was produced by NIST, an agency of the U.S. government,
# and by statute is not subject to copyright in the United States.
# Recipients of this software assume all responsibilities associated
# with its operation, modification and maintenance. However, to
# facilitate maintenance we ask that before distributing modified
# versions of this software, you first contact the authors at
# oof_manager@nist.gov. 

# Install build_shlib and install_shlib as distutils commands.  This
# code is based on similar code in py2app, which also installs a new
# distutils command.  NOTE: I didn't base this on the latest version
# of py2app.  After writing this, I noticed that later versions do it
# differently.  If there's a better and more standard way of
# installing new distutils commands, please fix this (and let me
# know)!

import sys
import distutils.dist
import distutils.command
from . import build_shlib
from distutils.command import build
from distutils.command import build_ext

# Redefine the Distribution class so that it has an shlibs list and a
# has_shared_libraries method.

# NOTE: I think this is done incorrectly, but this is the way that
# py2app does it.  The reason that it's incorrect is that the new
# class is derived from distutils.dist.Distribution but is used to
# redefine distutils.core.Distribution.  If two different distutils
# extensions try to use this mechanism, the second one will wipe out
# the first one.

class Distribution(distutils.dist.Distribution):
    def __init__(self, attrs):
        self.shlibs = []
        distutils.dist.Distribution.__init__(self, attrs)
    def has_shared_libraries(self):
        return len(self.shlibs) > 0
distutils.core.Distribution = Distribution

distutils.command.__all__.append('build_shlib')
sys.modules['distutils.command.build_shlib'] = build_shlib
setattr(distutils.command, 'build_shlib', build_shlib)

# Redefine build.build so that build_shlib and build_ext are run
# first.  The unmodified distutils runs build_py *before* build_ext,
# but if build_ext is being used to run swig, and swig is creating
# python files, then build_py must be run after build_ext.

class Build(build.build):
    def has_shared_libraries(self):
        return self.distribution.has_shared_libraries()
    oldsub = build.build.sub_commands[:]
    subs = [sub for sub in oldsub if sub[0] != 'build_ext']
    bext = [sub for sub in oldsub if sub[0] == 'build_ext']
    sub_commands = [('build_shlib', has_shared_libraries)] + bext + subs
    del subs
    del bext
    del oldsub

build.build = Build

# build_shlib requires two modifications to build_ext: It has to make
# sure that the directory containing the shared libraries is in the
# search path when building the extensions. (It's the user's
# responsibility to actually link each library by listing it in the
# Extension's "libraries" list.) It also has to make sure that the
# shared libraries are built *before* the extensions that link to
# them.

oldbuildext = build_ext.build_ext
class BuildExt(oldbuildext):
    def build_extensions(self):
        if self.distribution.has_shared_libraries():
            # Build shared libraries before extensions.
            self.run_command('build_shlib')
            # Put the shared lib. directory in the search path.
            bshlib = self.get_finalized_command('build_shlib')
            self.compiler.add_library_dir(bshlib.build_shlib)
        oldbuildext.build_extensions(self)
build_ext.build_ext = BuildExt


################

## Install 'install_shlib' as a distutils installation command

from distutils.command import install
from . import install_shlib

distutils.command.__all__.append('install_shlib')
sys.modules['distutils.command.install_shlib'] = install_shlib
setattr(distutils.command, 'install_shlib', install_shlib)

install.WINDOWS_SCHEME['shlib'] = '$base/Lib' # really, I have no idea
install.INSTALL_SCHEMES['unix_prefix']['shlib'] = '$base/lib'
install.INSTALL_SCHEMES['unix_home']['shlib'] = '$base/lib'
try:
    # This fails with Python 2.7.  Why does distutils change so often?
    install.INSTALL_SCHEMES['mac']['shlib'] = '$base/Lib'
except KeyError:
    pass
# install.INSTALL_SCHEMES['os2']['shlib'] = '$base/Lib'

# Some systems have additional install schemes.  Ubuntu 9.04 in
# particular introduces 'unix_local', and makes it the default.  But
# not everyone has it, so only modify it if it's present.
try:
    install.INSTALL_SCHEMES['unix_local']['shlib'] = '$base/lib'
except KeyError:
    pass

install.SCHEME_KEYS = install.SCHEME_KEYS + ('shlib',)

oldinstall = install.install
class Install(oldinstall):
    
    user_options = oldinstall.user_options + [
        ('install-shlib=', None, 'installation directory for shared libraries')
        ]
    def initialize_options(self):
        oldinstall.initialize_options(self)
        self.install_shlib = None
    def finalize_options(self):
        oldinstall.finalize_options(self)
        self.convert_paths('shlib')
        if self.root is not None:
            self.change_roots('shlib')

    def expand_dirs(self):
        oldinstall.expand_dirs(self)
        self._expand_attrs(['install_shlib'])

    def has_shlib(self):
        return self.distribution.has_shared_libraries()

    sub_commands = [('install_shlib', has_shlib)] + oldinstall.sub_commands

install.install = Install
