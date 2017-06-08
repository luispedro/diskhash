# -*- python -*-

# This software was produced by NIST, an agency of the U.S. government,
# and by statute is not subject to copyright in the United States.
# Recipients of this software assume all responsibilities associated
# with its operation, modification and maintenance. However, to
# facilitate maintenance we ask that before distributing modified
# versions of this software, you first contact the authors at
# oof_manager@nist.gov.

## Distutils command to install the shared libraries built by the
## build_shlib command.  Most of this code was cribbed from the other
## install_* commands.  Perhaps install_shlib should be derived from
## one of them.

## install_shlib is inserted into the distutils command structure by
## running setup_shlib.py.

import os
import sys
from distutils.core import Command
from distutils.util import convert_path
from distutils import log
from distutils.errors import DistutilsExecError

class install_shlib(Command):
    description = "install shared libs used by extension modules"

    user_options = [
        ('install-dir=', 'd', "directory to install to"),
        ('build-dir=', 'b', "build directory (where to install from)"),
        ('skip-build', None, "skip the build steps")
        ]
    boolean_options = ['skip-build']

    def initialize_options(self):
        self.install_dir = None
        self.build_dir = None
        self.outfiles = []
        self.shlibs = self.distribution.shlibs
        self.skip_build = None

    def finalize_options(self):
        self.set_undefined_options('install',
                                   ('install_shlib', 'install_dir'),
                                   ('skip_build', 'skip_build'))
        self.set_undefined_options('build_shlib',
                                   ('build_shlib', 'build_dir'))

    def run(self):
        self.build()
        self.install()
        
    def build(self):
        if not self.skip_build:
            if self.distribution.has_shared_libraries():
                self.run_command('build_shlib')

    def install(self):
        if os.path.isdir(self.build_dir):
            outfiles = self.copy_tree(self.build_dir, self.install_dir)
            ## On OS X, we have to run install_name_tool here, since
            ## dylibs contain info about their own location and the
            ## locations of the libraries they link to.  The
            ## alternative is to force users to set DYLD_LIBRARY_PATH.
            ## Neither should be necessary if the installation is in a
            ## standard location.
            if sys.platform == "darwin":
                for ofile in outfiles:
                    name = os.path.split(ofile)[1]
                    cmd = "install_name_tool -id %(of)s %(of)s" % dict(of=ofile)
                    log.info(cmd)
                    errorcode = os.system(cmd)
                    if errorcode:
                        raise DistutilsExecError("command failed: %s" % cmd)
                    # See what other dylibs it links to.  If they're
                    # ours, then we have to make sure they link to the
                    # final location.
                    f = os.popen('otool -L %s' % ofile)
                    for line in f.readlines():
                        l = line.lstrip()
                        if l.startswith("build"): # it's one of ours
                            dylib = l.split()[0] # full path in build dir
                            dylibname = os.path.split(dylib)[1]
                            cmd = 'install_name_tool -change %s %s %s' % (
                                dylib,
                                os.path.join(self.install_dir, dylibname),
                                ofile)
                            log.info(cmd)
                            errorcode = os.system(cmd)
                            if errorcode:
                                raise DistutilsExecError("command failed: %s"
                                                         % cmd)
        else:
            self.warn("'%s' does not exist! no shared libraries to install"
                      % self.build_dir)
            return
        return outfiles

    def get_outputs(self):
        # List of files that would be installed if this command were run.
        if not self.distribution.has_shared_libraries():
            return []
        build_cmd = self.get_finalized_command('build_shlib')
        build_files = build_cmd.get_outputs()
        build_dir = build_cmd.build_shlib
        prefix_len = len(build_dir) + len(os.sep)
        outputs = []
        for file in build_files:
            outputs.append(os.path.join(self.install_dir, file[prefix_len:]))
        return outputs

    def get_inputs(self):
        if not self.distribution.has_shared_libraries():
            return []
        build_cmd = self.get_finalized_command('build_py')
        return build_cmd.get_outputs()
            
