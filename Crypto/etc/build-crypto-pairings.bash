#!/usr/bin/env bash
#
#  Assuming the proper development tools are in place, make the
#  libraries needed, and produce a shared object suitable for loading
#  the code for Pair Based Curves (PBC).
#
# Linux
#   apt-get install gcc make g++ flex bison
# MacOS
#   XCode needs to be installed


# debug
set -x

EXTERNAL_LIBS_VERSION=release-0.1.15

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

BASE=${DIR}/..
var=${BASE}/var

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
        echo Building for Linux
        lib_suffix=linux
        maketarget=makefile.linux.static
        if [ "x${PENTIUM4}" == "xtrue" ] ; then
          EXTERNAL_LIBS_VERSION=release-0.1.8-p4-linux
        fi
        ;;
    Darwin*)
        echo Building for macOS
        lib_suffix=osx
        maketarget=makefile.macos.static
        ;;
    *)
        maketarget=makefile.linux
        echo Unknown OS \"$(uname_s)\" -- defaulting to Linux Makefile
        exit 127
        ;;
esac

libs_url=https://github.com/emotiq/emotiq-external-libs/releases/download/${EXTERNAL_LIBS_VERSION}/emotiq-external-libs-${lib_suffix}.tgz

mkdir -p ${var}/local

(cd ${var}/local && curl -L ${libs_url} | tar xvfz -)

prefix=${var}/local
lib=${prefix}/lib
inc=${prefix}/include
pbcintf=${BASE}/src/Crypto/Crypto-Libraries/PBC-Intf

# Remove shared libs (we use only statics)
rm ${lib}/libgmp*.dylib ${lib}/libpbc*.dylib ${lib}/libgmp.so* ${lib}/libpbc.so*

export CFLAGS=-I${inc}
export CPPFLAGS=-I${inc}
export CXXFLAGS=-I${inc}
export LDFLAGS=-L${lib}

cd ${pbcintf} && \
    make --makefile=${maketarget} PREFIX=${prefix}
