#!/usr/bin/env bash
#
#  Assuming the proper development tools are in place, make the
#  libraries needed, and produce a shared object suitable for loading
#  the code for High Performance Curve1174 and Ed3363.
#
# Linux
#   apt-get install gcc make g++ flex bison
# MacOS
#   XCode needs to be installed


# debug
set -x

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

BASE=${DIR}/..
var=${BASE}/var

uname_s=$(uname -s)
case ${uname_s} in
    Linux*)
        echo Building for Linux
        maketarget=makefile.linux
        ;;
    Darwin*)
        echo Building for macOS
        maketarget=makefile.macos
        ;;
    *)
        maketarget=makefile.linux
        echo Unknown OS \"$(uname_s)\" -- defaulting to Linux Makefile
        exit 127
        ;;
esac

mkdir -p ${var}/local/{lib,include}

prefix=${var}/local
lib=${prefix}/lib
inc=${prefix}/include
eccintf=${BASE}/src/Crypto/Crypto-Libraries/Ed3363-C-Code

export CFLAGS=-I${inc}
export CPPFLAGS=-I${inc}
export CXXFLAGS=-I${inc}
export LDFLAGS=-L${lib}

cd ${eccintf} && \
    make --makefile=${maketarget} PREFIX=${prefix}
