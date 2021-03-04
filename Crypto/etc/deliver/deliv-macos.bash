#!/usr/bin/env bash

# Cocoa adds complications (see Delivery manual).  This binary appears not to need Cocoa, so things are simple.
# basically, this script calls LWM with two arguments '-build' and 'deliver.lisp'
# Change the pathname appropriately for 64-bit LWM

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# LWPRO_MACOS_32=${LWPRO_MACOS_32:-/Applications/LispWork\ 7.0\ \(32-bit\)/LispWorks\ \(32-bit\).app/Contents/MacOS/lispworks-7-0-0-x86-darwin}
LWPRO_MACOS_64=${LWPRO_MACOS_64:-/Applications/LispWorks\ 7.1\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin}

LWPRO=${LWPRO_MACOS_64}

"${LWPRO}" -build "${DIR}/deliver.lisp"
