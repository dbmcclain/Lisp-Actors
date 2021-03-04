#!/usr/bin/env bash

DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# LWPRO=${LWPRO:-/usr/local/lib64/LispWorks/lispworks-7-1-0-amd64-linux}
LWPRO=${LWPRO:-~/.roswell/impls/x86-64/linux/lispworks-bin/7.1.0.0/lwpro}

"${LWPRO}" -build "${DIR}/deliver.lisp"
