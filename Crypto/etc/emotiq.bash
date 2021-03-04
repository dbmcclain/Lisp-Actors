#!/usr/bin/env bash

APP_NAME=${1:-emotiq}

workingdir="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case $(uname -s) in
  Linux*)
    export LD_LIBRARY_PATH=${workingdir}
    echo workingdir = ${workingdir}
    echo LD_LIBRARY_PATH $LD_LIBRARY_PATH
    ;;
  Darwin*)
    export DYLD_LIBRARY_PATH=${workingdir}
    export DYLD_FALLBACK_LIBRARY_PATH=${workingdir}
    echo workingdir = ${workingdir}
    echo DYLD_LIBRARY_PATH $DYLD_LIBRARY_PATH
    echo DYLD_FALLBACK_LIBRARY_PATH $DYLD_FALLBACK_LIBRARY_PATH
    ;;
  *)
    echo "Unknown platform!"
    exit 1
    ;;
esac

cd ${workingdir}
exec ${workingdir}/${APP_NAME} $*
