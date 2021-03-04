#!/usr/bin/env bash
#
# Run the collected test harness for all tests
#
# The behavior is controlled by the following variables
#
# lisp
#   Lisp executable to use to invoke test harness.
#
# systems
#
#   Whitespace separated list of systems to test.  If no systems are
#   specified, those contained in <file:all-tests.sexp> are used.
#
#
# For example the following command
#
#   lisp=ccl systems=":gossip-test :cosi-bls-test" bash test-harness.bash
#
# would invoke the test harness using `ccl` on the `gossip-test` and
# `cosi-bls-test` systems.
#
DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# The command to run a Lisp test form
lisp=${lisp:-'ccl'}

function lisp_exec () {
    # We're running in the CI
    if [[ ${lisp} == "ci/lisp-wrapper.bash" ]]; then
        ${lisp} $*
    else
        tmpfile=$(mktemp /tmp/script-exec.lisp-XXXXXX)
        while test $# -gt 0; do
            case "$1" in
                --eval|-e)
                    echo "$2" >> $tmpfile
                    shift
                    shift
                    ;;
                --load|-l)
                    echo "(load \"$2\")" >> $tmpfile
                    shift
                    shift
                    ;;
                *)
                    echo "Unknown argument $1!"
                    exit 1
                    ;;
            esac
        done
        echo "(uiop:quit 0)" >> $tmpfile
        ${lisp} <$tmpfile
    fi
}

echo Test harness invoked using implementation: ${lisp}
echo $(lisp_exec --eval '(format t "~&~a ~a~&" (lisp-implementation-type)(lisp-implementation-version))')

systems=${systems:-\
 $(systems_file=$(mktemp /tmp/systems-XXXXX); \
   lisp_exec --load ${DIR}/parse-systems.lisp --eval "(parse-systems-into \"${systems_file}\")" >/dev/null; \
   cat < ${systems_file})}

# Collect whitespace list of failed systems
failed=

for system in ${systems}; do
    rm -rf ~/.cache/common-lisp/
    echo "==> ASDF:TEST-SYSTEM invoked on ${system}..."
    lisp_exec \
        --eval "(ql:quickload :asdf-test-harness)" \
        --load "${DIR}/test-harness.lisp" \
        --eval "(test-harness:test-system ${system})"
    status=$?
    if [[ $status -ne 0 ]] ; then
        echo "!== ASDF:TEST-SYSTEM failed for ${system}"
        failed="${failed} ${system}"
    else
        echo "<== ASDF:TEST-SYSTEM on ${system} succeeded."
    fi
done

if [[ ! -z ${failed} ]]; then
    echo === The following systems had failing tests: ${failed}
    exit 1
fi

exit 0
