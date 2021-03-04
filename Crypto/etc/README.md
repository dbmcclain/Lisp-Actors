We need to have two branches, determined at runtime, to differentiate between Emotiq development and building a binary.

Building a binary is referred to as `Production`.

This process must also gel with GitLab CI, which rebuilds the whole system (inside of GitLab) on every push to the repository.

## Development:

As a first step, we need to snip all ties with non-standard libraries, e.g. `var/local/lib/libpbc.dylib` and `var/local/lib/libgmp.dylib` (from now on all pathes are relative to root of the source tree).

This process is described in section **Building the native libraries required by CRYPTO-PAIRINGS** in the root `README.md` file.

The build is only necessary for developers using the crypto-pairing libraries (PBC and GMP).

For development, this script needs to be run only once, and the var directory structure is retained.

Developers need to load these systems: `emotiq`, `emotiq/delivery`, `emotiq/utilities`, `emotiq/blockchain` and `emotiq/startup`.

Developers should run `(emotiq:main)` to "run" the system.  The code to run a blockchain node are installed by modifying the function `main`in the file `src/startup.lisp`.

## Building a binary:

Building a binary (which is something developers will rarely do) is performed by another script.  The steps are:

1. Delete the `var` directory (e.g. `rm -rf var`)
1. Check path to the LispWorks binary at `etc/deliver/deliv-{macos,linux}.bash`
1. Run the delivery script `etc/build-binary.sh`.

This script also runs for a long time and displays many messages.

This script results in:
- a script emotiq.bash
- a binary called emotiq
- a DLL called libLispPBCIntf.{dylib,.so}
- DLLs called libpbc.{dylib,.so}.*
- DLLs called libgmp.{dylib,.so}.*

The resulting files are all placed in one directory `emotiq/var/local/production/emotiq-<timestamp>-<commit_hash>-<arch>`.

The binary is run with the command line `bash emotiq.bash`.

The binary always calls the Lisp function "start".

The above works with LispWorks development, and the processes may change as we branch out to use other lisps.

## Production:

Building `production` binary is done by pushing _tagged_ commit to the repository.  
