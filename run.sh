#!/usr/bin/env -S guix shell -m manifest.scm bash -- bash

LD_LIBRARY_PATH="$LIBRARY_PATH" sbcl --noinform  --eval "(require :asdf)" --eval "(asdf:load-system :micros)" --eval "(micros:create-server :dont-close t)"
