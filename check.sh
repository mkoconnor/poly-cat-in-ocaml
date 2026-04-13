#!/bin/bash
set -euo pipefail
ocamlopt -c state_machine.ml && ghc -c StateMachine.hs
