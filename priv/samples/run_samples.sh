#!/bin/bash

set -exuo pipefail

erlc addressbook_capnp.erl calculator_capnp.erl \
    calculator-client.erl calculator-server.erl

./addressbook.sh write | ./addressbook.sh read

erl -eval "'calculator-server':run()" -noinput -noshell &
sleep 0.1
erl -eval "'calculator-client':run()" -noinput -noshell
kill %+
