#!/bin/bash
erlc addressbook_capnp.erl calculator-client.erl
./addressbook.sh write | ./addressbook.sh read
erl -eval "'calculator-client':run()"
