#!/bin/bash
erlc addressbook_capnp.erl
./addressbook.sh write | ./addressbook.sh read
