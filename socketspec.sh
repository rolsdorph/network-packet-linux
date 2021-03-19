#!/bin/sh

./setup_unixtest_interface.sh
cabal test socketspec
