#!/bin/sh

sudo ip link add haskellnetwork type dummy
sudo ifconfig haskellnetwork hw ether 12:34:56:78:37
sudo ip link set haskellnetwork up
