#!/bin/sh

sudo ip link add haskellnetwork type dummy
sudo ip link set haskellnetwork address 12:34:56:78:37:00
sudo ip link set haskellnetwork up
