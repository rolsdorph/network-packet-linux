#!/bin/sh

ip link add haskellnetwork type dummy
ip link set haskellnetwork address 12:34:56:78:37:00
ip link set haskellnetwork up
