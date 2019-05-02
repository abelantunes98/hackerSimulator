#!/bin/bash
# @hcks <... /plp>
# resources instalation
# last update: 20-04-2019

<<DESCRIPTION
  Installation of resources necessary for the operation of the hackerSimulator.
DESCRIPTION

sudo apt-get update

# libboost-dev
sudo apt-get install libboost-dev -y

## c++ compiler
sudo apt-get install g++ -y
