#!/bin/sh
g++-12 -std=c++23 -shared -fPIC -o libalgo.dylib algorithm.cpp && ghc Algo.hs -L. -lalgo
