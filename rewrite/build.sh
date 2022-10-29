#!/bin/sh
g++ -std=c++20 -shared -fPIC -o libalgo.dylib algorithm.cpp && ghc Algo.hs -L. -lalgo
