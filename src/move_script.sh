#!/bin/bash

cd $1

mv puzzle1.erl day$2_puzzle1.erl
mv puzzle2.erl day$2_puzzle2.erl

mkdir day$2_tests
mv *.txt day$2_tests

mv * ..
cd ..

rmdir $1