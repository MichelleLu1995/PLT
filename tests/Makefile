# 
# JSTEM: Makefile for Testing
#  

default: test

test: build
	./scanner_tests.sh

build: 
	cd scanner; make

.PHONY: clean
clean:
	cd scanner; make clean