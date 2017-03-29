# 
# JSTEM: Makefile for Testing
#  

default: test

test: 
	./all_testing.sh
	@echo "   " 
	./scanner_testing.sh
	@echo "   "
	./parser_testing.sh
	@echo "   "
	@echo "TESTING COMPLETE"

build: 
	cd scanner; make
	cd parser; make

.PHONY: clean
clean:
	cd scanner; make clean
	cd parser; make clean