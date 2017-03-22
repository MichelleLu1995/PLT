#!/bin/bash

INPUT_FILES="scanner/*.in"
printf "$Running scanner tests...\n$"

for input_file in $INPUT_FILES; do
    output_file=${input_file/.in/.out}
    scanner/tokenize < $input_file | cmp -s $output_file -
    if [ "$?" -eq 0 ]; then
       printf "%-65s $SUCCESS\n$" "  - checking $input_file..."
    else
       printf "%-65s $ERROR\n$" "  - checking $input_file..." 1>&2
       exit 1
    fi
done

exit 0
