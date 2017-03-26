#!/bin/bash

INPUT_FILES="scanner/*.in"
printf "TESTING SCANNER\n"

for input_file in $INPUT_FILES; do
    output_file=${input_file/.in/.out}
    scanner/tokenize < $input_file | cmp -s $output_file -
    if [ "$?" -eq 0 ]; then
       printf "%-65s SUCCESS\n" "  - $input_file..."
    else
       printf "%-65s ERROR\n" "  - $input_file..." 1>&2
       exit 1
    fi
done

exit 0
