#!/bin/bash

INPUT_FILES="parser/*.in"
printf "TESTING PARSER\n"

for input_file in $INPUT_FILES; do
    output_file=${input_file/.in/.out}
    input=$(parser/parserize < $input_file | tr -d "[:space:]")
    output=$(tr -d "[:space:]" < $output_file);
    if [[ "$input" == "$output" ]]; then
        printf "%-65s SUCCESS\n" "  - $input_file..."
    else
        printf "%-65s ERROR\n" "  - $input_file..." 1>&2
        printf "$input\n"
        exit 1
    fi
done

exit 0