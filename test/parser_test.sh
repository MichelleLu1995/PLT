#!/bin/bash


INPUT_FILES="parser/*.in"
printf "$Running parser tests...\n$"

for input_file in $INPUT_FILES; do
    output_file=${input_file/.in/.out}
    input=$(parser/parserize < $input_file | tr -d "[:space:]")
    output=$(tr -d "[:space:]" < $output_file);
    if [[ "$input" == "$output" ]]; then
        printf "%-65s $SUCCESS\n$" "  - checking $input_file..."
    else
        printf "%-65s $ERROR\n$" "  - checking $input_file..." 1>&2
        exit 1
    fi
done

exit 0