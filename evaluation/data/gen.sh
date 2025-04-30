#!/bin/bash

set -e
OUTPUT_FILE="data.ml"

echo "let well_typed : string list = [" > "$OUTPUT_FILE"
for f in ./well-typed/*.hazel; do
  echo "  {|" >> "$OUTPUT_FILE"
  cat "$f" >> "$OUTPUT_FILE"
  echo "|};" >> "$OUTPUT_FILE"
done
echo "]" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

echo "let ill_typed_annotated : string list = [" >> "$OUTPUT_FILE"
for f in ./ill-typed/*.typed.hazel; do
  echo "  {|" >> "$OUTPUT_FILE"
  cat "$f" >> "$OUTPUT_FILE"
  echo "|};" >> "$OUTPUT_FILE"
done
echo "]" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

echo "let ill_typed_dynamic : string list = [" >> "$OUTPUT_FILE"
for f in ./ill-typed/*.hazel; do
  if [[ "$f" != *.typed.hazel ]]; then
    echo "  {|" >> "$OUTPUT_FILE"
    cat "$f" >> "$OUTPUT_FILE"
    echo "|};" >> "$OUTPUT_FILE"
  fi
done
echo "]" >> "$OUTPUT_FILE"

echo "Generated $OUTPUT_FILE"
