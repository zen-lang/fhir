#!/bin/bash

for file in $(ls *.json)
do
        if [[ "null" != $(cat $file | jq .url) ]]; then
                cat $file \
                        | jq 'with_entries(select([.key] | inside(["resourceType", "id", "url", "version", "type"])))' \
                        | jq --arg filename $(echo $file | sed 's/.*\///') '. += {"filename": $filename}'
        fi;
done | jq '{"index-version": 1, "files": [inputs]}' > .index.json
