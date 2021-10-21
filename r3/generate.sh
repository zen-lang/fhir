#!/bin/sh

jq -cMs '{
           "index-version": 1,
           "files": [
             .[]
             | with_entries(select(.key == ("url", "resourceType", "id", "version", "type")))
               + {"filename": input_filename}
             | select(.url != null)
           ]
         }' *.json > .index.json
