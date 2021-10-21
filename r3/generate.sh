#!/bin/sh

jq -cM 'with_entries(select(.key == ("url", "resourceType", "id", "version", "type")))
        + {"filename": input_filename}
        | select(.url != null)' *.json | jq -s '{"index_version": 1, "files": .}' > .index.json
