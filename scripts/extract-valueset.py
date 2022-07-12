#!/usr/bin/env python
# coding: utf-8
"""
Convert xlsx valueset to json which can be accepted
by terminology bundle generator.

Output format:
list with two elements.
First element: valueset resource
Second element: list that contains concept codes included in this valueset

Invocation:
python extract-valueset.py path/to/valueset.xlsx where/to/save/converted.json
"""
import pandas as pd
import json
import sys

filename = sys.argv[1]
output = sys.argv[2]

df_info = pd.read_excel(filename, sheet_name=0, index_col=0, squeeze=True)
df_expansion = pd.read_excel(filename, sheet_name=1)

vsname = df_info['Value Set Name']
vsname_letters = [word[0].lower() for word in vsname.split()]
vsname_short = ''.join(vsname_letters)

oid = df_info.loc[['OID']][0]

vs = {
    'resourceType': 'ValueSet',
    'id': 'rxnorm-vs-' + vsname_short,
    'description': vsname,
    'version': df_info['Definition Version'],
    'identifier': [{
            'system': 'urn:ietf:rfc:3986',
            'value': oid
        }
    ],
    'url': f"http://cts.nlm.nih.gov/fhir/ValueSet/{oid}",
    'publisher': df_info[['Steward']][0].strip(),
    'purpose': df_info[['Purpose: Clinical Focus']][0].strip(),
}

assert df_expansion.iloc[11,0] == 'Code', 'Unexpected file structure'
concepts = df_expansion.iloc[12:, 0]

vs_data = [vs, list(concepts)]
with open(output, 'w') as f:
    json.dump(vs_data, f)
