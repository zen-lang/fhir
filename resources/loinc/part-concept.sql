SELECT
  PartNumber LoincNumber,
  json_object('id', 'loinc-' || PartNumber, 'code', PartNumber, 'display', PartName, '_source', 'zen.fhir', 'system', 'http://loinc.org', 'valueset', json_group_array ('http://loinc.org/vs'), 'resourceType', 'Concept') AS concept
FROM
  part
GROUP BY
  PartNumber
