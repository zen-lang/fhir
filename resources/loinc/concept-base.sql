CREATE TABLE loinc_concept AS
SELECT
  base.id AS LoincNumber,
  json_object('id', 'loinc-' || base.id, 'code', base.id, 'display', loinc.LONG_COMMON_NAME, '_source', 'zen.fhir', 'system', 'http://loinc.org', 'valueset', json_array ('http://loinc.org/vs'), 'resourceType', 'Concept', 'property', json_object('loinc', json_patch (json_patch (base.property, prim.property), supp.property))) AS concept
FROM
  loinc_base_json base
  LEFT JOIN partlink_primary_json prim ON prim.LoincNumber = base.id
  LEFT JOIN partlink_supplementary_json supp ON supp.LoincNumber = base.id
  LEFT JOIN loinc ON loinc.LOINC_NUM = base.id
GROUP BY
  base.id
