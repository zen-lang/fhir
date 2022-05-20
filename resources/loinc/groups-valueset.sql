SELECT
  l.GroupId,
  json_object('resourceType', 'ValueSet', 'id', l.GroupId, '_source', 'zen.fhir', 'name', g. `Group`, 'url', 'http://loinc.org/vs' || l.GroupId, 'status', lower(g.Status), 'compose', json_object('include', json_array (json_object('system', 'http://loinc.org', 'concept', json_group_array (json_object('code', LoincNumber, 'display', LongCommonName)))))) AS valueset
FROM
  loinc_groups l
  LEFT JOIN GROUPS g ON l.GroupId = g.GroupId
GROUP BY
  l.GroupId
