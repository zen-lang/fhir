SELECT
  p.ParentGroupId,
  json_object('id', p.ParentGroupId, 'name', p.ParentGroup, 'url', 'http://loinc.org/vs/' || p.ParentGroupId, '_source', 'zen.fhir', 'resourceType', 'ValueSet', 'compose', json_object('include', json_array (json_object('valueset', json_group_array ('http://loinc.org/vs/' || g.GroupId))))) AS valueset
FROM
  GROUPS g
  LEFT JOIN parent_groups p ON p.ParentGroupId = g.ParentGroupId
GROUP BY
  p.ParentGroupId
