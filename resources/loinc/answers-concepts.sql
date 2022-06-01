SELECT
  AnswerStringId LoincNumber,
  json_object('resourceType', 'Concept', 'id', 'loinc-' || AnswerStringId, 'code', AnswerStringId, 'display', DisplayText, '_source', 'zen.fhir', 'system', 'http://loinc.org', 'valueset', json_insert (json_group_array ('http://loinc.org/vs/' || AnswerListId), '$[#]', 'http://loinc.org/vs')) AS concept
FROM
  answerlist
WHERE
  AnswerStringId <> ''
GROUP BY
  LoincNumber
