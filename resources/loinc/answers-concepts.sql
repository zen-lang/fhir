SELECT
  AnswerStringId LoincNumber,
  json_object('resourceType', 'Concept', 'id', 'loinc-' || AnswerStringId, 'code', AnswerStringId, 'display', DisplayText, '_source', 'zen.fhir', 'system', 'http://loinc.org', 'valueset', json_array ('http://loinc.org/vs/' || AnswerListId)) AS concept
FROM
  answerlist
WHERE
  AnswerStringId <> ''
