SELECT
  json_object('resourceType', 'Concept',
              'id', 'loinc-' || AnswerStringId,
	      'code', AnswerStringId,
	      'display', DisplayText,
	      '_source', 'zen.fhir',
	      'codesystem', 'http://loinc.org',
	      'valueset', json_array ('http://loinc.org/vs')) AS concept
FROM
  answerlist
