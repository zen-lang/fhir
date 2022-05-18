SELECT
	json_object('compose', json_object('include', json_array(json_object('system', 'http://loinc.org',
																		 'concept', json_group_array (json_object('code', AnswerStringId, 'display',DisplayText))))),
									   'url', 'http://loinc.org/vs/' || AnswerListId,
									   'name', AnswerListName,
									   'id', AnswerListId,
									   'resourceType', 'ValueSet'
									   ) as valueset
FROM
	answerlist
GROUP BY
	AnswerListId
LIMIT 10
