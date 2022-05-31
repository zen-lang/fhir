SELECT
  LoincNumber,
  json_object('answer-list', json_object('code', AnswerListId, 'display', AnswerListName, 'system', 'http://loinc.org')) AS property
FROM
  answerlistlink
GROUP BY
  LoincNumber
