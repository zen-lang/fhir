-- This query selects property `answers-for` for LL* concepts
SELECT
  AnswerListId,
  json_object('answers-for', json_group_array (json_object('code', LoincNumber, 'system', 'http://loinc.org', 'display', LongCommonName)))
FROM
  answerlistlink
GROUP BY
  AnswerListId
