WITH answer_list AS (
  SELECT
    LoincNumber,
    json_object('code', AnswerListId, 'display', AnswerListName, 'system', 'http://loinc.org') AS property
  FROM
    answerlistlink
  GROUP BY
    LoincNumber)
UPDATE
  loinc_concept
SET
  concept = json_set (loinc_concept.concept, '$.property.loinc.answer-list', json (answer_list.property))
FROM
  answer_list
WHERE
  loinc_concept.LoincNumber = answer_list.LoincNumber
