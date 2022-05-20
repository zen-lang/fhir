SELECT
  LoincNumber AS LoincNumber,
  json_object('valueset', 'http://loinc.org/vs/' || GroupId) AS valueset
FROM
  loinc_groups
UNION
SELECT
  AnswerStringId AS LoincNumber,
  json_object('valueset', 'http://loinc.org/vs/' || AnswerListId) AS valueset
FROM
  answerlist
WHERE (LoincNumber <> '' and AnswerStringId <> '')
