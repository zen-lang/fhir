SELECT
  LoincNumber AS LoincNumber,
  json_group_array ('http://loinc.org/vs/' || GroupId) AS valueset
FROM
  loinc_groups
GROUP BY
  LoincNumber
UNION
SELECT
  AnswerStringId AS LoincNumber,
  json_group_array ('http://loinc.org/vs/' || AnswerListId) AS valueset
FROM
  answerlist
WHERE (LoincNumber <> '' and AnswerStringId <> '')
GROUP BY
  AnswerStringId
