WITH valuesets AS (
  SELECT
    LoincNumber AS LoincNumber,
    'http://loinc.org/vs/' || GroupId AS valueset
  FROM
    loinc_groups
  UNION
  SELECT
    AnswerStringId AS LoincNumber,
    'http://loinc.org/vs/' || AnswerListId AS valueset
  FROM
    answerlist
  WHERE (LoincNumber <> '' and AnswerStringId <> '')
),
all_valuesets AS (
  SELECT
    LoincNumber,
    json_insert (json_group_array (valueset), '$[#]', 'http://loinc.org/vs') valueset
  FROM
    valuesets
  GROUP BY
    LoincNumber)
UPDATE
  loinc_concept
SET
  concept = json_set (loinc_concept.concept, '$.valueset', json (all_valuesets.valueset))
FROM
  all_valuesets
WHERE
  loinc_concept.LoincNumber = all_valuesets.LoincNumber
