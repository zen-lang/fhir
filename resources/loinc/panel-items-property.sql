WITH sorted_groups AS (
  SELECT
    *
  FROM
    panels_and_forms
  WHERE
    ParentLoinc <> Loinc
  ORDER BY
    ParentLoinc,
    cast("SEQUENCE" AS int)
),
panel_items AS (
  SELECT
    ParentLoinc,
    json_group_array (json_object('system', 'http://loinc.org', 'code', Loinc, 'display', iif (DisplayNameForForm <> '', DisplayNameForForm, LoincName), 'answer-list', iif (AnswerListIdOverride <> '', json_object('code', AnswerListIdOverride, 'system', 'http://loinc.org'), NULL), 'cardinality', AnswerCardinality)) AS property
  FROM
    sorted_groups
  GROUP BY
    ParentLoinc)
UPDATE
  loinc_concept
SET
  concept = json_set (loinc_concept.concept, '$.property.loinc.panel-items', json (panel_items.property))
FROM
  panel_items
WHERE
  loinc_concept.LoincNumber = panel_items.ParentLoinc
