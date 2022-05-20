WITH sorted_groups AS (
  SELECT
    *
  FROM
    panels_and_forms
  WHERE
    ParentLoinc <> Loinc
  ORDER BY
    ParentLoinc,
    cast("SEQUENCE" AS int))
SELECT
  ParentLoinc,
  json_object('panel-items', json_group_array (json_object('system', 'http://loinc.org', 'code', Loinc, 'display', iif (DisplayNameForForm <> '', DisplayNameForForm, LoincName)))) AS property
FROM
  sorted_groups
GROUP BY
  ParentLoinc
