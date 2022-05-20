CREATE TABLE IF NOT EXISTS {{partlink_json_table}} AS
with parts AS (
  SELECT
    LoincNumber AS LoincNumber,
    lower(replace(Property, 'http://loinc.org/property/', '')) AS property,
    iif (count(PartNumber) > 1, json_group_array (json_object('code', PartNumber, 'display', PartName, 'system', PartCodeSystem)), json_object('code', PartNumber, 'display', PartName, 'system', PartCodeSystem)) AS property_object
  FROM
    {{partlink_table}}
  GROUP BY
    LoincNumber,
    property
)
SELECT
  LoincNumber,
  json_group_object (property, json (property_object)) AS property
FROM
  parts
GROUP BY
  LoincNumber
