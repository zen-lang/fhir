CREATE TABLE IF NOT EXISTS {{partlink_json_table}} AS
SELECT LoincNumber AS id
     , json_group_array(
           json_object(
               'code', replace(Property, 'http://loinc.org/property/', ''),
               'valueCode', json_object(
                   'valueCode', PartNumber,
                   'display', PartName,
                   'system', PartCodeSystem))) 
       AS property
FROM {{partlink_table}} 
GROUP BY LoincNumber
