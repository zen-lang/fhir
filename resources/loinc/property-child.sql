CREATE TABLE IF NOT EXISTS property_child AS
SELECT IMMEDIATE_PARENT id
     , json_group_array(
           json_object(
               'code', 'child',
               'valueCoding', json_object(
                   'code', CODE,
                   'system', 'http://loinc.org',
                   'display',
                       CASE
                       WHEN part.PartName != '' AND part.PartName NOT NULL THEN part.PartName
                       WHEN loinc.LONG_COMMON_NAME != '' AND loinc.LONG_COMMON_NAME NOT NULL THEN loinc.LONG_COMMON_NAME
                       ELSE CODE_TEXT
                       END)))
       AS property
FROM hierarchy
LEFT OUTER JOIN part
ON PartNumber = CODE
LEFT OUTER JOIN loinc
ON LOINC_NUM = CODE
WHERE IMMEDIATE_PARENT != ''
GROUP BY IMMEDIATE_PARENT;

