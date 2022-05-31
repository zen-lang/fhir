SELECT CODE AS id
     , json_group_array(
           json_object(
               'code', 'parent',
               'valueCoding', json_object(
                   'code', IMMEDIATE_PARENT,
                   'system', 'http://loinc.org',
                   'display',
                       CASE
                       WHEN part.PartNumber != '' AND part.PartNumber NOT NULL THEN part.PartName
                       WHEN loinc.LONG_COMMON_NAME != '' AND loinc.LONG_COMMON_NAME NOT NULL THEN loinc.LONG_COMMON_NAME
                       ELSE (SELECT CODE_TEXT FROM hierarchy ph WHERE ph.CODE=h.IMMEDIATE_PARENT) 
                       END
                   )))
       AS property
FROM hierarchy h
LEFT JOIN part
ON IMMEDIATE_PARENT = part.PartNumber
LEFT JOIN loinc
ON IMMEDIATE_PARENT = loinc.LOINC_NUM
GROUP BY CODE
