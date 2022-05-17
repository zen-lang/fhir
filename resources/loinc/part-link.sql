CREATE TABLE IF NOT EXISTS {{partlink_json_table}} AS
with parts as (SELECT LoincNumber AS LoincNumber
     , replace(Property, 'http://loinc.org/property/', '') as property,
          iif(count(PartNumber) > 1,
	  json_group_array(
		 json_object(
                   'code', PartNumber,
                   'display', PartName,
                   'system', PartCodeSystem)),
	  	json_object(
                   'code', PartNumber,
                   'display', PartName,
                   'system', PartCodeSystem))
       AS property_object
FROM {{partlink_table}}
GROUP BY LoincNumber, property)
select LoincNumber, json_group_object(property, json(property_object)) as property
from parts
group by LoincNumber
