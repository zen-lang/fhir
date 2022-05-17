create table part_concept as
select json_object('id', 'loinc-' || PartNumber,
                   'code', PartNumber,
		   'display', PartName,
		   '_source', 'zen.fhir',
		   'system', 'http://loinc.org',
		   'valueset', json_array('http://loinc.org/vs'),
		   'resourceType', 'Concept',
		   'deprecated', status == 'DEPRECATED') as concept
from part
