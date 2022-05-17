create table loinc_concept as
select json_object('id', 'loinc-' || base.id,
		   'code', base.id,
		   'display', loinc.LONG_COMMON_NAME,
		   '_source', 'zen.fhir',
		   'system', 'http://loinc.org',
		   'valueset', json_array('http://loinc.org/vs'),
		   'resourceType', 'Concept',
		   'property', json_patch(json_patch(base.property, prim.property), supp.property)) as concept
from loinc_base_json base
left join partlink_primary_json prim
on prim.LoincNumber = base.id
left join partlink_supplementary_json supp
on supp.LoincNumber = base.id
left join loinc
on loinc.LOINC_NUM = base.id
group by base.id
