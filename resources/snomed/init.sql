-- Concept table
-- https://confluence.ihtsdotools.org/display/DOCRELFMT/4.2.1+Concept+File+Specification
DROP TABLE IF EXISTS concept;
--;--
CREATE TABLE concept
(
    id TEXT PRIMARY KEY,
    effectiveTime TEXT,
    active CHAR(1),
    moduleId TEXT,
    definitionStatusId TEXT
);
--;--
-- Description table
-- https://confluence.ihtsdotools.org/display/DOCRELFMT/4.2.2+Description+File+Specification
DROP TABLE IF EXISTS description;
--;--
CREATE TABLE description
(
    id TEXT PRIMARY KEY,
    effectiveTime TEXT,
    active CHAR(1),
    moduleId TEXT,
    conceptId TEXT,
    languageCode TEXT,
    typeId TEXT,
    term TEXT,
    caseSignificanceId TEXT
);
--;--
-- Textual definition of concepts
DROP TABLE IF EXISTS textdefinition;
--;--
CREATE TABLE textdefinition
(
    id TEXT PRIMARY KEY,
    effectiveTime TEXT,
    active CHAR(1),
    moduleId TEXT,
    conceptId TEXT,
    languageCode TEXT,
    typeId TEXT,
    term TEXT,
    caseSignificanceId TEXT
);
--;--
-- Relationship table
-- https://confluence.ihtsdotools.org/display/DOCRELFMT/4.2.3+Relationship+File+Specification
DROP TABLE IF EXISTS relationship;
--;--
CREATE TABLE relationship
(
    id TEXT PRIMARY KEY,
    effectiveTime TEXT,
    active CHAR(1),
    moduleId TEXT,
    sourceId TEXT,
    destinationId TEXT,
    relationshipGroup TEXT,
    typeId TEXT,
    characteristicTypeId TEXT,
    modifierId TEXT
);
--;--
-- SDL table
-- http://people.apache.org/~dongsheng/horak/100309_dag_structures_sql.pdf
DROP TABLE IF EXISTS sdl;
--;--
CREATE TABLE sdl
(
    src  text,
    dst  text,
    dist integer
);
--;--
create or replace function jsonb_object_nullif(
    _data jsonb
)
returns jsonb
as $$
    select nullif(jsonb_strip_nulls(_data)::text, '{}')::json
$$ language sql;
