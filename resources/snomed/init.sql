-- Concept table
-- https://confluence.ihtsdotools.org/display/DOCRELFMT/4.2.1+Concept+File+Specification
DROP TABLE IF EXISTS concept;
--;--
CREATE TABLE concept
(
    id TEXT PRIMARY KEY,
    effectiveTime TEXT,
    active INTEGER,
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
    active INTEGER,
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
    active INTEGER,
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
    active INTEGER,
    moduleId TEXT,
    sourceId TEXT,
    destinationId TEXT,
    relationshipGroup INTEGER,
    typeId TEXT,
    characteristicTypeId TEXT,
    modifierId TEXT
);
