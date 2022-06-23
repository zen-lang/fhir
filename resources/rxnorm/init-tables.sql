DROP TABLE IF EXISTS rxnconso;
--;--
DROP TABLE IF EXISTS rxncui;
--;--
CREATE TABLE rxnconso
(
   rxcui             varchar(8) not null,
   lat               varchar (3) default 'eng' not null,
   ts                varchar (1),
   lui               varchar(8),
   stt               varchar (3),
   sui               varchar (8),
   ispref            varchar (1),
   rxaui             varchar(8) not null,
   saui              varchar (50),
   scui              varchar (50),
   sdui              varchar (50),
   sab               varchar (20) not null,
   tty               varchar (20) not null,
   code              varchar (50) not null,
   str               varchar  not null,
   srl               varchar (10),
   suppress          varchar (1),
   cvf               varchar(50)
);
--;--
CREATE TABLE rxncui (
  cui1        varchar(8),
  ver_start   varchar(40),
  ver_end     varchar(40),
  cardinality varchar(8),
  cui2        varchar(8)
);
