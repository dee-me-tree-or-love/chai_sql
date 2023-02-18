-- NB: This file is used to test the type annotation syntax

-- ~~

-- file: case_1.sql
-- @chaisql:check

-- @chaisql:returns DbView <bag> {id: Number, name: String}
SELECT *
FROM cat;

-- ~~

-- file: case_2.sql
-- @chaisql:check

-- @chaisql:returns DbView <bag> {name: String}
SELECT *
FROM person;

-- ~~

-- file: case_3.sql
-- @chaisql:check

-- @chaisql:returns DbView <set> {id: Number}
SELECT DISTINCT catId
FROM catFriends;

-- ~~

-- file: case_4.sql
-- @chaisql:check

-- @chaisql:returns DbView <set> {id: Number, name: String}
SELECT DISTINCT p.id, p.name
FROM person AS p
WHERE p.name IN (
   -- @chaisql:returns DbView <set> {id: Number}
   SELECT DISTINCT personId
   FROM catFriends
);

-- ~~

-- file: case_5.sql
-- @chaisql:check

-- @chaisql:returns DbView <set> {catName: String, personName: String, years: Number}
SELECT DISTINCT c.name as catName, p.name as personName, cf.years
FROM cat as c, person as p, catFriends as cf
WHERE   c.id = cf.catId
    AND p.id = cf.personId;

-- Outdated --
-- ~~~~~~~~ --

-- FIXME: remove the outdated examples
-- -- @sql-tick:tc

-- -- @tr {String}
-- SELECT "Cats:";

-- -- @tr {Number, String}
-- SELECT c.id, c.name
-- FROM cat AS c;

-- -- @tr {String}
-- SELECT "People:";

-- -- @tr {Number, String}
-- SELECT p.id, p.name
-- FROM person AS p;

-- -- @tr {String}
-- SELECT "Cat Friends: Cat, Person, Years";

-- -- @tr {Number, Number, Number}
-- SELECT cf.catId, cf.personId, cf.years
-- FROM catFriends AS cf;
