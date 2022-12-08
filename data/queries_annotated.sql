-- NB: This file is used to test the type annotation syntax

-- @sql-tick:tc

-- @tr {String}
SELECT "Cats:";

-- @tr {Number, String}
SELECT c.id, c.name
FROM cat AS c;

-- @tr {String}
SELECT "People:";

-- @tr {Number, String}
SELECT p.id, p.name
FROM person AS p;

-- @tr {String}
SELECT "Cat Friends: Cat, Person, Years";

-- @tr {Number, Number, Number}
SELECT cf.catId, cf.personId, cf.years
FROM catFriends AS cf;