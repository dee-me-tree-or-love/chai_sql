SELECT "> Cats:";

SELECT c.id, c.name
FROM cat AS c;

SELECT "> People:";

SELECT p.id, p.name
FROM person AS p;

SELECT "> Cat Friends: Cat, Person, Years";

SELECT cf.catId, cf.personId, cf.years
FROM catFriends AS cf;

SELECT "> Condition experiments";

SELECT ">> IN (SELECT c2.name FROM cat AS c2 WHERE c2.id = 1)";
SELECT c.id, c.name
FROM cat AS c
WHERE c.name IN (
    SELECT c2.name FROM cat AS c2 WHERE c2.id = 1
);

SELECT ">> IN (Coltrane)";
SELECT c.id, c.name
FROM cat AS c
WHERE c.name IN ("Coltrane");

SELECT ">> IS NOT null";
SELECT c.id, c.name
FROM cat AS c
WHERE c.name IS NOT null;

SELECT ">> c.name IS Coltrane";
SELECT c.id, c.name
FROM cat AS c
WHERE c.name IS "Coltrane";

SELECT ">> true IS true";
SELECT c.id, c.name
FROM cat AS c
WHERE true IS true;

SELECT ">> true IS null";
SELECT c.id, c.name
FROM cat AS c
WHERE true IS null;

SELECT ">> false IS null";
SELECT c.id, c.name
FROM cat AS c
WHERE false IS null;

SELECT ">> null IS null";
SELECT c.id, c.name
FROM cat AS c
WHERE null IS null;

SELECT "> Multiple selects experiments";

SELECT c.name, p.name
FROM cat AS c, person AS p;

SELECT c.name, p.name, cf.id
FROM cat AS c, person AS p, catFriends as cf;

SELECT "> Exist experiments";

SELECT COUNT(*), "-- anything"
FROM cat AS c
WHERE EXISTS (SELECT * FROM person AS p);

SELECT COUNT(*), "-- anything 1?"
FROM cat AS c
WHERE EXISTS (SELECT 1 FROM person AS p);

SELECT COUNT(*), "-- same name"
FROM cat AS c
WHERE EXISTS (
    SELECT 1 
    FROM person AS p 
    WHERE p.name = c.name
);

SELECT COUNT(*), "-- different name?"
FROM cat AS c
WHERE NOT EXISTS (
    SELECT 1 
    FROM person AS p 
    WHERE p.name = c.name
);

SELECT "> Type hint experiments";

-- @chaisql:returns DbView<bag>[String, String]
SELECT
    -- @chaisql:returns String
    p.name,
    -- @chaisql:returns String
    "friend" AS class
FROM person AS p;


SELECT "> Manual join?";

-- @chaisql:returns DbView <set> {catName: String, personName: String, years: Number}
SELECT DISTINCT c.name as catName, p.name as personName, cf.years
FROM cat as c, person as p, catFriends as cf
WHERE   c.id = cf.catId
    AND p.id = cf.personId;