SELECT "Cats:";

SELECT c.id, c.name
FROM cat AS c;

SELECT "People:";

SELECT p.id, p.name
FROM person AS p;

SELECT "Cat Friends:";

SELECT cf.id, cf.catId, cf.personId, cf.years
FROM catFriends AS cf;