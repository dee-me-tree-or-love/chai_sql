SELECT "Cats:";

SELECT c.id, c.name
FROM cat AS c;

SELECT "People:";

SELECT p.id, p.name
FROM person AS p;

SELECT "Cat Friends: Cat, Person, Years";

SELECT cf.catId, cf.personId, cf.years
FROM catFriends AS cf;