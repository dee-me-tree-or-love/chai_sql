CREATE TABLE IF NOT EXISTS cat (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT not null
);

CREATE TABLE IF NOT EXISTS person (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT not null
);

CREATE TABLE IF NOT EXISTS catFriends (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    catId INTEGER,
    personId INTEGER,
    years INTEGER,
    FOREIGN KEY(catId) REFERENCES cat(id),
    FOREIGN KEY(personId) REFERENCES person(id)
);

-- insert data

INSERT INTO cat(name) VALUES("Coltrane");
INSERT INTO cat(name) VALUES("Scratchy");
INSERT INTO cat(name) VALUES("Snowball III");

INSERT INTO person(name) VALUES("Lisa");
INSERT INTO person(name) VALUES("Homer");
INSERT INTO person(name) VALUES("Bart");

INSERT INTO catFriends(catId, personId, years)
SELECT c.id, p.id, 0
FROM cat c, person p
WHERE p.name IS "Lisa";

INSERT INTO catFriends(catId, personId, years)
SELECT c.id, p.id, 0
FROM cat c, person p
WHERE p.name IS "Homer"
AND c.name IS "Coltrane";

INSERT INTO catFriends(catId, personId, years)
SELECT c.id, p.id, 0
FROM cat c, person p
WHERE p.name IS "Bart"
AND c.name IS "Scratchy";

