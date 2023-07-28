--@chaisql :: {catName: Text, dogName: Text}
SELECT
    c.name,
    d.name
FROM
    Cats AS c,
    Dogs AS d;