CREATE TABLE t (
    `id`   INTEGER PRIMARY KEY,
    `str`  STRING,
    `flag` BOOLEAN DEFAULT (FALSE) 
                 NOT NULL ON CONFLICT REPLACE
);
