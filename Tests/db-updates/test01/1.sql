CREATE TABLE t (
    `id`   INTEGER PRIMARY KEY /*! AUTO_INCREMENT */,
    `str`  VARCHAR(128),
    `flag` BOOLEAN DEFAULT FALSE
                 NOT NULL /*ON CONFLICT REPLACE*/
);
