/* Create second table */
CREATE TABLE `groups` (
    `id` INTEGER PRIMARY KEY,
    `name` VARCHAR(32) NOT NULL
);

/* Fill with several built-in values */
INSERT INTO `groups` (`name`) VALUES ('Others'), ('Administrator'), ('Manager'), ('Seller');

ALTER TABLE `contacts`
ADD COLUMN `group_id` INTEGER /*NOT NULL*/;

/* Set all exists persons to "Others" group */
UPDATE `contacts` SET `group_id` = (SELECT `id` FROM `groups` WHERE `name` = 'Others');
