ALTER TABLE `contacts`
ADD COLUMN `gender` VARCHAR(1) CHECK(`gender` IN('M','F'));
