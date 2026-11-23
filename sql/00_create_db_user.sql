-- Run once as root (edit the password first, and put the same one in db_config.cnf):
--   mysql -uroot -p < sql/00_create_db_user.sql
CREATE DATABASE IF NOT EXISTS IMPC_Group11 CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE USER IF NOT EXISTS 'group11'@'localhost' IDENTIFIED BY 'CHANGE_ME';
GRANT ALL PRIVILEGES ON IMPC_Group11.* TO 'group11'@'localhost';
FLUSH PRIVILEGES;
