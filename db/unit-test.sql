create table profile
(
    id int(10) unsigned NOT NULL AUTO_INCREMENT,
    name char(20) NOT NULL,
    birth date DEFAULT NULL,
    color enum('blue', 'red', 'green', 'brown', 'black', 'white') DEFAULT NULL,
    foods set('lutefisk', 'burrito', 'curry', 'eggroll', 'fadge', 'pizza') DEFAULT NULL,
    cats int(11) DEFAULT NULL,
    PRIMARY KEY (id)
);

create table p
(
    id int(10) unsigned NOT NULL AUTO_INCREMENT,
    name varchar(50) DEFAULT NULL,
    age int(11) DEFAULT NULL,
    first_name varchar(100) DEFAULT NULL,
    lastName varchar(100) DEFAULT NULL,
    PRIMARY KEY (id)
);

create table tmp
(
    id int(10) unsigned NOT NULL AUTO_INCREMENT,
    last_update timestamp NOT NULL,
    create_time timestamp NOT NULL,
    valid char(1) DEFAULT NULL,
    PRIMARY KEY (id)
);
