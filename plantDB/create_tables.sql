create table plant(
    id     integer primary key autoincrement, 
    name   text, 
    ename  text,
    sname  text,
    family text,
    memo   text,
    othername text,
    origin text,
    location text,
    usage text,
    stem text,
    leaf text,
    flower text,
    fruit text,
    property text,
    more text,
    photo_where text
);

INSERT INTO plant(name,ename) VALUES ("abc", "enmae");

commit;
