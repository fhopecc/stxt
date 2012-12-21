create table assets(
    id integer primary key autoincrement, 
    name text, 
    type text,
    location text,
    ip text,
    memo text);

insert into assets (name, type)
values("abc", "server");

commit;
