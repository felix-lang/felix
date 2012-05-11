
create table fdocs (
  name text primary key,
  content text,
  modified_on timestamp,
  modified_by text );

create table users (
  user text primary key,
  password text,
  grp text,
  name text,  
  email text
);

create table fdocs_archive (
  name text primary key,
  content text,
  modified_on timestamp,
  modified_by text );

create table authorized_paths (
  path text,
  grp text);

create table grp (
  grp text,
  description);
 
create table locks (
path text primary key,
user text,
ts timestamp);

create table paths (
path text ,
grp txt,
lvl text,
primary key(path,grp));

