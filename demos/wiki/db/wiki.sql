
create table fdocs (
  name text primary key,
  content text,
  modified_on timestamp,
  modified_by text );

create table users (
  user text primary key,
  password text);

create table fdocs_archive (
  name text primary key,
  content text,
  modified_on timestamp,
  modified_by text );


  