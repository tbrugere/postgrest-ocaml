-- Example schema used in the README and the docs: authors, their books, and
-- a lookup table of genres.

-- A lookup table: postgrest-codegen generates a Genres module from its
-- values, and books.genre is typed Genres.t.
create table genres (
  value text primary key
);
comment on table genres is 'metadata: kind=lookup';
insert into genres (value) values ('essay'), ('novel'), ('poetry');

create table authors (
  id   uuid primary key default gen_random_uuid(),
  name text not null
);

create table books (
  id        uuid primary key default gen_random_uuid(),
  title     text not null,
  author_id uuid not null references authors (id),
  genre     text not null references genres (value),
  year      integer not null,
  added_at  timestamptz not null default now()
);
