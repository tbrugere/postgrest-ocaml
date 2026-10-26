-- Test schema for the postgrest library's integration fixtures.
-- Loaded into a throwaway Postgres by scripts/record-postgrest-fixtures.sh.
drop schema if exists public cascade;
create schema public;
grant usage on schema public to public;

create role web_anon nologin;
grant usage on schema public to web_anon;

create type plant_status as enum ('active', 'archived');

create table plants (
  id              uuid primary key,
  name            text not null,
  scientific_name text,
  status          plant_status not null default 'active',
  rating          integer,
  height_cm       numeric,
  is_native       boolean not null default false,
  created_at      timestamptz not null default '2026-01-01T00:00:00Z',
  tags            jsonb
);

create table observations (
  id        uuid primary key,
  plant_id  uuid not null references plants (id),
  taken_at  timestamptz not null,
  note      text,
  lat       double precision,
  lon       double precision
);

grant select, insert, update, delete on plants, observations to web_anon;
