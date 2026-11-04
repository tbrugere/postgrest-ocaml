-- The catalog postgrest-codegen reads: one row per table of a schema, a
-- pretty-printed JSON object, so that the committed output diffs by line.
--
--   psql "$DB_URL" -X -q -At -v ON_ERROR_STOP=1 -f catalog.sql > catalog.json
--
-- Plain SQL for any client: two functions created in pg_temp (dropped with
-- the session, so nothing is left in the database; the connection needs the
-- TEMP privilege and a writable transaction), then one select. The schema is
-- the one named in the `params` CTE below.
--
-- Each object has the table's `name` and `comment`, its `columns` in column
-- order, and `values`: the sorted `value` column of a lookup table (its
-- comment's metadata line says kind=lookup), null for any other table.
-- A foreign key is reported only when it has a single column and targets a
-- table of the same schema (the first by name if a column has several): the
-- generated relationships name nothing else.

-- Whether a table comment has a metadata line with the kind=lookup token,
-- split as postgrest-codegen's Metadata splits it (commas, spaces, tabs).
create or replace function pg_temp.is_lookup(comment text)
returns boolean
language sql
immutable
as $$
  select coalesce(
    comment ~ '(^|\n)metadata:([^\n]*[ ,\t\r])?kind=lookup([ ,\t\r\n]|$)',
    false)
$$;

-- A lookup table's values. Dynamic SQL, because plain SQL cannot select
-- from a table named by a row.
create or replace function pg_temp.lookup_values(
  schema_name name,
  table_name name)
returns jsonb
language plpgsql
stable
as $$
declare
  result jsonb;
begin
  execute format(
    'select coalesce(jsonb_agg(value::text order by value::text collate "C"),'
    ' ''[]'') from %I.%I',
    schema_name, table_name)
  into result;
  return result;
end
$$;

with params as (
  select 'public'::regnamespace as schema
)
select jsonb_pretty(jsonb_build_object(
  'name', c.relname,
  'comment', obj_description(c.oid, 'pg_class'),
  'columns', columns.list,
  'values', case
    when pg_temp.is_lookup(obj_description(c.oid, 'pg_class'))
      then pg_temp.lookup_values(p.schema::name, c.relname)
  end))
from params p
join pg_class c on c.relnamespace = p.schema
cross join lateral (
  -- An identity GENERATED ALWAYS is reported as generated: like a generated
  -- column, it rejects the value a write would give it.
  select coalesce(jsonb_agg(jsonb_build_object(
           'name', a.attname,
           'type', format_type(a.atttypid, a.atttypmod),
           'not_null', a.attnotnull,
           'has_default', a.atthasdef or a.attidentity <> '',
           'is_generated', a.attgenerated <> '' or a.attidentity = 'a',
           'is_primary_key', exists (
             select from pg_constraint k
             where k.conrelid = c.oid and k.contype = 'p'
               and k.conkey = array[a.attnum]),
           'is_unique', exists (
             select from pg_constraint k
             where k.conrelid = c.oid and k.contype in ('p', 'u')
               and k.conkey = array[a.attnum]),
           'comment', col_description(c.oid, a.attnum),
           'fk_table', (
             select f.relname
             from pg_constraint k
             join pg_class f on f.oid = k.confrelid
             where k.conrelid = c.oid and k.contype = 'f'
               -- not the copies Postgres adds per partition of a
               -- partitioned target
               and k.conparentid = 0
               and k.conkey = array[a.attnum]
               and f.relnamespace = c.relnamespace
             order by k.conname collate "C"
             limit 1))
         order by a.attnum), '[]') as list
  from pg_attribute a
  where a.attrelid = c.oid and a.attnum > 0 and not a.attisdropped
) columns
where c.relkind in ('r', 'p')
  and not c.relispartition
order by c.relname collate "C";
