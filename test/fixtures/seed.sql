-- Deterministic seed. Re-run after every mutating scenario.
truncate observations, plants;

insert into plants (id, name, scientific_name, status, rating, height_cm, is_native, created_at, tags) values
  ('00000000-0000-0000-0000-000000000001', 'Arnica',          'Arnica montana',   'active',   5,    30.5, true,  '2026-03-01T10:00:00Z', '{"color":"yellow","alt":[1500,2500]}'),
  ('00000000-0000-0000-0000-000000000002', 'Gentiane jaune',  'Gentiana lutea',   'active',   null, 120,  true,  '2026-04-15T08:30:00Z', null),
  ('00000000-0000-0000-0000-000000000003', 'Plante, inconnue','Mystery (sp.)',    'archived', 2,    null, false, '2026-05-20T16:45:00Z', '[]');

insert into observations (id, plant_id, taken_at, note, lat, lon) values
  ('10000000-0000-0000-0000-000000000001', '00000000-0000-0000-0000-000000000001', '2026-06-12T14:32:00Z', 'En fleur',        48.0331, 7.0028),
  ('10000000-0000-0000-0000-000000000002', '00000000-0000-0000-0000-000000000001', '2026-07-03T09:10:00Z', null,              48.0412, 7.0101),
  ('10000000-0000-0000-0000-000000000003', '00000000-0000-0000-0000-000000000002', '2026-06-20T11:00:00Z', 'Rosette, 1 pied', 48.0500, 7.0200),
  ('10000000-0000-0000-0000-000000000004', '00000000-0000-0000-0000-000000000003', '2026-05-21T12:00:00Z', 'A identifier',    null,    null);
