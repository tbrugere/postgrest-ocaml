TODO one day or if I need it: (agents: don’t touch this part of the code)

- handle counts
- handle timezones. The solution is simple: use `type time = Ptime.t * Ptime.tz_offset_s`. (holding a UTC time and a timezone.). Alternatively I could use `type datetime = Ptime.date * Ptime.time`, but I don’t see the point.
- RPCs

