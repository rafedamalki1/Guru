define variable vtable     as char init "customer".
define variable hTable     as handle.
define variable hField     as handle.
define variable vField     as char init "country".
define variable hQuery     as handle.

create query hQuery.
create buffer htable for table vtable.
hField = hTable:buffer-field(vField).
hQuery:set-buffers(hTable).
hQuery:query-prepare("for each " + vTable).
hQuery:query-open().
repeat:
  hQuery:get-next().
  if not hQuery:query-off-end then
   display hField:buffer-value 
    format "x(20)" label "Country"
     with use-text 10 down three-d.
  else leave.
end.
