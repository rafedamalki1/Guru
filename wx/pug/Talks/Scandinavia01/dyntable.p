define variable vTable    as character init "customer".
define variable hTable    as handle.
define variable vField    as character init "country".
define variable hField    as handle.
define variable vFldValue as character init "Australia".
define variable vRowid    as rowid.

find last customer. 
message Custnum Country view-as alert-box.
vRowid = rowid(Customer).
create buffer hTable for table vTable.
hField = hTable:buffer-field(vField).
hTable:find-by-rowid(vRowid).
do transaction:
  assign hField:buffer-value = vFldValue.
end. 
message custnum country view-as alert-box.
