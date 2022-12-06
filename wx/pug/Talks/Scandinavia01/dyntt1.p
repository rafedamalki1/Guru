define variable TableName        as char.
TableName = "tCustomer".

define temp-table FieldList /* simulates DB Table */
  field FieldName as char
  field DataType  as char.
define variable        hTempTable as handle.
create Fieldlist.
assign fieldname = "country"
       datatype  = "character".
 
DEFINE VARIABLE nytabh AS HANDLE NO-UNDO.  
DEFINE VARIABLE nyfalth AS HANDLE NO-UNDO.  
create temp-table hTempTable.
for each FieldList:
   hTempTable:add-new-field("t" +
   FieldList.FieldName,
   fieldList.DataType ).
end.

hTempTable:temp-table-prepare(TableName).
nytabh = hTempTable:default-buffer-handle.
nytabh:BUFFER-CREATE().
nyfalth = nytabh:BUFFER-FIELD("tcountry").
nyfalth:BUFFER-VALUE() = "hej".
DISPLAY  nyfalth:BUFFER-VALUE().


