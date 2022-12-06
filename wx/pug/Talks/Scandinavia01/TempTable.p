/* temptable.p 
   This program is called from the demo program simplewin.w.
   It contains three internal procedures: 
   BuildTable builds the local copy of the temp table from the
   information it receives.
   FindRecord locates a record based on the field data it receives
   CreateRecord builds a DB  record and populates it from the temp table
   It also contains a function which passes the table name to the procedure level   
*/
define variable hTempTable       as handle.
define variable hTTBuffer        as handle.
define variable TableName        as char.
define variable Cntr             as int.

procedure BuildTable:
  define input parameter pFieldList as char. 
  define input parameter pDataTypes as char.
  define input parameter pFormats   as char.
  define input parameter pLabels    as char.
  /* the client sends us a list of field names, separated by chr(1)
     data types, formats and labels;
   */

  create temp-table hTempTable.
  do cntr = 1 to num-entries(pFieldList,chr(1)):
    hTempTable:add-new-field
    (entry(cntr,pFieldList,chr(1)) ,   /* name */
     entry(cntr,pDataTypes,chr(1)) ,   /* data type */
     0,                                /* extents */
     entry(cntr,pFormats,chr(1)),      /* format */
     "",                               /* initial */
     entry(cntr,pLabels,chr(1)) ).     /* label */
  end.
  /* add one more dummy field to hold the row of the DB record */
  hTempTable:add-new-field("DBRowid","rowid").
  
  /* when done, 'prepare' the structure for use */
  hTempTable:temp-table-prepare("t" + TableName).

end procedure.

procedure GetData:
  define input parameter pFieldList as char.
  define input parameter pValueList as char.
  define output parameter table-handle OutTable.
  define variable hDBTable as handle.
  define variable hTTField as handle.
  define variable FieldName as char.
  define variable hDBField as handle.
  define variable QueryPhrase as char.
  define variable hQuery      as handle.
  
  create buffer hDBTable for table TableName.
  create query hQuery.
  hquery:set-buffers(hDBtable).
  /* set the variable up as the temp table BUFFER */
  hTTBuffer = hTempTable:default-buffer-handle.
  
  /* build the query phrase from the values sent from the interface;
     this simple example assumes that all values are character.
     Otherwise, you would have to check the datatypes for each 
     value, transform the value, and change the operator from 
     begins to something else  */
  do cntr = 1 to num-entries(pFieldList,chr(1)):
    if entry(cntr,pValueList,chr(1)) <> "" then do:
      if QueryPhrase <> "" then QueryPhrase = QueryPhrase + " and ".
      else QueryPhrase =  " where ".
      QueryPhrase = QueryPhrase + 
        substring(entry(cntr,pFieldList,chr(1)),2) + 
        " begins '" + entry(cntr,pValueList,chr(1)) + "'".
    end.
  end.
  /* now set up the 'front' part of the query phrase */
  QueryPhrase = "for each " + TableName + QueryPhrase.

 
  hQuery:query-prepare(QueryPhrase).
  hQUery:query-open().
 
  repeat:
    hQuery:get-next().
    if not hquery:query-off-end then do:
      /* create and populate the temp table */
      hTTBuffer:buffer-create().
      /* walk through the fields and get the field name,
         then put the temp table value into the DB  */
      do Cntr = 1 to hTTBuffer:num-fields - 1: /* leave off the rowid field */
         
        assign
         /* point to the field from the temp table */
         hTTField              = hTTBuffer:buffer-field(cntr)
         FieldName             = substring(hTTField:name,2) /* lop off the leading character */
          /* use the name to point to the DB field */
         hDBField              = hDBTable:buffer-field(FieldName)
          /* populate the Temp table field from the DB field */
         hTTField:buffer-value = hDBField:buffer-value.
         .

      end.
      /* point to the rowid field and put the DB record's rowid in it */
      assign
      hTTField              = hTTBuffer:buffer-field("DBRowid")
      hTTField:buffer-value = hDBTable:rowid.
    end.
    else leave.
  end.
  OutTable = hTempTable.
end procedure.

procedure PopulateTable:
  define input parameter pFieldValues as char.
  
  define variable hField as handle.
  /* point to a BUFFER for the temp table */
  hTTBuffer = hTempTable:default-buffer-handle.
  hTTBuffer:buffer-create().
  /* the buffer will match the incoming field list because
     that is where the structure came from in the first place */
  do Cntr = 1 to num-entries(pFieldValues,chr(1)):
     assign
     hField               = hTTBuffer:buffer-field(cntr)
     hField:buffer-value  = entry(cntr,pFieldValues,chr(1)).
  end.

end procedure.

procedure CreateRecord:
    define input parameter pValueList as char.
    define input parameter pRowid     as rowid. /* comes from the T-T field with DB rowid in it */

    define variable hDBTable  as handle. /* handle for buffer to DB table */
    define variable hTTField  as handle. /* handle for TT field */
    define variable hDBField  as handle. /* handle for DB field */
    define variable FieldName as char.   /* name of DB field */
   
   create buffer hDBTable for table TableName. /* customer, etc */

   do transaction:
     hDBTable:find-by-rowid(pRowid).

      /* walk through the values sent */
      do Cntr = 1 to num-entries(pValueList,chr(1)):
        assign
          /* point to the current field */
        hTTField              = hTTBuffer:buffer-field(cntr)
        /* the temp table field is the letter "T" + the DB field name */
        FieldName             = substring(hTTField:name,2)
         /* get the corresponding DB field */
        hDBField              = hDBTable:buffer-field(FieldName)
        hDBField:buffer-value = entry(cntr,pValueList,chr(1)).
      end. /* field loop */
    end. /* transaction */
end procedure.

function SetTableName returns logical (pTableName as char):
  /* pass it up to the procedure level */
   TableName = pTableName.
   
end function.
