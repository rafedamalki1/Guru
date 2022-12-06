/*----------------------------------------------------------------------

Standard Trigger for Replication

Usage: reptrg.i
        &table = <name of the table of this trigger>
        &event = <write, create or delete>
        &fldex = <fields to be excluded in first log-entry and
                  moved into second log entry, when record to big>
        &dot-p = <Program to be called when record to big>

Note: If record is to big we provide two mechanisms to deal with that.
    a)  includer provides a .p-file name which will deal with this
        situation
    b)  includer provides the name of a field. This field should be a
        character or raw field (i.e variable length starage), which
        likely has a lot of data in it. In this case, the trigger will 
        produce two zz-rplctn-chng records. One with the whole record
        EXCEPT the specified field. The other with ONLY the specified 
        field. In order to differentiate between thos two, the first
        wil have the table-name as table-name. The second will have
        the table-name concatenated with "|2" as table-name.

History:
    10/96   hutegger    creation

----------------------------------------------------------------------*/

&global-define g_16KB  2147483647


define variable l_i       as integer.
define variable l_ta#     as integer.
define variable l_time    as integer.
define variable l_rawfld  as raw.

create replicate-chng.

assign
  l_time                    = time
  l_ta# = dbtaskid(ldbname(buffer replicate-chng))
  replicate-chng.event      = "{&event}"
  replicate-chng.table-name = "{&table}"
  replicate-chng.trans-id   = l_ta#
  replicate-chng.create-usr = userid(ldbname(buffer replicate-chng))
  replicate-chng.create-dt  = today
  replicate-chng.create-tm  = string(l_time)
  l_i = {&g_16KB} - ( record-length(replicate-chng) / 2) 
  .

if (record-length({&table}) / 2) >= l_i
 then do:  /* record too big */

/* if includer provided a field to be excluded in case of a too big
 * record we try to exclude it and store the record in two separate
 * replicate-chng records. We will make sure, that the creation-time
 * of both is the same, so the log-interpreter will be able to
 * concatenate them together again
 */
                &if "{&fldex}" <> ""
                    &then

  define temp-table l_ttb_{&table} like {&table}.
  define temp-table l_ttb_charfld field fld as character.
  buffer-copy {&table} to l_ttb_{&table}.                
  assign l_ttb_{&table}.{&fldex} = "".
  raw-transfer l_ttb_{&table} to l_rawfld.

  if (length(l_rawfld) / 2) < l_i
   then do:  /* splitting solves size-problem */

    assign
      replicate-chng.data-record = l_rawfld.

    create l_ttb_charfld.
    create replicate-chng.
    assign
      l_ttb_charfld.fld          = {&table}.{&fldex}
      replicate-chng.event       = "{&event}"
      replicate-chng.table-name  = "{&table}|2"
      replicate-chng.trans-id    = l_ta#
      replicate-chng.create-usr  = userid(ldbname(buffer replicate-chng))
      replicate-chng.create-dt   = today
      replicate-chng.create-tm   = string(l_time)
      .
    raw-transfer l_ttb_charfld to replicate-chng.data-record.

    if l_time = time then pause 1.

    end.     /* splitting solves size-problem */

   else do:  /* record still too big */

  /* if includer provided name of .p-file try to run it */
                        &if "{&dot-p}" <> ""
                            &then
    delete replicate-chng.
    if search("{&dot-p}") <> ?
     then run {&dot-p}.
     else return error.
                            &else
  /* if includer didn't provide .p-file, return with error */
    return error.
                            &endif

    end.     /* record still too big */


/* otherwise, check if includer provided name of .p-file. If so, then
 * try to run it */
                &elseif "{&dot-p}" <> ""
                    &then
  delete replicate-chng.
  if search({&dot-p}) <> ?
   then run {&dot-p}.
   else return error.
                    &else
/* if includer didn't provide .p-file, return with error */
  return error.
                    &endif

  end.     /* record too big */

 else do:  /* create one replication-log entry */

  raw-transfer {&table} to replicate-chng.data-record.

  end.     /* create one replication-log entry */

/*--------------------------------------------------------------------*/
