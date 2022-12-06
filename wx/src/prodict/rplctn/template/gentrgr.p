/*

replication trigger wrapper generator

usage: connect to a database, that has tables which are to be replicated
    first step:
        add replication specific information to the table-definition
        * name of the replication program
        * name of the program to be called when a record is to big to fit
          in one single replication-change record
          (in Table Description add: REPLICATION DOT-P <program-name>)
    second step:
        create a subdirectory-tree within your current working-directory:
            rplctn
            rplctn/create
            rplctn/delete
            rplctn/write
    third step:
        run this program
    fifth step:
        double-check that there are no triggers in the generated
        rplctn/reptrig.df file, that would overwrite triggers you
        already have in your schema!
        If you are sure it's save, then load this .df-file into your
        Database.
    forth step:
        double-check that all generated triggers are correct. Compile
        them...

NOTE: This is NOT a load-and-go tool! It is rather just to give you 
    some ideas about what you could do and how, to speed up the
    process....


history:
    10/96   hutegger    creation

*/

define variable l_action-list   as character init "changed,new,deleted".
define variable l_dot-p         as character.
define variable l_event-list    as character init "write,create,delete".
define variable l_event         as character.
define variable l_fldex         as character.
define variable l_i             as integer.
define variable l_max           as integer.
define variable l_ok            as logical.
define variable l_token         as character.

define stream df-file.

/* trigger-definitions */
output stream df-file to value("rplctn/reptrig.df").

for each _File
  where _File._File-name < "_":

/** /
  if  _File._Fil-misc2[6] = ""
   or _File._Fil-misc2[6] = ?
   then next.  /* no replication for this table */
/ **/


  /* 1. step: check for information entered by user into _file-desc */

  assign
    l_i     = index(_File._desc,"REPLICATION FLDEX").
  if l_i <> 0
   then assign
    l_token = substring(_File._desc,l_i + 18,-1,"character")
    l_i     = index(l_token," ")
    l_fldex = substring(l_token,1,l_i - 1,"character").
   else assign
    l_fldex = "".

  assign
    l_i     = index(_File._desc,"REPLICATION DOT-P").
  if l_i <> 0
   then assign
    l_token = substring(_File._desc,l_i + 18,-1,"character")
    l_i     = index(l_token," ")
    l_dot-p = substring(l_token,1,l_i - 1,"character").
   else assign
    l_dot-p = "".


  /* 2. step: determine field to exclude when record too big 
   *    a) select character-field with biggest format (only "x(<int>)")
   *    b) if none found take first raw-field
   *    c) if none found take first character-field
   */

  if l_fldex = ""
   then do:  /* select fldex from _field-records */

    assign l_max = 0.

    for each _Field of _File:

      if _Field._data-type = "character"
       and _field._Format matches "x(*)"
       then do:  /* potential field */

        assign
          l_ok    = true
          l_token = substring(_field._Format
                             ,3
                             ,length(_field._Format,"character") - 3
                             ,"character"
                             ).

        repeat l_i = 1 to length(l_token)
          while l_ok = true:
          assign l_ok = ( lookup(substring(l_token,l_i,1,"character")
                                ,"0,1,2,3,4,5,6,7,8,9"
                                ) <> 0 ).
          end.

        if l_ok
         then do:  /* found x(<int>) format */
          assign
            l_i = integer(l_token).
          if l_i > l_max
           then assign
            l_max   = l_i
            l_fldex = _Field._field-name.
          end.     /* found x(<int>) format */

        end.     /* potential field */

      end.     /* for each _Field of _File */

    /* if fldex is still empty we haven't found any character-field 
     * with format "x(<int>)".
     * So we just take the first raw or character field we find 
     */

    if l_fldex = ""
     then do:  /* take any raw or character field */
      find first _Field of _File
        where _Field._data-type = "raw"
        no-error.
      if not available _Field
       then find first _Field of _File
        where _Field._data-type = "character"
        no-error.
      if available _Field
       then assign
        l_fldex = _field._Field-name.
      end.     /* take any raw or character field */

    end.     /* select fldex from _field-records */


  /* 3. step: generate trigger .p's */


  /* trigger-definitions */
  put stream df-file unformatted
    "UPDATE TABLE """ _File._File-name """"            skip.

  repeat l_i = 1 to 3:
    assign
      l_event = entry(l_i,l_event-list).

    /* trigger-definitions */
    put stream df-file unformatted
      "  TABLE-TRIGGER ""REPLICATION-" CAPS(l_event) 
          """ NO-OVERRIDE PROCEDURE ""rplctn/"
          l_event "/" _File._Dump-name ".p"" CRC ""?"""   skip.

    /* trigger code */
    output to value("rplctn/" + l_event + "/" + _File._Dump-name + ".p").
    put unformatted
      "/* replication trigger for "
          entry(l_i,l_action-list)
          " records of the " 
          _File._File-name + " table */"      skip(1)
      "TRIGGER PROCEDURE FOR REPLICATION-"
      l_event " OF " _File._File-name "."     skip(1)
      chr(123) "rplctn/reptrgr.i"             skip
      "  &event = """ l_event """"            skip
      "  &table = """ _File._File-name """"   skip
      "  &fldex = """ l_fldex " """           skip
      "  &dot-p = """ l_dot-p " """           skip
      "  " chr(125)                           skip(1)
      "/*" fill("-",66) "*/"                  skip.
    end.  

  end.
