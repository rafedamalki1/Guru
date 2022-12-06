/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _ora_seq.p - Oracle-to-Progress schema conversion program */

DEFINE INPUT        PARAMETER ora-name AS CHARACTER   NO-UNDO.
DEFINE INPUT        PARAMETER ora-user AS CHARACTER   NO-UNDO.
DEFINE INPUT        PARAMETER ora-prog AS CHARACTER   NO-UNDO.
DEFINE INPUT        PARAMETER ora-link AS CHARACTER   NO-UNDO.

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/ora/ora_ctl.i 6 }

define variable l_int       as integer   no-undo.
define variable l_int1      as integer   no-undo.
define variable l_link      as character no-undo.
define variable l_name      as character no-undo.

/*------------------------------------------------------------------*/

assign
  l_link = ( if ora-link <> ""
              then ora-link
              else ?
           )
  l_name = ora-prog.
            
find DICTDB._Sequence
  where DICTDB._Sequence._Db-Recid    = drec_db
  and   DICTDB._Sequence._Seq-Name    = l_name
  and   DICTDB._Sequence._Seq-misc[8] = l_link
  NO-ERROR.

if NOT AVAILABLE DICTDB._Sequence
 then do:
 
  /* the sequence for ORACLE-7 gets created in _ora_lks.p
   * so for oracle-7 this branch is a do-never-branch <hutegger 94/11>
   */

  assign
    l_int  = min(29,length(l_name,"character"))
    l_int1 = 0.
  repeat while can-find(
                    first DICTDB._Sequence
                    where DICTDB._Sequence._Db-Recid = drec_db
                    and   DICTDB._Sequence._Seq-Name = l_name
                       ):
    assign
      l_int1 = l_int1 - 1
      l_name = substring(l_name,1,l_int,"character")
             + string(l_int1,"-99").
    end.
  
  create DICTDB._Sequence.
  assign
    DICTDB._Sequence._Db-Recid    = drec_db
    DICTDB._Sequence._Seq-Name    = l_name
    DICTDB._Sequence._Seq-misc[8] = l_link.
  END.

ASSIGN
  DICTDB._Sequence._Seq-Misc[1] = ora-name
  DICTDB._Sequence._Seq-Misc[2] = ora-user.


RETURN.

/*------------------------------------------------------------------*/
