/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/gate/cmp_sum.i

Description:
    generates the report and puts it into gate-work.gate-edit. If there
    were severe differences (l_sev-msg <> "") then it also initializes 
    gate-work.gate-flag with YES

        
Text-Parameters:
    &object    s_ttb_{seq|tbl}  

Included in:
    gate/_gat_cmp.p

History:
    hutegger    95/03   creation
    
--------------------------------------------------------------------*/        
/*h-*/

  if   l_sev-msg = ""
   and l_int-msg = ""
   and l_ret-msg = ""
   and l_min-msg = ""
   then assign
      gate-work.gate-edit = gate-work.gate-edit 
            + "Object: " + s_ttb_{&object}.pro_name
            + chr(9) + "(" + s_ttb_{&object}.ds_type + " - "
            + ( if    edbtyp <> "ORACLE"
                 and  s_ttb_{&object}.ds_spcl <> ""
                 and  s_ttb_{&object}.ds_spcl <> ?
                 then s_ttb_{&object}.ds_spcl + "."
                 else ""
              )
            + s_ttb_{&object}.ds_user + "."  
            + s_ttb_{&object}.ds_name
            + ( if   edbtyp = "ORACLE"
                 and s_ttb_{&object}.ds_spcl <> ""
                 and s_ttb_{&object}.ds_spcl <> ?
                 then "@" + s_ttb_{&object}.ds_spcl
                 else ""
              )
            + ")" + chr(10) + chr(10) 
            + l_no-diff.
   else assign
      gate-work.gate-edit = "Object: " + s_ttb_{&object}.pro_name
            + chr(9) + "(" + s_ttb_{&object}.ds_type + " - "
            + ( if    edbtyp <> "ORACLE"
                 and  s_ttb_{&object}.ds_spcl <> ""
                 and  s_ttb_{&object}.ds_spcl <> ?
                 then s_ttb_{&object}.ds_spcl + "."
                 else ""
              )
            + s_ttb_{&object}.ds_user + "."  
            + s_ttb_{&object}.ds_name
            + ( if   edbtyp = "ORACLE"
                 and s_ttb_{&object}.ds_spcl <> ""
                 and s_ttb_{&object}.ds_spcl <> ?
                 then "@" + s_ttb_{&object}.ds_spcl
                 else ""
              )
            + ")" + chr(10) + chr(10)
            + ( if  l_sev-msg <> ""
                 or l_int-msg <> ""
                 then l_shupd-msg
                 else ""
              )
            + ( if l_sev-msg <> ""
                 then l_sev-msg-txt + chr(10) + l_sev-msg + chr(10)
                 else ""
              )
            + ( if l_int-msg <> ""
                 then l_int-msg-txt /*+ chr(10) + l_int-msg */ + chr(10)
                 else ""
              )
            + ( if l_ret-msg <> ""
                 then l_ret-msg-txt + chr(10) + l_ret-msg + chr(10)
                 else ""
              )
            + ( if l_min-msg <> ""
                 then l_min-msg-txt + chr(10) + l_min-msg + chr(10)
                 else ""
              )
            + chr(10)
            + "( SH = Schema Holder  NS = Native Schema )"
      gate-work.gate-flag = ( l_sev-msg <> ""
                           or l_int-msg <> "" )
      gate-work.gate-flg2 = ( l_sev-msg <> ""
                           or l_int-msg <> ""
                           or l_ret-msg <> "" ).
      

/*------------------------------------------------------------------*/
