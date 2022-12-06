/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/gate/cmp_nav.i

Description:
    
    generates message for "onject not available"

        
Text-Parameters:
    &nr        Index-Number for error-array (can be ommited)
    &object    {Sequence|Table|Field|Index|Index-Field}  
    &obj       {seq|tbl|fld|idx|idf} for s_ttb_{&obj}
    &objm      {seq|tbl|fld|idx|idf} for l_{&objm}-msg

Output-Parameters:
    none
    
Included in:
    gate/_gat_cmp.p
    gate/_gat_fld.i
    gate/_gat_idf.i
    gate/_gat_idx.i
    gate/_gat_tbl.i

History:
    hutegger    95/03   creation
    
--------------------------------------------------------------------*/        
/*h-*/

       l_sev-msg = l_sev-msg + "    {&object} "
                 + s_ttb_{&obj}.ds_name {&plus} + ": " + chr(10) + chr(9)
                 + l_msg[l_{&objm}-msg[1{&nr}]]        + chr(10).

/*------------------------------------------------------------------*/
