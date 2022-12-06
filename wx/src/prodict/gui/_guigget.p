/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/
/*----------------------------------------------------------------------

File: _guigget.p

Description:   
    Select one or more objects from the list of records in the gatework
    temp-table. 
    This routnine gets called when the field gate-work.gate-flg2 should
    NOT be part of the browse-widget's display.

Input: 
    Workfile gate-work contains info on all the gateway objects.
    p_Gate      Name of the gateway, e.g., "Oracle".
    p_sel-type  "Create"        when creating new schema
                "Differences"   to browse differences
                "Update"        when plain updating schema

Output:
    gatework.gate-flag = "yes"  object to be created/updated/deleted
 
Returns:
    "ok"     if 1 or more tables were chosen.
    "cancel" if user cancelled out.

Author: Laura Stern

Date Created : 07/28/93 

History:
    hutegger    95/03   extented functionality to get used for create, 
                        update and browse differences and extracted
                        body of program into .i-file
    gfs         94/07   Install correct help contexts
    gfs         94/05   Changed Selection-List to Browse

----------------------------------------------------------------------*/

&SCOPED-DEFINE GATE_FLG2 NO
{ prodict/gui/guigget.i }
&UNDEFINE GATE_FLG2
  
/*--------------------------------------------------------------------*/
