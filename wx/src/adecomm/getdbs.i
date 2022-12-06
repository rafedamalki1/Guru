/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: adecomm/getdbs.i

Description:
    
    definition of s_ttb_db - Temp-Table
            
Input-Parameters:
    none

Output-Parameters:
    none
    
included in:
    adecomm/_getdbs.p
    adecomm/_getfdbs.p
    prodict/_dctsget.p
    adedict/DB/_getdbs.p

    
Author: Tom Hutegger

History:
    hutegger    95/06   creation
    hutegger    95/08   added dbrcd, empty and dspnm fields

                            
--------------------------------------------------------------------*/
/*h-*/

def {&new} shared temp-table   s_ttb_db
        field               cnnctd      as logical   format "yes/no"
        field               dbnr        as integer   format "zz9"
        field               dbtyp       as character format "x(12)"
        field               dspnm       as character format "x(20)"
        field               ldbnm       as character format "x(12)"
        field               pdbnm       as character format "x(40)"
        field               sdbnm       as character format "x(12)"
        field               vrsn        as character format "x"
        field               empty       as logical   format "yes/no"
        field               dbrcd       as recid
        field               local       as logical   format "local/holder"
        index               upi         is primary unique
                                        dbnr ldbnm.

/*------------------------------------------------------------------*/
