/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: adecomm/_getdbs.p

Description:
    
    creates s_ttb_db-records for all connected physical PROGRESS-Dbs
    and all auto-connect records.
            
Input-Parameters:
    none

Output-Parameters:
    none
    
Used/Modified Shared Objects:
    s_ttb_db    Temp-Table containing a record for EACH AND EVERY
                available DB


Author: Tom Hutegger

History:
    hutegger    95/06   creation
    hutegger    95/08   added dbrcd, empty and dspnm fields

                            
--------------------------------------------------------------------*/
/*h-*/

/*----------------------------  DEFINES  ---------------------------*/

{ prodict/dictvar.i NEW }

define         variable     l_curr-db as integer init 1.
define         variable     l_dbnr    as integer.

{ adecomm/getdbs.i }

/*------------------------  INT.-PROCEDURES  -----------------------*/

/*---------------------------  MAIN-CODE  --------------------------*/


/* determine current DICTDB */

repeat l_dbnr = 1 to NUM-DBS:
  if LDBNAME("DICTDB") = LDBNAME(l_dbnr)
   then assign l_curr-db = l_dbnr.
  end.


/* generate temp-table records for all available dbs */

repeat l_dbnr = 1 to NUM-DBS:

if DBTYPE(l_dbnr) <> "PROGRESS" then next.  /* SchemaHolders later */

  run adecomm/_setalia.p
    ( INPUT l_dbnr
    ).

  run adecomm/_getpdbs.p
    ( INPUT l_dbnr
    ).

  end.


/* reset DICTDB to the db it pointed when we came in, or delete it */

if LDBNAME(l_curr-db) = ?
 then delete alias DICTDB.
 else run adecomm/_setalia.p
        ( INPUT l_curr-db
        ).

/*------------------------------------------------------------------*/
