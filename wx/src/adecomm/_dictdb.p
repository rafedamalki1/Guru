/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*-----------------------------------------------------------------------

File: adecomm/_dictdb.p

Description:   
   This procedure checks the setting of DICTDB. In case it's ? or pointing
   to a non-progress db it resets it to the first PROGRESS-db it finds

Shared Output:
   DICTDB       pointing at valid db

History:
    95/08   hutegger    creation

-----------------------------------------------------------------------*/

define variable l_dbnr          as integer.

/*-----------------------------  TRIGGERS  ----------------------------*/

/*--------------------------  INT.-PROCEDURES  ------------------------*/

/*--------------------------  INITIALIZATIONS  ------------------------*/

/*-----------------------------  MAIN-CODE  ---------------------------*/

if NUM-DBS > 0
 AND LDBNAME("DICTDB") = ?
 OR  DBTYPE("DICTDB") <> "PROGRESS"
 then do:  /* change/set DICTDB alias */

  repeat while l_dbnr < NUM-DBS
   and DBTYPE(l_dbnr) <> "PROGRESS":
    assign l_dbnr = l_dbnr + 1.
    end.

  if l_dbnr <= NUM-DBS
   then create alias DICTDB for database value(LDBNAME(l_dbnr)).
   
  end.     /* change/set DICTDB alias */

/*---------------------------------------------------------------------*/
