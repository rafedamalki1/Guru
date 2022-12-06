/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/_dctsget.p

Description:
    
    populates the shared variables that hold the available dbs info.
            
Input-Parameters:
    none

Output-Parameters:
    none
    
Used/Modified Shared Objects:
    cache_db_l  array of logical db-names
    cache_db_p  array of physical db-names
    cache_db_s  array of names of the db, thats the schemaholder
    cache_db_t  array of db-types
    cache_db#   index of the current working-db to the above arrays


Author: Tom Hutegger

History:
    hutegger    94/06   creation (derived from & replacing old version)

                            
--------------------------------------------------------------------*/
/*h-*/

/*----------------------------  DEFINES  ---------------------------*/

{ prodict/dictvar.i }

{ adecomm/getdbs.i
  &new = "NEW"
  }

/*------------------------  INT.-PROCEDURES  -----------------------*/

/*---------------------------  MAIN-CODE  --------------------------*/


ASSIGN
  cache_db#  = 0
  cache_db_l = ""
  cache_db_p = ""
  cache_db_s = ""
  cache_db_t = "".

if NUM-DBS > 0 
 then do:
  RUN adecomm/_dictdb.p.
  RUN adecomm/_getdbs.p.
  end.
  
for each s_ttb_db:
  
  /* get rid of older versions, because we can't handle them    */
  /* if s_ttb_db.vrsn <> "8" then next.                         */

  assign
    cache_db#             = cache_db# + 1
    cache_db_e[cache_db#] = s_ttb_db.dbtyp
                          + ( if s_ttb_db.vrsn < "8" 
                                then "/V" + s_ttb_db.vrsn
                                else ""
                            )
    cache_db_l[cache_db#] = s_ttb_db.ldbnm
    cache_db_p[cache_db#] = ( if s_ttb_db.cnnctd
                                then s_ttb_db.pdbnm
                                else ?
                            )
    cache_db_s[cache_db#] = s_ttb_db.sdbnm
    cache_db_t[cache_db#] = {adecomm/ds_type.i
                               &direction = "etoi"
                               &from-type = "s_ttb_db.dbtyp"
                            }
    cache_db_t[cache_db#] = cache_db_t[cache_db#]
                          + ( if s_ttb_db.vrsn < "8" 
                                then "/V" + s_ttb_db.vrsn
                                else ""
                            ).
  end.


/*-------------------------- old version --------------------------* /
DO dbcnt = 1 TO NUM-DBS:
  IF DBTYPE(dbcnt) <> "PROGRESS" THEN NEXT.
  IF cache_db# = EXTENT(cache_db_s) THEN LEAVE.
  ASSIGN
    prov5                 = DBVERSION(dbcnt) + DBTYPE(dbcnt) = "5PROGRESS"
    prov6                 = DBVERSION(dbcnt) + DBTYPE(dbcnt) = "6PROGRESS"
  /*alnam                 = (IF alnam = ? THEN LDBNAME(dbcnt) ELSE alnam)*/
    cache_db#             = cache_db# + 1
    cache_db_s[cache_db#] = SDBNAME(dbcnt)
    cache_db_l[cache_db#] = LDBNAME(dbcnt)
    cache_db_p[cache_db#] = PDBNAME(dbcnt)
    cache_db_t[cache_db#] = DBTYPE(dbcnt)
                          + (IF prov5 THEN "/V5" ELSE "")
                          + (IF prov6 THEN "/V6" ELSE "").
  CREATE ALIAS "DICTDB" FOR DATABASE VALUE(LDBNAME(dbcnt)) NO-ERROR.
  IF cache_db_t[cache_db#] = "PROGRESS" THEN RUN "prodict/_dctssub.p".
END.

IF alnam = ? THEN
  DELETE ALIAS "DICTDB".
ELSE
  CREATE ALIAS "DICTDB" FOR DATABASE VALUE(alnam) NO-ERROR.

RETURN.
/ *-------------------------- old version --------------------------*/


/*------------------------------------------------------------------*/
