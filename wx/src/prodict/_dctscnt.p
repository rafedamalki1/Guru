/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File:
    prodict/_dctscnt.p

Description:
    counts the user-defined tables in db with ldbname p_dbname
    if the database is version 6 or 5 the number will be 0

Input Parameter:
    p_ldbname   logical name of db to count its files

Output Parameter:
    p_file-nr   number of user defined files in that database

Used/Modified Shared Info:
    none

History:
    95/08       changed 1. parameter to logical db-name
                and changed the overall structure to have defined
                output-behaviour and better sanity testing on the 
                db to be used

--------------------------------------------------------------------*/
/*h-*/

/*----------------------------  DEFINES  ---------------------------*/

/*{ prodict/dictvar.i }*/

DEFINE INPUT  PARAMETER p_ldbname  AS character NO-UNDO.
DEFINE OUTPUT PARAMETER p_file-nr  AS integer   NO-UNDO.


/*---------------------------  TRIGGERS  ---------------------------*/

/*------------------------  INT.-PROCEDURES  -----------------------*/

/*------------------------  INITIALIZATIONS  -----------------------*/

/*---------------------------  MAIN-CODE  --------------------------*/


if  DBVERSION(p_ldbname) = "5"
 or DBVERSION(p_ldbname) = "6"
 then assign p_file-nr = 0.
 
 else do:  /* DICTDB is a valid db */

  if DBTYPE(p_ldbname) = "PROGRESS"
   then find DICTDB._Db where DICTDB._Db._Db-name = ?.
   else find DICTDB._Db where DICTDB._Db._Db-name = p_ldbname.

  /* we are counting USER files, not UNHIDDEN files */
  for each DICTDB._File of DICTDB._Db
    where DICTDB._File._File-num > 0
    p_file-nr = 1 to p_file-nr + 1:
    end.
    
  end.     /* DICTDB is a valid db */

/*------------------------------------------------------------------*/
