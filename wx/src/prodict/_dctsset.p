/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* called from _usrsget.p, used with _dctsget.p */
/* ----------------------------------------------------------------------
File:   prodict/_dctsset.p

Description:
    1. sets drec_db to the currently selected _Db-record and sets
       dict_rog for this db
    2. issues a message if db-type is not supported by the current
       executable
    3. Reads file-list into cache

Input Parameters:
    none
    
Output Parameters:
    none

history:
        95/07   hutegger    Prefixed _Db with DICTDB
---------------------------------------------------------------------- */
/*h-*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE INPUT PARAMETER nam AS CHARACTER NO-UNDO.

DEFINE SHARED VARIABLE fast_track AS LOGICAL. /* FT active? */

DEFINE VARIABLE istrans  AS LOGICAL INITIAL TRUE. /*UNDO (not no-undo!) */
DEFINE VARIABLE i        AS INTEGER NO-UNDO.
DEFINE VARIABLE l_hidden AS LOGICAL NO-UNDO.

/* ------------------------------------------------------------------- */

DO ON ERROR UNDO:
  istrans = FALSE.
  UNDO,LEAVE.
  end.


/* --- Check to see if dictionary is called from within Fast Track --- */

DO i = 1 TO i + 1
  WHILE PROGRAM-NAME(i) <> ?
  AND   NOT fast_track:
  fast_track = ( "ft.p" 
               = SUBSTRING(PROGRAM-NAME(i)
                          ,MAXIMUM(1
                                  ,LENGTH(PROGRAM-NAME(i)) - 3
                                  )
                          ,-1
                          ,"character"
                          )
               ).
  end.


/* -------------------- set drec_db and dict_rog --------------------- */

find DICTDB._Db WHERE DICTDB._Db._Db-name = nam.
assign
  drec_db  = RECID(DICTDB._Db)
  dict_rog = 
      istrans  /* cannot change dict if called from w/i a trans */
      OR PROGRESS = "Query"    /* or used with query product    */
      OR PROGRESS = "Run-Time" /* or used with run-time product */
      OR fast_track            /* or called from within Fast Track */
      OR CAN-DO("READ-ONLY",DBRESTRICTIONS("DICTDB")). /* or read-only DB */

/* (los 12/27/94) */
if NOT CAN-DO(GATEWAYS, DICTDB._Db._Db-type)
 then MESSAGE
    "This module does not support connections to this Data Server type."
    view-as alert-box.


/* -------------------- recreate file-list cache --------------------- */

find first DICTDB._File 
  where DICTDB._File._Db-recid  = drec_db 
  and   DICTDB._File._File-name = user_filename
   no-lock no-error.
assign l_hidden = ( available DICTDB._File
                    and DICTDB._File._Hidden = TRUE).

run prodict/_dctcach.p 
  ( INPUT l_hidden
  ).

/* ------------------------------------------------------------------- */