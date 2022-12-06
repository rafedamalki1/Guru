/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: prodict/load_df.p

    End-user entry point for Admin Tool's "Load .df file" utility
      
Input Parameters:
    df-file-name    The name of the .df file to be loaded into DICTDB
    
Output Parameters:
    none
    
Used/Modified Shared Objects:
    defines several NEW SHARED objects
    
History:
    tomn        08/29/95    Added check for codepage spec in .df file
                            trailer; Leaves as UNDEFINED if none specified


----------------------------------------------------------------------------*/
/*h-*/

/*==========================  DEFINITIONS ===========================*/

DEFINE INPUT PARAMETER df-file-name AS CHARACTER NO-UNDO.

{ prodict/user/uservar.i NEW }
{ prodict/dictvar.i NEW }

DEFINE VAR save_ab       AS LOGICAL        NO-UNDO.
DEFINE VAR codepage AS CHARACTER      NO-UNDO FORMAT "X(20)".
DEFINE VAR lvar     AS CHAR EXTENT 10 NO-UNDO.
DEFINE VAR lvar#    AS INT            NO-UNDO.
DEFINE VAR i        AS INT            NO-UNDO.


/*========================= MAINLINE CODE ============================*/

/*check runtime create privileges*/
FOR EACH _File
  WHERE _File._File-number >= -4 AND _File._File-number <= -1:
  IF CAN-DO(_File._Can-write,USERID("DICTDB"))
           AND CAN-DO(_File._Can-create,USERID("DICTDB")) THEN NEXT.
  MESSAGE "You do not have permission to load table definitions.".
  RETURN.
END. 

FIND FIRST _Db WHERE _Db._Db-local.
ASSIGN
  user_dbname = LDBNAME("DICTDB")
  user_dbtype = DBTYPE("DICTDB")
  drec_db     = RECID(_Db)
  user_env[2] = df-file-name
  user_env[6] = "b"
  user_env[8] = user_dbname
  codepage    = "".  
  
RUN read-cp.  /* get codepage out of the .df file trailer (tomn 8/28/95) */ 
user_env[10] = codepage.

SESSION:APPL-ALERT-BOXES = NO.
save_ab = SESSION:APPL-ALERT-BOXES.

DO TRANSACTION ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
  user_path = "*".
  RUN "prodict/dump/_lodv5df.p".
  IF user_path = "*" THEN RUN "prodict/dump/_lodsddl.p".
  IF user_path = "*R" then UNDO, LEAVE.
END.

SESSION:APPL-ALERT-BOXES = save_ab.
RETURN.

/*====================== INTERNAL PROCEDURES =========================*/

PROCEDURE read-cp.
  /* Read trailer of file and find codepage */
  /* (partially stolen from lodtrail.i)     */
  
  DEFINE VARIABLE i     AS INT            NO-UNDO.

  INPUT FROM VALUE(user_env[2]) NO-ECHO NO-MAP.
  SEEK INPUT TO END.
  SEEK INPUT TO SEEK(INPUT) - 11. /* position to beginning of last line */

  READKEY PAUSE 0.
  ASSIGN
    lvar# = 0
    lvar  = ""
    i     = 0.
  DO WHILE LASTKEY <> 13 AND i <> ?: /* get byte count (last line) */
    i = (IF LASTKEY > 47 AND LASTKEY < 58 
          THEN i * 10 + LASTKEY - 48
          ELSE ?).
    READKEY PAUSE 0.
  END.
  IF i > 0 then run get_psc. /* get it */
  ELSE RUN find_psc. /* look for it */
  INPUT CLOSE.
  DO i = 1 TO lvar#:
    IF lvar[i] BEGINS "codepage=" THEN codepage = TRIM(SUBSTRING(lvar[i],10,-1,"character":U)).
  END.
END PROCEDURE.

PROCEDURE get_psc:
  /* using the byte count, we scoot right down there and look for
   * the beginning of the trailer ("PSC"). If we don't find it, we
   * will go and look for it.
   */
   
  DEFINE VARIABLE rc AS LOGICAL INITIAL no.
  _psc:
  DO ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
    SEEK INPUT TO i. /* skip to beginning of "PSC" in file */
    READKEY PAUSE 0. IF LASTKEY <> ASC("P") THEN LEAVE _psc. /* not there!*/
    READKEY PAUSE 0. IF LASTKEY <> ASC("S") THEN LEAVE _psc.
    READKEY PAUSE 0. IF LASTKEY <> ASC("C") THEN LEAVE _psc.
    ASSIGN rc = yes. /* found it! */
    RUN read_bits (INPUT i). /* read trailer bits */
  END.
  IF NOT rc THEN RUN find_psc. /* look for it */
END PROCEDURE.

PROCEDURE find_psc:
  /* If the bytecount at the end of the file is wrong, we will jump
   * back the maximum number of bytes in a trailer and start looking
   * from there. If we still don't find it then tough luck.
   * NOTE: Variable p holds the number of bytes to roll back. AS of
   * 7/21/94, the max size of a trailer (.d) is 204 bytes, if you add
   * anything to this trailer, you must change this number to reflect
   * the number of bytes you added. I'll use 256 to add a little padding. (gfs)
   */
  DEFINE VARIABLE p AS INTEGER INITIAL 256. /* really 204, added extra just in case */
  DEFINE VARIABLE l AS INTEGER.             /* last char position */
  
  SEEK INPUT TO END.
  ASSIGN l = SEEK(INPUT). /* EOF */
  SEEK INPUT TO SEEK(INPUT) - MINIMUM(p,l). /* take p, or size of file */
  IF SEEK(INPUT) = ? THEN RETURN.
  _scan:
  REPEAT ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
    READKEY PAUSE 0.
    p = SEEK(INPUT). /* save off where we are looking */
    IF LASTKEY = ASC("P") THEN DO:
       READKEY PAUSE 0.
       IF LASTKEY <> ASC("S") THEN NEXT.
       ELSE DO: /* found "PS" */
         READKEY PAUSE 0.
         IF LASTKEY <> ASC("C") THEN NEXT.
         ELSE DO: /* found "PSC"! */
           RUN read_bits (INPUT p - 1).
           LEAVE.
         END. /* IF "C" */
       END. /* IF "S" */    
    END. /* IF "P" */
    ELSE IF p >= l THEN LEAVE _scan. /* at EOF, so give up */
  END. /* repeat */
END.

PROCEDURE read_bits:
  /* reads trailer given a starting position 
   */ 
  DEFINE INPUT PARAMETER i as INTEGER. /* "SEEK TO" location */
    
  SEEK INPUT TO i.
  REPEAT:
    IMPORT lvar[lvar# + 1].
    lvar# = lvar# + 1.
  END.
END PROCEDURE. 

/*========================== END OF load_df.p ==========================*/


