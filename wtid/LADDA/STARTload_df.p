/* STARTload_df.p
prowin32.exe -rx -p STARTload_df.p
https://knowledgebase.progress.com/articles/Knowledge/P147079
COMPILE XCODE
xcode [ -k key ] -d directory [ files] [ - ]

*/
/*==========================  DEFINITIONS ===========================*/

DEFINE INPUT PARAMETER df-file-name AS CHARACTER NO-UNDO.

{ ladda\dictvar.i NEW }
{ ladda\uservar.i NEW }

DEFINE VARIABLE save_ab     AS LOGICAL        NO-UNDO.
DEFINE VARIABLE codepage    AS CHARACTER      NO-UNDO FORMAT "X(20)".
DEFINE VARIABLE lvar        AS CHAR EXTENT 10 NO-UNDO.
DEFINE VARIABLE lvar#       AS INT            NO-UNDO.
DEFINE VARIABLE i64           AS INT64          NO-UNDO.
DEFINE VARIABLE old-session AS CHARACTER      NO-UNDO INITIAL ?.
DEFINE VARIABLE counter     AS INTEGER        NO-UNDO INITIAL 1.
 DEFINE VARIABLE I AS INTEGER     NO-UNDO.

/*========================= MAINLINE CODE ============================*/

/*check runtime create privileges*/
FOR EACH _File
  WHERE _File._File-number >= -4 AND _File._File-number <= -1:
  IF CAN-DO(_File._Can-write,USERID("DICTDB"))
           AND CAN-DO(_File._Can-create,USERID("DICTDB")) THEN NEXT.
  MESSAGE "You do not have permission to load table definitions.".
  RETURN.
END. 

FIND FIRST _Db WHERE _Db._Db-local NO-LOCK.
ASSIGN
  user_dbname = LDBNAME("DICTDB")
  user_dbtype = DBTYPE("DICTDB")
  drec_db     = RECID(_Db)  
  user_env[6] = "f"
  user_env[8] = user_dbname
  user_env[19] = THIS-PROCEDURE:FILE-NAME
  codepage    = "".  
  
   
IF NUM-ENTRIES(df-file-name) = 3 THEN
    ASSIGN user_env[2] = ENTRY(1,df-file-name)
           user_env[15] = (IF ENTRY(2,df-file-name) <> "" THEN ENTRY(2,df-file-name)
                          ELSE "")
           old-session = SESSION:SCHEMA-CHANGE
           SESSION:SCHEMA-CHANGE = ENTRY(3,df-file-name) .
ELSE 
   
IF NUM-ENTRIES(df-file-name) = 2 THEN
    ASSIGN user_env[2] = ENTRY(1,df-file-name)
           user_env[15] = ENTRY(2,df-file-name).
ELSE
   ASSIGN user_env[2] = df-file-name.

RUN read-cp.  /* get codepage out of the .df file trailer (tomn 8/28/95) */ 

ASSIGN user_env[10] = CODEPAGE
       save_ab = SESSION:APPL-ALERT-BOXES
       SESSION:APPL-ALERT-BOXES = NO.

REPEAT:
 IF _msg(counter) > 0 THEN
      ASSIGN counter = counter + 1.
 ELSE
     LEAVE.
END.

DO TRANSACTION ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
  user_path = "*".
  IF user_path = "*" THEN RUN ladda\t_lodsddl.p (df-file-name).  
  IF user_path = "*R" then UNDO, LEAVE.
END.

/*====================== INTERNAL PROCEDURES =========================*/

PROCEDURE read-cp.
  
  /* Read trailer of file and find codepage */
  /* (partially stolen from lodtrail.i)     */
  
  INPUT FROM VALUE(user_env[2]) NO-ECHO NO-MAP.
  SEEK INPUT TO END.
  i = SEEK(INPUT) - 12.
  SEEK INPUT TO i. /* position to possible beginning of last line */

  READKEY PAUSE 0.
  IF LASTKEY = 13 THEN /* deal with CRLF on Windows */
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
    IF lvar[i] BEGINS "cpstream=" OR lvar[i] BEGINS "codepage=" THEN codepage = TRIM(SUBSTRING(lvar[i],10,-1,"character":U)).
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
  DEFINE VARIABLE p AS INT64   INITIAL 256. /* really 204, added extra just in case */
  DEFINE VARIABLE l AS INT64.               /* LAST char position */
  
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
           IF RETURN-VALUE EQ "False Alarm" THEN NEXT.
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
  DEFINE INPUT PARAMETER i as INT64  . /* "SEEK TO" location */

  DEFINE VARIABLE iStartAt   AS INT64       NO-UNDO.
  DEFINE VARIABLE iLinesRead AS INTEGER     NO-UNDO INITIAL 1.
    
  iStartAt = i64.

  SEEK INPUT TO i.
  REPEAT:
    IMPORT lvar[lvar# + 1].
    
    /* The entire line must be equal to "PSC" which is a reserved 
       keyword, but if it's part of a string we simply return to 
       where we left off and continue searching for the trailer.
       20050727-041 */
    IF iLinesRead = 1 AND 
       TRIM(lvar[lvar# + 1]) NE "PSC" THEN DO:
      SEEK INPUT TO iStartAt + LENGTH(lvar[lvar# + 1]) + 1.
      RETURN "False Alarm".
    END.

    ASSIGN lvar#      = lvar# + 1
           iLinesRead = iLinesRead + 1.

  END.
  RETURN "".
END PROCEDURE. 

/*========================== END OF load_df.p ==========================*/


