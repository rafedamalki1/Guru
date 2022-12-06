/*  qdtasnd.p
    Donna L. McMann
    May 13, 1997
    
    Procedure to send an entry to a data queue from the client.
 
   Modified: 06/09/98 D. McMann Added find for dictdb._file 98-06-08-023
       
*/

    
DEFINE INPUT PARAMETER que-name  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER lib-name  AS CHARACTER NO-UNDO.    
DEFINE INPUT PARAMETER ent-data  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ky-data   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ky-length AS INTEGER   NO-UNDO.  

DEFINE OUTPUT PARAMETER stat     AS INTEGER   NO-UNDO.


&IF "{&OPSYS}" = "OS400" &THEN
    DEFINE NEW SHARED VARIABLE queue         AS CHARACTER          NO-UNDO.
    DEFINE NEW SHARED VARIABLE lib           AS CHARACTER          NO-UNDO.
    DEFINE NEW SHARED VARIABLE wait-time     AS INTEGER            NO-UNDO.
    DEFINE NEW SHARED VARIABLE key-length    AS INTEGER            NO-UNDO.
    DEFINE NEW SHARED VARIABLE key-buffer    AS CHARACTER          NO-UNDO.
    DEFINE NEW SHARED VARIABLE entry-buffer  AS CHARACTER          NO-UNDO.
    DEFINE NEW SHARED VARIABLE entry-length  AS INTEGER            NO-UNDO.
    DEFINE NEW SHARED VARIABLE operation     AS CHARACTER          NO-UNDO.
    DEFINE NEW SHARED VARIABLE key-order     AS CHARACTER          NO-UNDO.
    DEFINE NEW SHARED VARIABLE sender-length AS INTEGER            NO-UNDO.
    DEFINE NEW SHARED VARIABLE sender-buffer AS CHARACTER          NO-UNDO.
    DEFINE NEW SHARED VARIABLE sts           AS INTEGER            NO-UNDO.
    DEFINE NEW SHARED VARIABLE msgarr        AS CHARACTER EXTENT 1 NO-UNDO.
    DEFINE NEW SHARED VARIABLE msgID         AS CHARACTER          NO-UNDO.

    ASSIGN operation     = "S"
           queue         = CAPS(que-name)
           lib           = CAPS(lib-name)
           entry-length  = LENGTH(ent-data)
           entry-buffer  = ent-data
           key-length    = ky-length
           key-buffer    = ky-data
           key-order     = ""
           wait-time     = 0
           sender-length = 64.

    OS400 EPI STATUS(sts) MESSAGES(msgarr)
          CALL PGM(usrdtaq)
           PARM(operation      AS CHARACTER(1) USE INPUT)
           PARM(queue          AS CHARACTER(10) USE INPUT)
           PARM(lib            AS CHARACTER(10) USE INPUT)
           PARM(entry-length   AS INTEGER(4) USE INPUT-OUTPUT)
           PARM(entry-buffer   AS CHARACTER(2048) USE INPUT-OUTPUT)
           PARM(key-length     AS INTEGER(4) USE INPUT)
           PARM(key-buffer     AS CHARACTER(256) USE INPUT)
           PARM(key-order      AS CHARACTER(2) USE INPUT)
           PARM(wait-time      AS INTEGER(4) USE INPUT)
           PARM(sender-length  AS INTEGER(4) USE INPUT-OUTPUT)
           PARM(sender-buffer  AS CHARACTER(64) USE OUTPUT)
           PARM(msgID          AS CHARACTER(7) USE OUTPUT).

    IF OS-ERROR > 0 THEN
      ASSIGN stat = -1.
    ELSE
      ASSIGN stat = 0.

    RETURN.

&ELSE

    DEFINE VARIABLE dta-length AS RECID NO-UNDO.

    FIND FIRST DICTDB._File WHERE DICTDB._File._File-name = "qdtaq-entry"
               NO-LOCK NO-ERROR.

    IF NOT AVAILABLE DICTDB._File THEN DO:
      ASSIGN stat = -2.
      RETURN.
    END.  

    CREATE as4dict.qdtaq-entry.
    ASSIGN as4dict.qdtaq-entry.queue-name = CAPS(que-name)
           as4dict.qdtaq-entry.library-name = CAPS(lib-name)
           as4dict.qdtaq-entry.entry-length = LENGTH(ent-data)
           as4dict.qdtaq-entry.entry-data   = ent-data
           as4dict.qdtaq-entry.key-length   = ky-length
           as4dict.qdtaq-entry.Key-data     = ky-data.
           
               
    ASSIGN dta-length = RECID(as4dict.qdtaq-entry).
    IF dta-length <> 0 THEN
      ASSIGN stat = 0.
    ELSE
      ASSIGN stat = -4.
             

    RETURN.
&ENDIF

