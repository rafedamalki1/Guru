/* rcvdtaq.p
   Entry procedure for API to the AS/400 to work with Data Queues for receiving
   an entry from the specified queue
   
   Created May 13, 1997
   Donna L. McMann
   
   Modified:  06/03/98 D. McMann Changed how alias is set to hand multiple
                       connected schema holders. 98-05-08-002
              06/09/98 D. McMann Added creation and deletion of dictdb 
                               98-06-08-023
   
*/

DEFINE INPUT        PARAMETER  db-name   AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER  que-name  AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER  lib-name  AS CHARACTER NO-UNDO.    
DEFINE INPUT        PARAMETER  ky-data   AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER  ky-length AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER  ky-order  AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER  wt-time   AS DECIMAL   NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER send-data AS CHARACTER NO-UNDO.

DEFINE OUTPUT       PARAMETER ent-data  AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER stat      AS INTEGER   NO-UNDO.

DEFINE VARIABLE apirtn      AS INTEGER                               NO-UNDO.
DEFINE VARIABLE rec-operand AS CHARACTER INITIAL "EQ,GE,GT,LT,LE,NE" NO-UNDO.
DEFINE VARIABLE i           AS INTEGER                               NO-UNDO.
DEFINE VARIABLE old-dictdb  AS CHARACTER                             NO-UNDO.

/* Verify database name and create alias as4dict, else return with error code -1 */

&IF "{&OPSYS}" <> "OS400" &THEN
    _dbloop:
    DO i = 1 TO NUM-DBS:
      IF (PDBNAME(i) = db-name OR LDBNAME(i) = db-name) AND CONNECTED(db-name) THEN DO:
         CREATE ALIAS as4dict FOR DATABASE VALUE(db-name).
         old-dictdb = LDBNAME("DICTDB").
         CREATE ALIAS DICTDB FOR DATABASE VALUE(SDBNAME(i)). 
         LEAVE _dbloop.
      END.   
    END.
    IF i > NUM-DBS THEN DO:
        ASSIGN stat = -1.
        RETURN.
    END.
  
&ENDIF.

/* Make sure the proper operand has been passed to procedure.  If incorrect return
   error code -3
*/   
IF ky-order = "=" THEN ASSIGN ky-order = "EQ".
ELSE IF ky-order = ">"  THEN ASSIGN ky-order = "GT".
ELSE IF ky-order = "<"  THEN ASSIGN ky-order = "LT".
ELSE IF ky-order = "<=" THEN ASSIGN ky-order = "LE".
ELSE IF ky-order = ">=" THEN ASSIGN ky-order = "GE".
ELSE IF ky-order = "<>" THEN ASSIGN ky-order = "NE".


IF LENGTH(ky-order) > 0 THEN DO:
    IF LOOKUP(ky-order, rec-operand) = 0 THEN DO:
        ASSIGN stat = -3.
        DELETE alias as4dict.
        RETURN.
    END.
END.    
IF ky-length > LENGTH(ky-data) THEN 
    ASSIGN ky-data = ky-data + FILL(" ", (ky-length - LENGTH(ky-data))).
     
 
RUN as4dict/qdtarcv.p (INPUT que-name, INPUT lib-name, INPUT ky-data, 
                       INPUT ky-length, INPUT ky-order, INPUT wt-time, 
                       INPUT-OUTPUT send-data, OUTPUT ent-data, OUTPUT apirtn).

ASSIGN stat = apirtn.

DELETE alias as4dict.
DELETE alias DICTDB.
CREATE ALIAS DICTDB FOR DATABASE VALUE(old-dictdb).

RETURN.

