/* snddtaq.p
   Entry procedure for API to the AS/400 to work with Data Queues for sending
   an entry to the specified queue
   
   Created May 13, 1997
   Donna L. McMann
   
   Modified:  06/03/98 D. McMann Changed how alias is set to hand multiple
                       connected schema holders. 98-05-08-002
              06/09/98 D. McMann Added creation and deletion of dictdb 
                               98-06-08-023
      
*/

DEFINE INPUT PARAMETER  db-name  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  que-name AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  lib-name  AS CHARACTER NO-UNDO.    
DEFINE INPUT PARAMETER  ent-data  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  ky-data   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  ky-length AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER stat      AS INTEGER   NO-UNDO.

DEFINE VARIABLE apirtn            AS INTEGER   NO-UNDO.
DEFINE VARIABLE i                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE old-dictdb        AS CHARACTER NO-UNDO.

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

IF ky-length > LENGTH(ky-data) THEN 
    ASSIGN ky-data = ky-data + FILL(" ", (ky-length - LENGTH(ky-data))).
                       
RUN as4dict/qdtasnd.p (INPUT que-name, INPUT lib-name, INPUT ent-data, 
                       INPUT ky-data, INPUT ky-length, OUTPUT apirtn).

ASSIGN stat = apirtn.

DELETE alias as4dict.
DELETE alias DICTDB.
CREATE ALIAS DICTDB FOR DATABASE VALUE(old-dictdb).

RETURN.

