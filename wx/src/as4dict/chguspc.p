/* chgdara.p
   Entry procedure for API to the AS/400 to work with user spaces for changing
   an entry from the specified user space
   
   Created June 30, 1997
   Donna L. McMann
   
   Modified:  06/03/98 D. McMann Changed how alias is set to hand multiple
                                 connected schema holders. 98-05-08-002
              06/09/98 D. McMann Added creation and deletion of dictdb 
                                 98-06-08-023

    
 */

DEFINE INPUT        PARAMETER db-name       AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER uspc-name     AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER lib-name      AS CHARACTER NO-UNDO.    
DEFINE INPUT        PARAMETER start-pos     AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER data-length   AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER data-val      AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER stat          AS INTEGER   NO-UNDO.

DEFINE VARIABLE apirtn      AS INTEGER                               NO-UNDO.
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
IF start-pos < 0 THEN DO:
  ASSIGN stat = -3.
  DELETE alias as4dict.
  DELETE alias DICTDB.
  CREATE ALIAS DICTDB FOR DATABASE VALUE(old-dictdb).

  RETURN.
END.
IF data-length > 2048 THEN DO:
  ASSIGN stat = -4.
  DELETE alias as4dict.
  DELETE alias DICTDB.
  CREATE ALIAS DICTDB FOR DATABASE VALUE(old-dictdb).
  RETURN.
END.

      
RUN as4dict/qusrspc.p (INPUT "C", INPUT uspc-name, INPUT lib-name, INPUT start-pos, 
                        INPUT data-length, INPUT-OUTPUT data-val, OUTPUT apirtn).

ASSIGN stat = apirtn.

DELETE alias as4dict.
DELETE alias DICTDB.
CREATE ALIAS DICTDB FOR DATABASE VALUE(old-dictdb).


RETURN.




