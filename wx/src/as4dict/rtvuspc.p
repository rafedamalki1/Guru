/* rtvuspc.p
   Entry procedure for API to the AS/400 to work with user spaces for receiving
   an entry from the specified space
   
   Created June 30, 1997
   Donna L. McMann
   
*/

DEFINE INPUT        PARAMETER db-name     AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER uspc-name   AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER lib-name    AS CHARACTER NO-UNDO.    
DEFINE INPUT        PARAMETER start-pos   AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER data-length AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER data-val    AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER stat        AS INTEGER   NO-UNDO.

DEFINE VARIABLE apirtn      AS INTEGER                               NO-UNDO.

/* Verify database name and create alias as4dict, else return with error code -1 */

&IF "{&OPSYS}" <> "OS400" &THEN
    FIND _Db WHERE _Db._Db-name = db-name NO-LOCK NO-ERROR.
    IF AVAILABLE _Db THEN DO:
      IF CONNECTED(_Db._Db-name) THEN
         CREATE ALIAS as4dict FOR DATABASE VALUE(_Db-name).
    END.
    ELSE DO:
      FIND _Db WHERE _Db._Db-ADDR = db-name NO-LOCK NO-ERROR.   
      IF AVAILABLE _Db THEN DO:
        IF CONNECTED(_Db._Db-name) THEN
         CREATE ALIAS as4dict FOR DATABASE VALUE(_Db-name).
      END.
      ELSE DO:
        ASSIGN stat = -1.
        RETURN.
      END.
    END.

    FIND _File WHERE _File._Db-recid = RECID(_Db)
               AND _File._File-name = "qusrspc"
               NO-LOCK NO-ERROR.

    IF NOT AVAILABLE _File THEN DO:
      ASSIGN stat = -2.
      DELETE alias as4dict.
      RETURN.
    END.    
&ENDIF.
IF start-pos < 0 THEN DO:
  ASSIGN stat = -3.
  DELETE alias as4dict.
  RETURN.
END.
IF data-length > 2048 THEN DO:
  ASSIGN stat = -4.
  DELETE alias as4dict.
  RETURN.
END.
           
RUN as4dict/qusrspc.p (INPUT "R", INPUT uspc-name, INPUT lib-name, INPUT start-pos, 
                       INPUT data-length, INPUT-OUTPUT data-val, OUTPUT apirtn).

ASSIGN stat = apirtn.

DELETE alias as4dict.

RETURN.


