PROCEDURE locktest_UI :
       DEFINE VARIABLE plaidBuffer     AS HANDLE.
       DEFINE VARIABLE qString AS CHARACTER NO-UNDO.
       DEFINE VARIABLE qString2 AS CHARACTER NO-UNDO.
       DEFINE VARIABLE qString3 AS CHARACTER NO-UNDO.
       DEFINE VARIABLE plaidValue AS INTEGER INITIAL 0.
       
       CREATE BUFFER plaidBuffer FOR TABLE "GPLAKTIVITET". 
   qString =  "WHERE PLID = " + "1" + " USE-INDEX PLAID". 
   DO TRANSACTION:   
         plaidBuffer:FIND-LAST(qString,EXCLUSIVE-LOCK).
              IF plaidBuffer:AVAILABLE THEN DO:
                  plaidValue = plaidBuffer:BUFFER-FIELD("PLAID"):BUFFER-VALUE NO-ERROR.
                  MESSAGE "PAUS"
                  VIEW-AS ALERT-BOX. 
              END. 
   END.
END PROCEDURE.