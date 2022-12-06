/*xtill.p*/
  /*VAR finns progress*/
 /*VAR finns är chs-flierna*/ 
 /*VAR finns applikationen*/                             
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.

DEFINE TEMP-TABLE valdbtemp 
   FIELD ORDNING AS INTEGER
   FIELD FORETAG AS CHARACTER LABEL "Företag"
   FIELD VALDB AS CHARACTER   LABEL "Databas"
   FIELD DBNAMN AS CHARACTER 
   FIELD DBCON AS CHARACTER 
   FIELD DBCACHE AS CHARACTER 
   INDEX ORDNING AS PRIMARY FORETAG ORDNING.
DEFINE VARIABLE i AS INTEGER.
DEFINE TEMP-TABLE provag
   FIELD VAG AS CHARACTER.
/*VAR FINNS APPLIKATIONEN ?*/
REPEAT i=1 TO NUM-ENTRIES(PROPATH):
    CREATE provag.
    provag.VAG = STRING(ENTRY(i,PROPATH),"x(78)").
END.
ASSIGN
dlcvar = ""
wtidvar = "".
FOR EACH provag:
    IF dlcvar = "" THEN DO:
       IF INDEX(provag.VAG,"DLC") NE 0 THEN DO:
          dlcvar = SUBSTRING(provag.VAG,1,INDEX(provag.VAG,"DLC")) + "LC\BIN\".
       END.
    END.   
    IF wtidvar = "" THEN DO:
       IF INDEX(provag.VAG,"GURU") NE 0 THEN DO: 
          IF INDEX(provag.VAG,"NTSERVER2") NE 0 THEN DO:
             IF INDEX(provag.VAG,"KOMP") NE 0 THEN DO:
                wtidvar = SUBSTRING(provag.VAG,1,INDEX(provag.VAG,"KOMP")) + "OMP\".
             END.              
          END.  
          IF wtidvar = "" THEN DO:    
             IF INDEX(provag.VAG,"WTID") NE 0 THEN DO:
                wtidvar = SUBSTRING(provag.VAG,1,INDEX(provag.VAG,"WTID")) + "TID\".
             END.
          END.
       END.
    END. 
END.
IF INDEX(wtidvar,"WTID") NE 0 THEN DO:
   guruvar = SUBSTRING(wtidvar,1,INDEX(wtidvar,"WTID") - 1).
END.
ELSE DO:
   IF INDEX(wtidvar,"KOMP") NE 0 THEN DO:
      guruvar = SUBSTRING(wtidvar,1,INDEX(wtidvar,"KOMP") - 1).
   END.
END.   

CREATE valdbtemp.
ASSIGN
valdbtemp.FORETAG = "SUND"
valdbtemp.DBNAMN = "SUNDn9"
valdbtemp.DBCON = "-db sundn9 -S 2516 -H 194.132.143.8 -N TCP"
valdbtemp.DBCACHE = "-cache " + guruvar + "sundn9.CSH"
valdbtemp.ORDNING = 2      
valdbtemp.VALDB = "Endast för förberedelser för tidsk.".
kommando = valdbtemp.DBCON + " " + valdbtemp.DBCACHE. 
CONNECT VALUE(kommando) NO-ERROR.                
i = 0.
DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
   MESSAGE ERROR-STATUS:GET-NUMBER(i)
   ERROR-STATUS:GET-MESSAGE(i)
   VIEW-AS ALERT-BOX.
END.   
