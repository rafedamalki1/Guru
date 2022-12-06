{EKGKAT.I}


FOR EACH EKGP2 WHERE EKGSUBID = 5 AND P2ARBKOD = "138"  :
         CREATE ekgArbLopKodTT.
         ASSIGN
         ekgArbLopKodTT.P2ARBKOD = ekgp2.P2ARBKOD
         ekgArbLopKodTT.BENAMNING = ekgp2.BENAMNING
         ekgArbLopKodTT.SVEPLOPNR = STRING(ekgp2.P2LOPNR).
        
      END.  
CREATE ekgArbLopKodTT.
      ASSIGN
      ekgArbLopKodTT.P2ARBKOD = "138"
      ekgArbLopKodTT.SVEPLOPNR = "05".

CREATE ekgArbLopKodTT.
      ASSIGN
      ekgArbLopKodTT.P2ARBKOD = "138"
      ekgArbLopKodTT.SVEPLOPNR = "19".

CREATE ekgArbLopKodTT.
      ASSIGN
      ekgArbLopKodTT.P2ARBKOD = "138"
      ekgArbLopKodTT.SVEPLOPNR = "20".

DEFINE VARIABLE sveplop AS CHARACTER NO-UNDO.
DEFINE VARIABLE lopnrvar AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE sveptemp NO-UNDO
   FIELD LOPNR AS INTEGER
   FIELD SVEPLOPNR AS CHAR
   FIELD TECKEN AS CHARACTER
   INDEX LOPNR LOPNR.

DEFINE VARIABLE lopi AS INTEGER NO-UNDO.

DEFINE VARIABLE ascivarde AS INTEGER NO-UNDO.
DEFINE VARIABLE extralop AS CHARACTER NO-UNDO.
DEFINE VARIABLE svepvar1 AS CHARACTER NO-UNDO EXTENT 2.
DEFINE VARIABLE svepvar2 AS CHARACTER NO-UNDO EXTENT 2.

EMPTY TEMP-TABLE sveptemp NO-ERROR. 
/*
sveplop = "11-14,18,23,25-26".
*/
sveplop = "11-13".

   REPEAT:
     lopi = lopi + 1.
     IF lopi > LENGTH(sveplop)  THEN LEAVE.
     
     ascivarde =  ASC(SUBSTRING(sveplop,lopi,1)).  
     IF ascivarde >= 48 AND ascivarde <= 57 THEN lopnrvar = lopnrvar + SUBSTRING(sveplop,lopi,1).
     ELSE DO:
         CREATE sveptemp.
         ASSIGN 
         sveptemp.LOPNR =  INTEGER(lopnrvar) 
         sveptemp.SVEPLOPNR =  lopnrvar
         sveptemp.TECKEN = SUBSTRING(sveplop,lopi,1). 
         lopnrvar = "".
      END.      
   END.
   IF lopnrvar NE "" THEN DO:
      CREATE sveptemp.
      ASSIGN 
      sveptemp.LOPNR =  INTEGER(lopnrvar) 
      sveptemp.SVEPLOPNR =  lopnrvar.
   END.   
   

                  /*sveplop = "11-14,18,23,25-26".*/
extralop = "14".
extralop = "28".  /*sveplop = "10-14,18-19,23,25-28"*/.
RUN svep_UI.
extralop = "29".  /*sveplop = "10-14,18-19,23,25-28"*/.
RUN svep_UI.
extralop = "30".  /*sveplop = "10-14,18-19,23,25-28"*/.
RUN svep_UI.
extralop = "31".  /*sveplop = "10-14,18-19,23,25-28"*/.
RUN svep_UI.
extralop = "32".  /*sveplop = "10-14,18-19,23,25-28"*/.
RUN svep_UI.
extralop = "33".  /*sveplop = "10-14,18-19,23,25-28"*/.
RUN svep_UI.
FOR EACH sveptemp:

   DISPL sveptemp.
END.
extralop = "30".  /*sveplop = "10-14,18-19,23,25-28"*/.
RUN svep_UI.

/*
RUN svep_UI.     /*sveplop = "10-14,18,23,25-26"*/.
extralop = "19".  /*sveplop = "10-14,18-19,23,25-26"*/.
RUN svep_UI.

extralop = "28".  /*sveplop = "10-14,18-19,23,25-28"*/.
RUN svep_UI.



extralop = "32". /*sveplop = "10-14,18-19,23,25-28,32"*/.
RUN svep_UI.

extralop = "30". /*sveplop = "10-14,18-19,23,25-28,30,32"*/.
RUN svep_UI.
extralop = "15". /*sveplop = "10-15,18-19,23,25-28,30,32"*/.
RUN svep_UI.
extralop = "26". /*sveplop = "10-15,18-19,23,25-28,30,32"*/.

RUN svep_UI.
extralop = "01".   /*sveplop = "01,10-15,18-19,23,25-28,30,32"*/.
RUN svep_UI.

/*
extralop = "16".   /*sveplop = "01,10-15,18-19,23,25-28,30,32"*/.
RUN svep_UI.
*/

RUN svep_UI.
extralop = "33".   /*sveplop = "01,10-15,18-19,23,25-28,30,32-33"*/.
RUN svep_UI.
*/
sveplop = "".
FOR EACH sveptemp WHERE NO-LOCK:
   IF sveplop = "-" AND sveptemp.TECKEN = "-" THEN DELETE sveptemp.
   IF AVAILABLE sveptemp THEN sveplop = sveptemp.TECKEN.
    
END.

sveplop = "".
FOR EACH sveptemp WHERE NO-LOCK:
   
   sveplop = sveplop + sveptemp.SVEPLOPNR + sveptemp.TECKEN. 
END.
MESSAGE sveplop
VIEW-AS ALERT-BOX.
PROCEDURE svep_UI :
   svepvar1 = "".
   svepvar2 = "".
   FIND FIRST sveptemp WHERE sveptemp.LOPNR = INTEGER(extralop) NO-LOCK NO-ERROR.
   IF AVAILABLE sveptemp THEN RETURN.
   
   FIND LAST sveptemp WHERE sveptemp.LOPNR < INTEGER(extralop) NO-LOCK NO-ERROR.
   IF AVAILABLE sveptemp THEN DO:
      ASSIGN 
      svepvar1[1] = sveptemp.SVEPLOPNR
      svepvar1[2] = sveptemp.TECKEN.
   END.   
   FIND FIRST sveptemp WHERE sveptemp.LOPNR > INTEGER(extralop) NO-LOCK NO-ERROR.
   IF AVAILABLE sveptemp THEN DO:
      ASSIGN 
      svepvar2[1] = sveptemp.SVEPLOPNR
      svepvar2[2] = sveptemp.TECKEN.
   END.   
   
   IF svepvar1[1] = "" THEN DO:
      /*ska bli första posten*/
      FIND FIRST ekgArbLopKodTT  WHERE P2ARBKOD = "138" AND INTEGER(ekgArbLopKodTT.SVEPLOPNR) > INTEGER(extralop) AND 
      INTEGER(ekgArbLopKodTT.SVEPLOPNR) < INTEGER(svepvar2[1])
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgArbLopKodTT THEN DO: 
         RUN findsvep_UI (INPUT svepvar2[1]).
         ASSIGN 
         sveptemp.LOPNR =  INTEGER(extralop)
         sveptemp.SVEPLOPNR = extralop
         sveptemp.TECKEN = "-".
      END.
      ELSE DO:
         CREATE sveptemp.
         ASSIGN 
         sveptemp.LOPNR =  INTEGER(extralop)
         sveptemp.SVEPLOPNR = extralop 
         sveptemp.TECKEN = ",".
      END.            
   END.
   ELSE IF svepvar1[2] = "" THEN DO:
      /*ska bli sista posten*/
      FIND FIRST ekgArbLopKodTT  WHERE P2ARBKOD = "138" AND INTEGER(ekgArbLopKodTT.SVEPLOPNR) < INTEGER(extralop) AND 
      INTEGER(ekgArbLopKodTT.SVEPLOPNR) > INTEGER(svepvar1[1])
      NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE ekgArbLopKodTT THEN DO: 
         RUN findsvep_UI (INPUT svepvar1[1]).
         sveptemp.TECKEN = "-".
         CREATE sveptemp.
         ASSIGN 
         sveptemp.LOPNR =  INTEGER(extralop)
         sveptemp.SVEPLOPNR = extralop
         sveptemp.TECKEN = "".
      END.
      ELSE DO:
         RUN findsvep_UI (INPUT svepvar1[1]).
         sveptemp.TECKEN = ",".
         CREATE sveptemp.
         ASSIGN 
         sveptemp.LOPNR =  INTEGER(extralop)
         sveptemp.SVEPLOPNR =  extralop
         sveptemp.TECKEN = "".
      END.     
   END. 
   ELSE DO:
      DEBUGGER:SET-BREAK().
      /*ska bli någon mitt i*/
      FIND FIRST ekgArbLopKodTT  WHERE P2ARBKOD = "138" AND INTEGER(ekgArbLopKodTT.SVEPLOPNR) < INTEGER(extralop) AND 
      INTEGER(ekgArbLopKodTT.SVEPLOPNR) > INTEGER(svepvar1[1])
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgArbLopKodTT THEN DO: 
         RUN findsvep_UI (INPUT svepvar1[1]).
         ASSIGN 
         sveptemp.TECKEN = "-".
         FIND FIRST ekgArbLopKodTT  WHERE P2ARBKOD = "138" AND INTEGER(ekgArbLopKodTT.SVEPLOPNR) > INTEGER(extralop) AND 
         INTEGER(ekgArbLopKodTT.SVEPLOPNR) < INTEGER(svepvar2[1])
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgArbLopKodTT THEN.
         ELSE DO:
            /*
            FIND PREV sveptemp NO-LOCK NO-ERROR.
            IF sveptemp.TECKEN = "-" THEN DO:
               RUN findsvep_UI (INPUT svepvar1[1]).
               /* + 15 sveplop = "10-14,18-19,23,25-28,30,32"*/.
               ASSIGN 
               sveptemp.LOPNR =  INTEGER(extralop)
               sveptemp.SVEPLOPNR =  extralop
               sveptemp.TECKEN = ",".
            END.   
            ELSE DO:
               /*+ 19 sveplop = "10-14,18,23,25-26"*/.
               CREATE sveptemp.
               ASSIGN 
               sveptemp.LOPNR =  INTEGER(extralop)
               sveptemp.SVEPLOPNR =  extralop
               sveptemp.TECKEN = ",".
            END.
            */   
            CREATE sveptemp.
               ASSIGN 
               sveptemp.LOPNR =  INTEGER(extralop)
               sveptemp.SVEPLOPNR =  extralop
               sveptemp.TECKEN = ",".
         END.
               
      END.
      ELSE DO:
         RUN findsvep_UI (INPUT svepvar1[1]).
         sveptemp.TECKEN = ",".
         CREATE sveptemp.
         ASSIGN 
         sveptemp.LOPNR =  INTEGER(extralop)
         sveptemp.SVEPLOPNR =  extralop
         sveptemp.TECKEN = ",".
         
         FIND FIRST ekgArbLopKodTT  WHERE P2ARBKOD = "138" AND INTEGER(ekgArbLopKodTT.SVEPLOPNR) > INTEGER(extralop) AND 
         INTEGER(ekgArbLopKodTT.SVEPLOPNR) < INTEGER(svepvar2[1])
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgArbLopKodTT THEN  sveptemp.TECKEN = "-".
         ELSE. 
      END.   
      
       
   END.            
END PROCEDURE.


PROCEDURE findsvep_UI :
   DEFINE INPUT  PARAMETER l1 AS CHARACTER NO-UNDO.
   
   FIND FIRST sveptemp WHERE sveptemp.LOPNR =  INTEGER(l1) NO-LOCK NO-ERROR.
END PROCEDURE.




FOR EACH sveptemp:

   DISPL sveptemp.
END.
