
   
   /*xoneprojkoll2.p*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE VARIABLE gurubilder AS CHARACTER NO-UNDO.

DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE hjdel AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE tidinahw
   FIELD AONRDELNR                AS CHARACTER       
   FIELD NYAONR          AS CHARACTER      
   FIELD activitysec      AS CHARACTER    
   INDEX AONRDELNR IS PRIMARY AONRDELNR.
   
  DEFINE TEMP-TABLE tidinaha
   FIELD AONRDELNR                AS CHARACTER       
   FIELD NYAONR1          AS CHARACTER   
   FIELD NYAONR2              AS CHARACTER
   FIELD NYARB              AS INTEGER
   FIELD activitysec      AS CHARACTER      
   INDEX AONRDELNR IS PRIMARY AONRDELNR.
   
DEFINE TEMP-TABLE bytaaonrtt NO-UNDO
FIELD AONRDELNRG AS CHARACTER
FIELD AONRG AS CHARACTER
FIELD DELNRG AS INTEGER
FIELD AONRN AS CHARACTER
FIELD DELNRN AS INTEGER
FIELD NYARB              AS INTEGER
FIELD ACTIVITYSEC      AS CHARACTER
INDEX  AONRG AONRG DELNRG.   

DEFINE TEMP-TABLE ejbytaaonrtt LIKE bytaaonrtt. 
   
DEFINE BUFFER aotbuff FOR bytaaonrtt.
DEFINE BUFFER aotbuff2 FOR bytaaonrtt.
DEFINE BUFFER aotbuff3 FOR bytaaonrtt.   

   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   

FIND FIRST FORETAG NO-LOCK NO-ERROR.
{muswait.i} 

EMPTY TEMP-TABLE tidinahw NO-ERROR. 
EMPTY TEMP-TABLE tidinaha NO-ERROR.    
   
   filnamn = "D:\elpool\Gamla AO service ort 10-11.SKV".   
   RUN inlas_UI.
   filnamn = "D:\elpool\Gamla AO service ort 12-13.SKV".   
   RUN inlas_UI.
   filnamn = "D:\elpool\Gamla AO service ort 14-15.SKV".   
   RUN inlas_UI.
   filnamn = "D:\elpool\Gamla AO service ort 16-17.SKV".   
   RUN inlas_UI.
   filnamn = "D:\elpool\Gamla AO service ort 18-19.SKV".   
   RUN inlas_UI.
   filnamn = "D:\elpool\Gamla AO service ort 20-21.SKV".   
   RUN inlas_UI.
   filnamn = "D:\elpool\Gamla AO service ort 22-23.SKV".   
   RUN inlas_UI.
   filnamn = "D:\elpool\Gamla AO service ort 24-26.SKV".   
   RUN inlas_UI.
   
   filnamn = "D:\elpool\fixAO-nummer till IFS projekt GURR 2020 11 03.skv".      
   RUN inlasa_UI.  

   RUN skapaaot_UI.
PROCEDURE inlas_UI: 

   EMPTY TEMP-TABLE tidinahw NO-ERROR.
   
   INPUT FROM VALUE(filnamn) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidinahw.
         ASSIGN.
         IMPORT DELIMITER ";" tidinahw   NO-ERROR.
      END.               
   END.
   FOR EACH tidinahw WHERE tidinahw.AONRDELNR = "":
      DELETE tidinahw.
   END.           
END PROCEDURE.

PROCEDURE inlasa_UI: 

   EMPTY TEMP-TABLE tidinaha NO-ERROR.
   
   INPUT FROM VALUE(filnamn) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidinaha.
         ASSIGN.
         IMPORT DELIMITER ";" tidinaha   NO-ERROR.
      END.               
   END.
   FOR EACH tidinaha WHERE tidinaha.AONRDELNR = "":
      DELETE tidinaha.
   END.           
END PROCEDURE.

PROCEDURE skapaaot_UI:   
   
   EMPTY TEMP-TABLE bytaaonrtt NO-ERROR. 
   EMPTY TEMP-TABLE ejbytaaonrtt NO-ERROR. 
   
   FOR EACH tidinaha NO-LOCK:          
      FIND FIRST AONRTAB  WHERE AONRTAB.AONR = SUBSTRING(tidinaha.AONRDELNR,1,6)
      AND AONRTAB.DELNR = INTEGER(SUBSTRING(tidinaha.AONRDELNR,7,2))  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE AONRTAB THEN DO:
         CREATE ejbytaaonrtt.
         ASSIGN
         ejbytaaonrtt.AONRDELNRG = tidinaha.AONRDELNR
         ejbytaaonrtt.AONRG = SUBSTRING(tidinaha.AONRDELNR,1,6).
         ejbytaaonrtt.DELNRG = INTEGER(SUBSTRING(tidinaha.AONRDELNR,7,2)).
         ejbytaaonrtt.AONRN = tidinaha.NYAONR1 + "." + tidinaha.NYAONR2.
         ejbytaaonrtt.NYARB = tidinaha.NYARB.
         ejbytaaonrtt.activitysec = tidinaha.activitysec.
      END.
      ELSE DO:                                   
         CREATE bytaaonrtt.
         ASSIGN
         bytaaonrtt.AONRDELNRG = tidinaha.AONRDELNR
         bytaaonrtt.AONRG = SUBSTRING(tidinaha.AONRDELNR,1,6).
         bytaaonrtt.DELNRG = INTEGER(SUBSTRING(tidinaha.AONRDELNR,7,2)).
         bytaaonrtt.AONRN = tidinaha.NYAONR1 + "." + tidinaha.NYAONR2.
         bytaaonrtt.DELNRN = 100.
         bytaaonrtt.NYARB = tidinaha.NYARB.
         bytaaonrtt.activitysec = tidinaha.activitysec.
      END.    
   END.
    
   OUTPUT TO D:\elpool\DELNRBYT.txt.   
   FOR EACH bytaaonrtt WHERE NO-LOCK:
      hjdel = 0.
      /*IF bytaaonrtt.AONRN = "30286.030" THEN DO:
         MESSAGE "30286.030"
         VIEW-AS ALERT-BOX.
      END.*/   
      FIND FIRST aotbuff WHERE aotbuff.AONRN = bytaaonrtt.AONRN AND  aotbuff.AONRDELNRG NE bytaaonrtt.AONRDELNRG  NO-LOCK NO-ERROR.
      IF AVAILABLE aotbuff THEN DO:
         FIND FIRST aotbuff2 WHERE aotbuff2.AONRN = bytaaonrtt.AONRN AND aotbuff2.DELNRN = bytaaonrtt.DELNRG AND aotbuff2.AONRG NE bytaaonrtt.AONRG     NO-LOCK NO-ERROR.
         IF NOT AVAILABLE aotbuff2 THEN DO:
            FIND FIRST aotbuff2 WHERE aotbuff2.AONRN = bytaaonrtt.AONRN AND aotbuff2.DELNRN = bytaaonrtt.DELNRG AND RECID(aotbuff2) NE RECID (bytaaonrtt)     NO-LOCK NO-ERROR.
         END.
         IF AVAILABLE aotbuff2 THEN DO:
            REPEAT:
               FIND FIRST aotbuff3 WHERE aotbuff3.AONRN = bytaaonrtt.AONRN AND aotbuff3.DELNRN = hjdel  NO-LOCK NO-ERROR.
               IF AVAILABLE aotbuff3 THEN DO:
                  hjdel = hjdel + 1.
               END.
               ELSE DO:
                  PUT UNFORMATTED bytaaonrtt.AONRG " "  bytaaonrtt.DELNRG " " bytaaonrtt.AONRN  " " bytaaonrtt.DELNRN " " bytaaonrtt.NYARB " "  hjdel " " "HJDEL" SKIP.
                  ASSIGN bytaaonrtt.DELNRN = hjdel.
                  LEAVE.
               END.
               IF hjdel > 99 THEN LEAVE.   
            END.         
         END.
         ELSE DO:                        
            ASSIGN bytaaonrtt.DELNRN = bytaaonrtt.DELNRG.   
            PUT UNFORMATTED bytaaonrtt.AONRG " "  bytaaonrtt.DELNRG " " bytaaonrtt.AONRN  " " bytaaonrtt.DELNRN " " bytaaonrtt.NYARB  " " "DELNRNG" SKIP.
         END.               
      END.
      ELSE DO:
         ASSIGN bytaaonrtt.DELNRN = bytaaonrtt.NYARB
         bytaaonrtt.NYARB = 0.
      END.      
   END.
   OUTPUT CLOSE.
   FOR EACH tidinahw NO-LOCK:                   
      FIND FIRST AONRTAB  WHERE AONRTAB.AONR = SUBSTRING(tidinahw.AONRDELNR,1,6)
      AND AONRTAB.DELNR = INTEGER(SUBSTRING(tidinahw.AONRDELNR,7,2))  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE AONRTAB THEN DO:
         CREATE ejbytaaonrtt.
         ASSIGN
         ejbytaaonrtt.AONRDELNRG = tidinahW.AONRDELNR
         ejbytaaonrtt.AONRG = SUBSTRING(tidinahw.AONRDELNR,1,6).
         ejbytaaonrtt.DELNRG = INTEGER(SUBSTRING(tidinahw.AONRDELNR,7,2)).
         ejbytaaonrtt.AONRN = tidinahw.NYAONR.
         ejbytaaonrtt.DELNRN = 0.
         ejbytaaonrtt.activitysec = tidinahw.activitysec.
      END.
      ELSE DO:                          
         CREATE bytaaonrtt.
         ASSIGN
         bytaaonrtt.AONRDELNRG = tidinahW.AONRDELNR
         bytaaonrtt.AONRG = SUBSTRING(tidinahw.AONRDELNR,1,6).
         bytaaonrtt.DELNRG = INTEGER(SUBSTRING(tidinahw.AONRDELNR,7,2)).
         bytaaonrtt.AONRN = tidinahw.NYAONR.
         bytaaonrtt.DELNRN = 0.
         bytaaonrtt.activitysec = tidinahw.activitysec.
      END.    
   END.
   
   OUTPUT TO D:\elpool\AONREJTRAFF.txt.     
   FOR EACH ejbytaaonrtt  NO-LOCK:      
      PUT UNFORMATTED ejbytaaonrtt.AONRG " "  ejbytaaonrtt.DELNRG " " ejbytaaonrtt.AONRN " " ejbytaaonrtt.DELNRN " "  ejbytaaonrtt.NYARB SKIP.               
   END.
   OUTPUT CLOSE.
   
   
   
   OUTPUT TO D:\elpool\AONRTRAFF.txt.     
   FOR EACH bytaaonrtt  NO-LOCK:      
      PUT UNFORMATTED bytaaonrtt.AONRG " "  bytaaonrtt.DELNRG " " bytaaonrtt.AONRN  " " bytaaonrtt.DELNRN " " bytaaonrtt.NYARB SKIP.              
   END.      
            
   OUTPUT CLOSE.
   OUTPUT TO D:\elpool\AONRdub.txt.
   FOR EACH bytaaonrtt  NO-LOCK:
      FIND FIRST aotbuff WHERE aotbuff.AONRN = bytaaonrtt.AONRN AND aotbuff.DELNRN = bytaaonrtt.DELNRN AND aotbuff.NYARB = bytaaonrtt.NYARB  AND  aotbuff.AONRDELNRG NE bytaaonrtt.AONRDELNRG  NO-LOCK NO-ERROR.
      IF AVAILABLE aotbuff THEN DO:
         PUT UNFORMATTED bytaaonrtt.AONRN " "  bytaaonrtt.DELNRN " " bytaaonrtt.NYARB " " bytaaonrtt.AONRDELNRG  " B" SKIP.
         PUT UNFORMATTED aotbuff.AONRN " "  aotbuff.DELNRN " " aotbuff.NYARB " " aotbuff.AONRDELNRG  " A" SKIP.
      END.
   END.
   OUTPUT CLOSE.               
         
       
END PROCEDURE.   

                

   

