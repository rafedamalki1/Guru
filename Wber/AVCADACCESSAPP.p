/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: AVCADACCESSAPP.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2007.12.03 14:04 ELPAO   
     Modified: 2007.12.27 10:48 ELPAO    
     Modified: 2008.03.13 11:54 ELPAO    
     Modified: 2008.10.08 15:58 ELPAO    
     Modified: 2009.03.23 10:16 ELPAO    
     Modified: 2009.09.11 08:39 ELPAO    
     Modified: 2009.11.20 14:09 ELPAO    
     Modified: 
*/
&Scoped-define NEW   
&Scoped-define SHARED 

{KONVALTEMP.I}
{BERPUNKTTEMP.I}
DEFINE INPUT PARAMETER valaonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valomrade AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valdelnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER globanv AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR kon_val.
DEFINE INPUT PARAMETER TABLE FOR berpunkttemp.

DEFINE VARIABLE f2kV AS CHARACTER  NO-UNDO.
DEFINE VARIABLE f2linaUS AS CHARACTER  NO-UNDO.
DEFINE VARIABLE f2lina AS CHARACTER  NO-UNDO.
DEFINE VARIABLE f2area AS CHARACTER  NO-UNDO.
DEFINE VARIABLE f4djup AS DECIMAL   NO-UNDO.
DEFINE VARIABLE f4area AS DECIMAL  NO-UNDO.
DEFINE BUFFER kbuff FOR kon_val.

FOR EACH kon_val:
   kon_val.GRUPP = 1.
   RUN sokF1_UI.  
END.



RUN sokF2_UI.
RUN sokF4_UI.

FOR EACH kon_val WHERE kon_val.KSKAP = FALSE NO-LOCK:
   IF kon_val.F2 NE "" THEN DO:  
      CREATE kbuff.
      BUFFER-COPY kon_val TO kbuff.
      ASSIGN
      kbuff.F4 = ""
      kbuff.ANMARK = ""
      kbuff.KSKAP = TRUE
      kbuff.SKAPNUM = 1      
      kon_val.F2      = "".
   END.
   IF kon_val.F4 NE "" THEN DO:  
      CREATE kbuff.
      BUFFER-COPY kon_val TO kbuff.
      ASSIGN
      kbuff.F2 = ""
      kbuff.ANMARK = ""
      kbuff.KSKAP = TRUE
      kbuff.SKAPNUM = 2      
      kon_val.F4      = "".
   END.
END.
     
FOR EACH kon_val:
   DO TRANSACTION:
      CREATE BERVAL.               
      ASSIGN            
      BERVAL.KSKAP     = kon_val.KSKAP
      BERVAL.ANMARK     = kon_val.ANMARK
      BERVAL.AONR       = valaonr
      BERVAL.OMRADE     = valomrade
      BERVAL.KONSKOD    = kon_val.GRUPP
      BERVAL.KTYPKOD    = kon_val.F1
      BERVAL.F2         = kon_val.F2
      BERVAL.F3         = kon_val.F3
      BERVAL.F4         = kon_val.F4
      BERVAL.F5         = kon_val.F5
      BERVAL.F6         = kon_val.F6
      
      BERVAL.ANVANDARE  = globanv
      BERVAL.NUM        = kon_val.NUM
      BERVAL.SKAPNUM    = kon_val.SKAPNUM.  
      IF kon_val.KSKAP = FALSE THEN DO:
         CREATE BERORD.
         ASSIGN
         BERORD.OMRADE = valomrade
         BERORD.AONR = valaonr
         BERORD.NUM = kon_val.NUM
         BERORD.ORD = kon_val.ORD.
         CREATE BERID.
         ASSIGN
         BERID.FRI2 = INTEGER(kon_val.ID2)
         BERID.AONR = valaonr     
         BERID.DELNR = valdelnr   
         BERID.OMRADE = valomrade
         BERID.NUM    = kon_val.NUM.            
         IF INTEGER(kon_val.ID2) > 0 THEN BERVAL.ID = TRUE.
      END.   
   END.
   RUN val_UI.
   
END.
FOR EACH berpunkttemp WHERE NO-LOCK:
   DO TRANSACTION:
      CREATE BERPUNKT.
      BUFFER-COPY berpunkttemp TO BERPUNKT.
   END.   
END.
PROCEDURE sokF2_UI :
   FOR EACH kon_val:
      IF INDEX(kon_val.F2,"$") > 0 THEN DO:
         ASSIGN
         f2lina = SUBSTRING(kon_val.F2,1,INDEX(kon_val.F2,"$") - 1)
         f2area = SUBSTRING(kon_val.F2,INDEX(kon_val.F2,"$") + 1).
         
         IF INDEX(f2lina," ") > 0 THEN DO:
            f2linaUS = SUBSTRING(f2lina,1,INDEX(f2lina," ") - 1).  
            f2kV = SUBSTRING(f2lina,INDEX(f2lina," ") + 1). 
         END.
         FIND FIRST BBENAMNING WHERE  BBENAMNING.KONSKOD = kon_val.GRUPP   NO-LOCK NO-ERROR.
         IF AVAILABLE BBENAMNING THEN DO:
            RUN sokkon_UI (INPUT 1).
            IF NOT AVAILABLE KONSTVAL THEN DO:
               ASSIGN
               f2lina = SUBSTRING(kon_val.F2,1,INDEX(kon_val.F2,"$") - 1)
               f2area = SUBSTRING(kon_val.F2,INDEX(kon_val.F2,"$") + 1).
               f2area = STRING(INTEGER(f2area) / 3).
               RUN sokkon_UI (INPUT 1).
            END.
            IF NOT AVAILABLE KONSTVAL THEN DO:
               ASSIGN
               f2lina = SUBSTRING(kon_val.F2,1,INDEX(kon_val.F2,"$") - 1)
               f2area = SUBSTRING(kon_val.F2,INDEX(kon_val.F2,"$") + 1).
               f2area = STRING(INTEGER(f2area) / 4).
               RUN sokkon_UI (INPUT 1).
            END.
            IF NOT AVAILABLE KONSTVAL THEN DO:
               
               ASSIGN
               f2lina = SUBSTRING(kon_val.F2,1,INDEX(kon_val.F2,"$") - 1)
               f2area = SUBSTRING(kon_val.F2,INDEX(kon_val.F2,"$") + 1).
               IF f2area = "220" THEN f2area = "70".
               RUN sokkon_UI (INPUT 1).
            END.   
            IF AVAILABLE KONSTVAL THEN DO:
               kon_val.F2 = KONSTVAL.KVALKOD.
            END.
            ELSE DO:
               IF AVAILABLE KONSTVAL THEN DO:
                  kon_val.F2 = KONSTVAL.KVALKOD.
               END.
               ELSE kon_val.F2 = kon_val.F2 + " Ingen lina". 
            END.
            
         END.
         ELSE kon_val.F2 = kon_val.F2 + " Ingen grupp". 
      END.               
      ELSE kon_val.F2 = kon_val.F2 + " Ingen area". 
      kon_val.F2 = REPLACE(kon_val.F2,"$"," ").
   END.     
END PROCEDURE.

PROCEDURE sokF4_UI :
   DEFINE VARIABLE forankfalt AS INTEGER NO-UNDO.
   FOR EACH kon_val:
      IF INDEX(kon_val.F4,"$") > 0 THEN DO:
         
         ASSIGN
         f4djup = DECIMAL(SUBSTRING(kon_val.F4,1,INDEX(kon_val.F4,"$") - 1))
         f4area = DECIMAL(SUBSTRING(kon_val.F4,INDEX(kon_val.F4,"$") + 1)).
         FIND FIRST BBENAMNING WHERE  BBENAMNING.KONSKOD = kon_val.GRUPP   NO-LOCK NO-ERROR.
         IF AVAILABLE BBENAMNING THEN DO:
            RUN sokforank_UI (OUTPUT forankfalt).    
            IF AVAILABLE KONSTVAL THEN DO:
               kon_val.F4 = "".
               IF forankfalt = 2 THEN kon_val.F2 = KONSTVAL.KVALKOD.
               ELSE IF forankfalt = 3 THEN kon_val.F3 = KONSTVAL.KVALKOD.
               ELSE IF forankfalt = 4 THEN kon_val.F4 = KONSTVAL.KVALKOD.
               ELSE IF forankfalt = 5 THEN kon_val.F5 = KONSTVAL.KVALKOD.
               ELSE IF forankfalt = 6 THEN kon_val.F6 = KONSTVAL.KVALKOD.
            END.
            ELSE DO:
               kon_val.F4 = "Ingen förankring". 
            END.
            
         END.
         ELSE kon_val.F4 = "Ingen grupp". 
      END.               
      ELSE kon_val.F4 = "Ingen förankring". 
      
   END.     
END PROCEDURE.

PROCEDURE sokforank_UI :
   DEFINE OUTPUT PARAMETER forankfalt AS INTEGER NO-UNDO.
   DEFINE VARIABLE foranktxt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sokfor AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sokforank AS CHARACTER NO-UNDO.
   IF f4djup >= 1.5 THEN sokforank = "SJ1".
   ELSE sokforank = "SB1".
   IF f4area <= 25 THEN sokfor = sokforank + " " + "25".
   ELSE IF f4area <= 52 THEN sokfor = sokforank + " " + "52".
   ELSE IF f4area <= 68 THEN sokfor = sokforank + " " + "68".
   ELSE DO:
     sokfor = sokforank + " " + "65".
   END.   
   IF BBENAMNING.B2 MATCHES "*FÖRANKRING*" THEN DO:
      foranktxt = BBENAMNING.B2.
      forankfalt = 2.
   END.   
   ELSE IF BBENAMNING.B3 MATCHES "*FÖRANKRING*" THEN DO:
      foranktxt = BBENAMNING.B3.
      forankfalt = 3.
   END.
   ELSE IF BBENAMNING.B4 MATCHES "*FÖRANKRING*" THEN DO:
      foranktxt = BBENAMNING.B4.
      forankfalt = 4.
   END.
   ELSE IF BBENAMNING.B5 MATCHES "*FÖRANKRING*" THEN DO:
      foranktxt = BBENAMNING.B5.
      forankfalt = 5.
   END.
   ELSE IF BBENAMNING.B6 MATCHES "*FÖRANKRING*" THEN DO:
      foranktxt = BBENAMNING.B6.
      forankfalt = 6.
   END. 
   FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = foranktxt AND
   TRIM(KONSTVAL.KVALKOD) = sokfor NO-LOCK NO-ERROR.
   IF AVAILABLE KONSTVAL THEN RETURN.
   sokfor = REPLACE(sokfor," ","/").
   FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = foranktxt AND
   TRIM(KONSTVAL.KVALKOD) = sokfor NO-LOCK NO-ERROR.
   IF AVAILABLE KONSTVAL THEN RETURN.
   sokfor = REPLACE(sokfor,"/","-").
   FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = foranktxt AND
   TRIM(KONSTVAL.KVALKOD) = sokfor NO-LOCK NO-ERROR.
   IF AVAILABLE KONSTVAL THEN RETURN.
   sokfor = REPLACE(sokfor,"-","_").
   FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = foranktxt AND
   TRIM(KONSTVAL.KVALKOD) = sokfor NO-LOCK NO-ERROR.
   IF AVAILABLE KONSTVAL THEN RETURN.
   sokfor = REPLACE(sokfor,"_","").
   FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = foranktxt AND
   TRIM(KONSTVAL.KVALKOD) = sokfor NO-LOCK NO-ERROR.
   IF AVAILABLE KONSTVAL THEN RETURN.
   sokfor = sokforank.
   FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = foranktxt AND
   TRIM(KONSTVAL.KVALKOD) = sokfor NO-LOCK NO-ERROR.
   IF AVAILABLE KONSTVAL THEN RETURN.
   
END PROCEDURE.

PROCEDURE sokkon_UI :
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
   KONSTVAL.BENAMNING = f2lina + " " + f2area NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.BENAMNING = f2lina + f2area NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.BENAMNING = f2lina + " 3x" + f2area NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.BENAMNING = f2lina + " 3*" + f2area NO-LOCK NO-ERROR.
   END.
   /**/
   IF NOT AVAILABLE KONSTVAL THEN DO:
      f2lina = TRIM(REPLACE(f2lina,"12 KV","")).
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.BENAMNING = f2lina + " 3x" + f2area NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.BENAMNING = f2lina + " 3*" + f2area NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      f2lina = TRIM(REPLACE(f2lina,"12KV","")).
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.BENAMNING = f2lina + " 3x" + f2area NO-LOCK NO-ERROR.      
   END.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.BENAMNING = f2lina + " 3*" + f2area NO-LOCK NO-ERROR.     
   END.
   /**/
   IF NOT AVAILABLE KONSTVAL THEN DO:
      f2lina = TRIM(REPLACE(f2lina,"24 KV","")).
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.BENAMNING = f2lina + " 3x" + f2area NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.BENAMNING = f2lina + " 3*" + f2area NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      f2lina = TRIM(REPLACE(f2lina,"24KV","")).
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.BENAMNING = f2lina + " 3x" + f2area NO-LOCK NO-ERROR.      
   END.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.BENAMNING = f2lina + " 3*" + f2area NO-LOCK NO-ERROR.     
   END.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.BENAMNING = f2linaUS + " " + f2area NO-LOCK NO-ERROR.   
   END.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.BENAMNING = f2linaUS + f2area NO-LOCK NO-ERROR.   
   END.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.KVALKOD = f2linaUS + " " + f2area NO-LOCK NO-ERROR.   
   END.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
      KONSTVAL.KVALKOD = f2linaUS + f2area NO-LOCK NO-ERROR.   
   END.
   IF NOT AVAILABLE KONSTVAL THEN DO:
      IF f2linaUS = "AXCLH" THEN DO: 
         f2linaUS = "Axligh".  
         FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
         TRIM(KONSTVAL.KVALKOD) = f2linaUS + " " + f2area NO-LOCK NO-ERROR.  
      END.
      IF NOT AVAILABLE KONSTVAL THEN DO:
         FIND FIRST KONSTVAL WHERE KONSTVAL.KONSKOD = kon_val.GRUPP AND KONSTVAL.KTYPKOD = kon_val.F1 AND KONSTVAL.BB = BBENAMNING.B2 AND
         TRIM(KONSTVAL.KVALKOD) = f2linaUS + f2area NO-LOCK NO-ERROR.   
      END.
      IF NOT AVAILABLE KONSTVAL THEN  f2linaUS = f2lina.
   END.
   
   /**/
END PROCEDURE.

PROCEDURE sokF1_UI :
   
   DEFINE VARIABLE hjvar AS CHARACTER NO-UNDO.
   FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = kon_val.F1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KONSTRUKTION THEN DO:
      kon_val.F1 = REPLACE(kon_val.F1,"-K1","").
   END.
   ELSE DO:
      kon_val.GRUPP = KONSTRUKTION.KONSKOD. 
      RETURN.
   END.
   FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = kon_val.F1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KONSTRUKTION THEN DO:
      kon_val.F1 = REPLACE(kon_val.F1,"-K2","").
   END.
   ELSE DO:
      kon_val.GRUPP = KONSTRUKTION.KONSKOD. 
      RETURN.
   END.
   FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = kon_val.F1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KONSTRUKTION THEN DO:
       kon_val.F1 = REPLACE(kon_val.F1,"/","").
   END.
   ELSE DO:
      kon_val.GRUPP = KONSTRUKTION.KONSKOD. 
      RETURN.
   END.
   FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = kon_val.F1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KONSTRUKTION THEN DO:
       kon_val.F1 = REPLACE(kon_val.F1," ","").
   END.
   ELSE DO:
      kon_val.GRUPP = KONSTRUKTION.KONSKOD. 
      RETURN.
   END.
  
   FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = kon_val.F1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KONSTRUKTION THEN DO:
      IF kon_val.F1 = "KR" OR kon_val.F1 = "KV" OR kon_val.F1 = "KÄ" OR kon_val.F1 = "KA" OR 
         kon_val.F1 = "KS" THEN DO:
         IF LENGTH(kon_val.F1) > 4 THEN DO:
            kon_val.F1 = SUBSTRING(kon_val.F1,1,4).            
         END.
         FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = kon_val.F1 NO-LOCK NO-ERROR.
         IF NOT AVAILABLE KONSTRUKTION THEN DO:
            kon_val.F1 = SUBSTRING(kon_val.F1,1,2) + " " + SUBSTRING(kon_val.F1,3,1).                        
            FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = kon_val.F1 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE KONSTRUKTION THEN DO: 
               FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = SUBSTRING(kon_val.F1,1,12) NO-LOCK NO-ERROR.
               IF AVAILABLE KONSTRUKTION THEN DO:
                  hjvar = SUBSTRING(kon_val.F1,1,12).
                  kon_val.F1 = "".
                  kon_val.F1 = hjvar.
                  kon_val.GRUPP = KONSTRUKTION.KONSKOD. 
                  RETURN.
               END.
            END.
            ELSE DO:
               kon_val.GRUPP = KONSTRUKTION.KONSKOD. 
               RETURN.
            END.
         END.
         ELSE DO:
            kon_val.GRUPP = KONSTRUKTION.KONSKOD. 
            RETURN.
         END.
      END.
      ELSE DO:
         FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = SUBSTRING(kon_val.F1,1,12) NO-LOCK NO-ERROR.
         IF AVAILABLE KONSTRUKTION THEN DO:
            hjvar = SUBSTRING(kon_val.F1,1,12).
            kon_val.F1 = "".
            kon_val.F1 = hjvar.
            kon_val.GRUPP = KONSTRUKTION.KONSKOD. 
            RETURN.
         END.
         ELSE DO:
            
            IF kon_val.F1 MATCHES "*-BERG" THEN DO:
               kon_val.F1 = REPLACE(kon_val.F1,"-BERG","").
               FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = SUBSTRING(kon_val.F1,1,12) NO-LOCK NO-ERROR.
               IF AVAILABLE KONSTRUKTION THEN DO:
                  kon_val.GRUPP = KONSTRUKTION.KONSKOD. 
                  RETURN.
               END.    
            END.   
         END.    
      END.
   END.
   ELSE DO:
      kon_val.GRUPP = KONSTRUKTION.KONSKOD. 
      RETURN.
   END.
END PROCEDURE.



PROCEDURE val_UI :
      /*HÄMTA MATERIELET SOM LIGGER DIREKT PÅ KONSTRUKTIONEN ENDAST EN GÅNG*/
   DEFINE VARIABLE stolpvar AS CHARACTER NO-UNDO.
   IF kon_val.KSKAP = FALSE THEN DO:
      OPEN QUERY mtrlq FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = kon_val.F1 AND
      MTRLBER.F1 = "" AND MTRLBER.F2 = " " AND MTRLBER.F3 = " " AND
      MTRLBER.F4 = " " AND MTRLBER.F5 = " " USE-INDEX KOD NO-LOCK. 
      GET FIRST mtrlq NO-LOCK.
      DO WHILE AVAILABLE(MTRLBER):
         RUN skapa_UI.
         GET NEXT mtrlq NO-LOCK. 
      END.      
      CLOSE QUERY mtrlq.
   END.   
   IF kon_val.F2 NE "" THEN DO: 
      OPEN QUERY mtrlq FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = kon_val.F1 AND
      MTRLBER.F1 = kon_val.F2 AND MTRLBER.F2 = " " AND MTRLBER.F3 = " " AND
      MTRLBER.F4 = " " AND MTRLBER.F5 = " " USE-INDEX KOD NO-LOCK. 
      GET FIRST mtrlq NO-LOCK.
      DO WHILE AVAILABLE(MTRLBER):
         RUN skapa_UI.
         GET NEXT mtrlq NO-LOCK. 
      END.
      CLOSE QUERY mtrlq.      
   END.   
   
   IF kon_val.F4 NE "" THEN DO: 
      OPEN QUERY mtrlq FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = kon_val.F1 AND
      MTRLBER.F1 = " " AND MTRLBER.F2 = " " AND MTRLBER.F3 = kon_val.F4 AND
      MTRLBER.F4 = " " AND MTRLBER.F5 = " " USE-INDEX KOD NO-LOCK. 
      GET FIRST mtrlq NO-LOCK.
      DO WHILE AVAILABLE(MTRLBER):
         RUN skapa_UI.
         GET NEXT mtrlq NO-LOCK. 
      END.
      CLOSE QUERY mtrlq.      
   END.   
   IF INDEX(kon_val.ANMARK,"Stolp dim:") > 0 THEN DO:
      stolpvar = "".
      IF INDEX(kon_val.ANMARK,"Stag area:") > 0 THEN DO:
         stolpvar =  TRIM(SUBSTRING(kon_val.ANMARK,1,INDEX(kon_val.ANMARK,"Stag area:") - 1)).
      END.
      ELSE stolpvar = kon_val.ANMARK. 
      stolpvar = REPLACE(stolpvar,"Stolp dim:","").
      IF stolpvar NE "" THEN DO:
         RUN StolpFind_UI (INPUT-OUTPUT stolpvar).
         IF NOT AVAILABLE BERSTOLP THEN DO:
            stolpvar = "".
            IF INDEX(kon_val.ANMARK,"Stag area:") > 0 THEN DO:
               stolpvar =  TRIM(SUBSTRING(kon_val.ANMARK,1,INDEX(kon_val.ANMARK,"Stag area:") - 1)).
            END.
            ELSE stolpvar = kon_val.ANMARK. 
            stolpvar = REPLACE(stolpvar,"Stolp dim:","").
            stolpvar = TRIM(SUBSTRING(stolpvar,2,3)) + " " + SUBSTRING(stolpvar,1,1). 
            IF stolpvar NE "" THEN RUN StolpFind_UI (INPUT-OUTPUT stolpvar).
         END.
         DEFINE VARIABLE antalvar AS INTEGER NO-UNDO.
         IF AVAILABLE BERSTOLP THEN DO TRANSACTION:
            antalvar = 1.
            FOR EACH BERSTOLP WHERE BERSTOLP.STOLPE = TRUE AND  BERSTOLP.BENAMNING MATCHES stolpvar  NO-LOCK BY BERSTOLP.ANTAL:
               CREATE BERMTRL.
               BUFFER-COPY BERSTOLP TO BERMTRL.
               ASSIGN  
               BERMTRL.ANTAL = antalvar
               BERMTRL.NUM = kon_val.NUM
               BERMTRL.SKAPNUM = kon_val.SKAPNUM
               BERMTRL.AONR = valaonr
               /* fler best per dag */
               BERMTRL.OMRADE = valomrade
               BERMTRL.DATUM = TODAY.
               antalvar = 0.
            END.        
         END.
         RELEASE BERMTRL NO-ERROR.
         
      END.
   END.
END PROCEDURE.
PROCEDURE StolpFind_UI :
   DEFINE INPUT-OUTPUT  PARAMETER stolpvarin AS CHARACTER NO-UNDO.
   stolpvarin = '*' + stolpvarin + '*'.
   FIND FIRST BERSTOLP WHERE BERSTOLP.STOLPE = TRUE AND  BERSTOLP.BENAMNING MATCHES stolpvarin NO-LOCK NO-ERROR.
   IF NOT AVAILABLE BERSTOLP THEN DO:
      stolpvarin = REPLACE(stolpvarin," ","  ").
      FIND FIRST BERSTOLP WHERE BERSTOLP.STOLPE = TRUE AND BERSTOLP.BENAMNING MATCHES stolpvarin NO-LOCK NO-ERROR.   
   END.
   IF NOT AVAILABLE BERSTOLP THEN DO:
      stolpvarin = REPLACE(stolpvarin," ","").
      FIND FIRST BERSTOLP WHERE BERSTOLP.STOLPE = TRUE AND  BERSTOLP.BENAMNING MATCHES stolpvarin NO-LOCK NO-ERROR.   
   END.
END PROCEDURE.
PROCEDURE skapa_UI :         
   DO TRANSACTION:
      CREATE BERMTRL.
      BUFFER-COPY MTRLBER TO BERMTRL.
      ASSIGN     
      BERMTRL.NUM = kon_val.NUM
      BERMTRL.SKAPNUM = kon_val.SKAPNUM
      BERMTRL.AONR = valaonr
      /* fler best per dag */
      BERMTRL.OMRADE = valomrade
      BERMTRL.DATUM = TODAY.     
   END.          
END PROCEDURE.      



