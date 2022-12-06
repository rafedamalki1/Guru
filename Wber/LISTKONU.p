/*LISTKONU.P*/
{STARTFORAPP.I}   
{KONVALTEMP.I}
{UPPLAGTEMP.I}
DEFINE TEMP-TABLE ord_temp
   FIELD NUM LIKE BERORD.NUM 
   FIELD ORD LIKE BERORD.ORD
   INDEX NUM IS PRIMARY NUM ASCENDING.          
   
DEFINE TEMP-TABLE id_temp  
   FIELD NUM LIKE BERVAL.NUM 
   FIELD GRUPP LIKE BERVAL.KONSKOD  
   FIELD FORNR LIKE BERID.FORNR
   FIELD LINNR LIKE BERID.LINNR
   FIELD NATNR LIKE BERID.NATNR
   FIELD FRI1 LIKE BERID.FRI1
   FIELD FRI2 LIKE BERID.FRI2 
   FIELD XKORD LIKE BERID.XKORD
   FIELD ENDKOMB LIKE BERID.ENDKOMB
   FIELD FRI3 LIKE BERID.FRI3
   FIELD ORD LIKE BERORD.ORD
   INDEX NUM IS PRIMARY NUM ASCENDING
   INDEX ORD FORNR LINNR NATNR FRI1 FRI2 ASCENDING
   INDEX ORD2 ORD ASCENDING. 
{SKAPTEMP.I}   


DEFINE INPUT PARAMETER valaonr LIKE AONRTAB.AONR NO-UNDO.
DEFINE INPUT PARAMETER valomrade LIKE AONRTAB.OMRADE NO-UNDO.
DEFINE INPUT PARAMETER valkonst AS LOGICAL.
DEFINE INPUT PARAMETER TABLE FOR kon_val.

DEFINE OUTPUT PARAMETER TABLE FOR uppl_temp.
DEFINE OUTPUT PARAMETER TABLE FOR skap_temp.
DEFINE OUTPUT PARAMETER TABLE FOR id_temp.              
 
IF valkonst = FALSE THEN DO:   
   OPEN QUERY ordq FOR EACH BERORD WHERE BERORD.AONR = valaonr
   AND BERORD.OMRADE = valomrade USE-INDEX ORD NO-LOCK.
   GET FIRST ordq NO-LOCK.
   DO WHILE AVAILABLE(BERORD):      
      CREATE ord_temp.  
      ASSIGN
      ord_temp.NUM = BERORD.NUM  
      ord_temp.ORD = BERORD.ORD.     
      GET NEXT ordq NO-LOCK.
   END.
   CLOSE QUERY ordq.
   OPEN QUERY upplq FOR EACH BERVAL WHERE BERVAL.AONR = valaonr
   AND BERVAL.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
   GET FIRST upplq NO-LOCK.
   DO WHILE AVAILABLE(BERVAL):
      IF BERVAL.KSKAP = FALSE THEN DO:        
         CREATE uppl_temp.  
         ASSIGN
         uppl_temp.KABNUM = BERVAL.NUM
         uppl_temp.BERAONR = valaonr
         uppl_temp.OMRADE = valomrade
         uppl_temp.NUM = BERVAL.NUM  
         uppl_temp.F1 = BERVAL.KTYPKOD  
         uppl_temp.F2 = BERVAL.F2
         uppl_temp.F3 = BERVAL.F3
         uppl_temp.F4 = BERVAL.F4
         uppl_temp.F5 = BERVAL.F5
         uppl_temp.F6 = BERVAL.F6   
         uppl_temp.UPPLAG = BERVAL.UPPLAG
         uppl_temp.GRUPP = BERVAL.KONSKOD
         uppl_temp.ANMARK = BERVAL.ANMARK.
         FIND FIRST BERUPP WHERE BERUPP.AONR = valaonr AND
         BERUPP.OMRADE = valomrade AND BERUPP.UPPLAG = uppl_temp.UPPLAG NO-LOCK NO-ERROR.
         IF AVAILABLE BERUPP THEN DO:
            ASSIGN
            uppl_temp.ADRESS = BERUPP.ADRESS.
         END.

         FIND FIRST ord_temp WHERE ord_temp.NUM = uppl_temp.NUM
         USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE ord_temp THEN uppl_temp.ORD = ord_temp.ORD.
      END.
      ELSE DO:
         CREATE skap_temp.  
         ASSIGN
         skap_temp.KABNUM = BERVAL.NUM
         skap_temp.NUM = BERVAL.NUM  
         skap_temp.F1 = BERVAL.KTYPKOD  
         skap_temp.F2 = BERVAL.F2
         skap_temp.F3 = BERVAL.F3
         skap_temp.F4 = BERVAL.F4
         skap_temp.F5 = BERVAL.F5
         skap_temp.F6 = BERVAL.F6
         skap_temp.SKAPNUM = BERVAL.SKAPNUM.
      END.   
      GET NEXT upplq NO-LOCK.
   END.
   CLOSE QUERY upplq.       
   OPEN QUERY beridq FOR EACH BERID WHERE BERID.AONR = valaonr AND
   BERID.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
   GET FIRST beridq NO-LOCK.
   DO WHILE AVAILABLE(BERID):
      CREATE id_temp.
      ASSIGN
      id_temp.NUM = BERID.NUM            
      id_temp.FORNR = BERID.FORNR 
      id_temp.LINNR = BERID.LINNR 
      id_temp.NATNR = BERID.NATNR  
      id_temp.FRI1 = BERID.FRI1 
      id_temp.FRI2 = BERID.FRI2 
      id_temp.XKORD = BERID.XKORD  
      id_temp.ENDKOMB = BERID.ENDKOMB
      id_temp.FRI3 = BERID.FRI3.      
      FIND FIRST uppl_temp WHERE uppl_temp.NUM = id_temp.NUM USE-INDEX NUM NO-ERROR.
      IF AVAILABLE uppl_temp THEN DO:
         ASSIGN
         id_temp.GRUPP = uppl_temp.GRUPP
         id_temp.ORD = uppl_temp.ORD.
      END.
        
      GET NEXT beridq NO-LOCK.
   END.
   CLOSE QUERY beridq.
END.
ELSE DO:
   FOR EACH kon_val:
      OPEN QUERY upplq FOR EACH BERVAL WHERE BERVAL.AONR = kon_val.BERAONR
      AND BERVAL.OMRADE = kon_val.OMRADE AND BERVAL.NUM = kon_val.NUM NO-LOCK.
      GET FIRST upplq NO-LOCK.
      DO WHILE AVAILABLE(BERVAL):
         IF BERVAL.KSKAP = FALSE THEN DO:        
            CREATE uppl_temp.  
            ASSIGN
            uppl_temp.KABNUM = kon_val.NUM
            uppl_temp.BERAONR = kon_val.BERAONR
            uppl_temp.OMRADE = kon_val.OMRADE
            uppl_temp.NUM = kon_val.ORD  
            uppl_temp.F1 = BERVAL.KTYPKOD  
            uppl_temp.F2 = BERVAL.F2
            uppl_temp.F3 = BERVAL.F3
            uppl_temp.F4 = BERVAL.F4
            uppl_temp.F5 = BERVAL.F5
            uppl_temp.F6 = BERVAL.F6   
            uppl_temp.UPPLAG = BERVAL.UPPLAG
            uppl_temp.GRUPP = BERVAL.KONSKOD
            uppl_temp.ANMARK = BERVAL.ANMARK
            uppl_temp.ORD = kon_val.ORD.
            FIND FIRST BERUPP WHERE BERUPP.AONR = kon_val.BERAONR AND
            BERUPP.OMRADE = kon_val.OMRADE AND BERUPP.UPPLAG = uppl_temp.UPPLAG NO-LOCK NO-ERROR.
            IF AVAILABLE BERUPP THEN DO:
               ASSIGN
               uppl_temp.ADRESS = BERUPP.ADRESS.
            END.
         END.
         ELSE DO:
            CREATE skap_temp.  
            ASSIGN
            skap_temp.KABNUM = kon_val.NUM
            skap_temp.NUM = kon_val.ORD  
            skap_temp.F1 = BERVAL.KTYPKOD  
            skap_temp.F2 = BERVAL.F2
            skap_temp.F3 = BERVAL.F3
            skap_temp.F4 = BERVAL.F4
            skap_temp.F5 = BERVAL.F5
            skap_temp.F6 = BERVAL.F6
            skap_temp.SKAPNUM = BERVAL.SKAPNUM.
         END.   
         GET NEXT upplq NO-LOCK.
      END.
      CLOSE QUERY upplq.       
      FIND FIRST BERID WHERE BERID.AONR = kon_val.BERAONR AND
      BERID.OMRADE = kon_val.OMRADE AND BERID.NUM = kon_val.NUM NO-LOCK NO-ERROR.         
      IF AVAILABLE BERID THEN DO:         
         CREATE id_temp.
         ASSIGN
         id_temp.NUM = kon_val.ORD            
         id_temp.FORNR = BERID.FORNR 
         id_temp.LINNR = BERID.LINNR 
         id_temp.NATNR = BERID.NATNR  
         id_temp.FRI1 = BERID.FRI1 
         id_temp.FRI2 = BERID.FRI2 
         id_temp.XKORD = kon_val.SKAPNUM  
         id_temp.ENDKOMB = BERID.ENDKOMB
         id_temp.FRI3 = BERID.FRI3.              
         FIND FIRST uppl_temp WHERE uppl_temp.NUM = id_temp.NUM USE-INDEX NUM.
         ASSIGN
         id_temp.GRUPP = uppl_temp.GRUPP
         id_temp.ORD = uppl_temp.ORD.           
      END.         
   END.
END.

RUN kondisp_UI.

PROCEDURE kondisp2_UI :     
   
   {MARKKABVAL3.I}
   IF enval = TRUE THEN DO:
      /*Anders Olsson Elpool i Umeå AB  30 okt 2018 16:44:26 
      ska ej summeras 
      
      FIND FIRST skap_temp WHERE skap_temp.NUM = uppl_temp.NUM AND skap_temp.F2 NE ""  
      USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE skap_temp THEN DO:
         IF uppl_temp.F2 = "" THEN DO:
            uppl_temp.F2 = skap_temp.F2. 
            skap_temp.F2 = "".
         END.  
         IF uppl_temp.F3 = "" THEN DO:
            uppl_temp.F3 = skap_temp.F3. 
            skap_temp.F3 = "".
         END.
         IF uppl_temp.F4 = "" THEN DO:
            uppl_temp.F4 = skap_temp.F4. 
            skap_temp.F4 = "".
         END.
         IF uppl_temp.F5 = "" THEN DO:
            uppl_temp.F5 = skap_temp.F5. 
            skap_temp.F5 = "".
         END.
         IF uppl_temp.F6 = "" THEN DO:
            uppl_temp.F6 = skap_temp.F6. 
            skap_temp.F6 = "".
         END.                      
      END. 
      ELSE DO:
         /*inträffar detta???*/
         FOR EACH skap_temp WHERE skap_temp.NUM = uppl_temp.NUM USE-INDEX NUM NO-LOCK:
            IF uppl_temp.F2 = "" THEN DO:
               uppl_temp.F2 = skap_temp.F2. 
               skap_temp.F2 = "".
            END.  
            IF uppl_temp.F3 = "" THEN DO:
               uppl_temp.F3 = skap_temp.F3. 
               skap_temp.F3 = "".
            END.
            IF uppl_temp.F4 = "" THEN DO:
               uppl_temp.F4 = skap_temp.F4. 
               skap_temp.F4 = "".
            END.
            IF uppl_temp.F5 = "" THEN DO:
               uppl_temp.F5 = skap_temp.F5. 
               skap_temp.F5 = "".
            END.
            IF uppl_temp.F6 = "" THEN DO:
               uppl_temp.F6 = skap_temp.F6. 
               skap_temp.F6 = "".
            END.                             
         END.
      END.
      */          
   END.
   
   ELSE DO:     
      FOR EACH skap_temp WHERE skap_temp.NUM = uppl_temp.NUM USE-INDEX NUM NO-LOCK:
         IF uppl_temp.F2 = "" THEN DO:
            uppl_temp.F2 = skap_temp.F2. 
            skap_temp.F2 = "".
         END.  
         IF uppl_temp.F3 = "" THEN DO:
            uppl_temp.F3 = skap_temp.F3. 
            skap_temp.F3 = "".
         END.
         IF uppl_temp.F4 = "" THEN DO:
            uppl_temp.F4 = skap_temp.F4. 
            skap_temp.F4 = "".
         END.
         IF uppl_temp.F5 = "" THEN DO:
            uppl_temp.F5 = skap_temp.F5. 
            skap_temp.F5 = "".
         END.
         IF uppl_temp.F6 = "" THEN DO:
            uppl_temp.F6 = skap_temp.F6. 
            skap_temp.F6 = "".
         END.                             
      END.
   END.   
   enval = FALSE.                        
END PROCEDURE.

PROCEDURE kondisp_UI :           
   DEFINE VARIABLE kon_rowid AS ROWID NO-UNDO.
   DEFINE BUFFER kdisp FOR kon_val.
   FOR EACH uppl_temp NO-LOCK:
      RUN kondisp2_UI.             
   END.
             
END PROCEDURE.

