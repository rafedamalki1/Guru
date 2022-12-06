PROCEDURE btnner_UI :
   musz = FALSE.
   APPLY "VALUE-CHANGED" TO BRW_FRAN.
   ASSIGN
   forvar = kon_id.FORNR 
   linvar = kon_id.LINNR
   natvar = kon_id.NATNR
   gruppvar = kon_id.GRUPP
   numvar = kon_id.FRI1
   numvar2 = kon_id.FRI2. 
   FIND FIRST annamntemp NO-LOCK NO-ERROR.
   FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = gruppvar NO-LOCK NO-ERROR.
   MESSAGE "Är det korrekt att konstruktion med " + annamntemp.TAB1 + ":" + forvar + " " +
   annamntemp.TAB2 + ":" + linvar + " " + annamntemp.TAB3 + ":" + natvar + " " +
   bbenamntemp.ID1 + ":" + STRING(numvar) + " " + bbenamntemp.ID2 + ":" + STRING(numvar2) + 
   " och efterföljande konstruktioner ska ökas med 1 i fältet för " + bbenamntemp.ID2 + "?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Ändring av identifikation" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:
      musz = TRUE.
   END.
   ELSE DO: 
      musz = FALSE.
   END.         
   IF musz = TRUE THEN DO:       
      EMPTY TEMP-TABLE id_temp NO-ERROR.       
      IF globforetag = "SUND" OR globforetag = "GRAN" OR globforetag = "GKAL" THEN DO:
         FOR EACH kon_id WHERE  
         kon_id.FRI2 >= numvar2 AND kon_id.ENDKOMB = FALSE:
            FIND FIRST id_temp WHERE id_temp.ROWVAR = ROWID(kon_id) USE-INDEX ROWVAR NO-LOCK NO-ERROR.
            IF AVAILABLE id_temp THEN DO:
               musz = musz.
            END.
            ELSE DO:            
               ASSIGN
               kon_id.FRI2 = kon_id.FRI2 + 1.
               FIND FIRST kon_val WHERE kon_val.NUM = kon_id.NUM AND 
               kon_val.KSKAP = FALSE NO-LOCK NO-ERROR.
               /*GGG 051004*/
               ASSIGN
               kon_val.ANDRAD = "AND"
               kon_val.ID2 = STRING(kon_id.FRI2).
               CREATE id_temp.
               id_temp.ROWVAR = ROWID(kon_id).
            END.   
         END.     
         FIND FIRST kon_id WHERE ROWID(kon_id) = selrowvar NO-LOCK NO-ERROR.
         IF AVAILABLE kon_id THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kon_id)).
            RUN lastselectdyn_UI IN brwproc[1]. 
         END.         
      END.
      ELSE DO:               
         FOR EACH kon_id WHERE kon_id.GRUPP = gruppvar AND kon_id.FORNR = forvar AND
         kon_id.LINNR = linvar AND kon_id.NATNR = natvar AND kon_id.FRI1 = numvar AND 
         kon_id.FRI2 >= numvar2 AND kon_id.ENDKOMB = FALSE:
            FIND FIRST id_temp WHERE id_temp.ROWVAR = ROWID(kon_id) USE-INDEX ROWVAR NO-LOCK NO-ERROR.
            IF AVAILABLE id_temp THEN DO:
               musz = musz.
            END.
            ELSE DO:            
               ASSIGN
               kon_id.FRI2 = kon_id.FRI2 + 1.
               FIND FIRST kon_val WHERE kon_val.NUM = kon_id.NUM AND 
               kon_val.KSKAP = FALSE NO-LOCK NO-ERROR.
               /*GGG 051004*/
               ASSIGN
               kon_val.ANDRAD = "AND"
               kon_val.ID2 = STRING(kon_id.FRI2).
               CREATE id_temp.
               id_temp.ROWVAR = ROWID(kon_id).
            END.   
         END. 
         FIND FIRST kon_id WHERE ROWID(kon_id) = selrowvar NO-LOCK NO-ERROR.
         IF AVAILABLE kon_id THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kon_id)).
            RUN lastselectdyn_UI IN brwproc[1]. 
         END.         
      END.         
   END.         
END PROCEDURE.
