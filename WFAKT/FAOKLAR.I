/*FAOKLAR.I*/

   FOR EACH faktaonrtemp: 
      DO TRANSACTION:            
         CREATE FAKTAONR.
         BUFFER-COPY faktaonrtemp TO FAKTAONR.
         IF faktyptemp.TYP = 6 OR faktyptemp.TYP = 7 THEN musz = musz.          
         ELSE DO:
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = FAKTAONR.AONR AND 
            AONRTAB.DELNR = FAKTAONR.DELNR USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.
            /*SNATFAKT*/
            IF AONRTAB.FAKTNR = 0 THEN AONRTAB.FAKTNR = FAKTAONR.FAKTNR.         
         END.   
      END.   
      FIND FIRST FAKTKOLL WHERE FAKTKOLL.FAKTNR = faktaonrtemp.FAKTNR AND
      FAKTKOLL.AONR = faktaonrtemp.AONR  AND
      FAKTKOLL.DELNR = faktaonrtemp.DELNR USE-INDEX FAKTNR NO-LOCK NO-ERROR.          
      IF NOT AVAILABLE FAKTKOLL THEN DO:         
         /*OBS OM TILLVAL AV NR EFTER FAKT*/
         DO TRANSACTION:
            CREATE FAKTKOLL.
            ASSIGN
            FAKTKOLL.FAKTNR = faktaonrtemp.FAKTNR
            FAKTKOLL.AONR = faktaonrtemp.AONR
            FAKTKOLL.DELNR = faktaonrtemp.DELNR
            FAKTKOLL.SLUTFAKT = FALSE.
           /* FAKTKOLL.BESTID = FAKTPLAN.BESTID*/
            IF faktyptemp.TYP = 8 THEN DO: 
               FAKTKOLL.VECKOKORD = "UTAN@".            
            END.
            ELSE IF FAKTPLAN.SENASTFAK NE ? THEN DO:
               RUN senastf_UI (INPUT varsenfak,INPUT varsentid, INPUT varsenvecko).                                                                               
            END.  
            /*SNATFAKT*/
            ELSE DO:
                RUN stopflerplaner_UI (INPUT FAKTKOLL.AONR,INPUT FAKTKOLL.DELNR ,INPUT FAKTKOLL.FAKTNR).               
            END.         
         END.                 
      END.      
   END.
   FOR EACH FAKTAONR WHERE FAKTAONR.FAKTNR = FAKTPLAN.FAKTNR NO-LOCK:
      FIND FIRST FAKTKOLL WHERE FAKTKOLL.FAKTNR = FAKTAONR.FAKTNR AND
      FAKTKOLL.AONR = FAKTAONR.AONR  AND
      FAKTKOLL.DELNR = FAKTAONR.DELNR USE-INDEX FAKTNR NO-LOCK NO-ERROR.          
      IF NOT AVAILABLE FAKTKOLL THEN DO TRANSACTION:
         CREATE FAKTKOLL.
         ASSIGN
         FAKTKOLL.FAKTNR = FAKTAONR.FAKTNR
         FAKTKOLL.AONR = FAKTAONR.AONR
         FAKTKOLL.DELNR = FAKTAONR.DELNR
         FAKTKOLL.SLUTFAKT = FALSE.
         RUN senastf_UI (INPUT varsenfak,INPUT varsentid, INPUT varsenvecko).                      
      END.
   END.
