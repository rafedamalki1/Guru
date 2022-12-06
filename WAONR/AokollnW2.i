/*AOKOLLNW2.I*/

PROCEDURE tidkoll_UI.
      
   FIND FIRST FORETAG NO-LOCK NO-ERROR.
   Guru.Konstanter:globforetag = FORETAG.FORETAG.
   FIND FIRST ANSTFORMTAB WHERE ANSTFORM.ANSTALLNING = PERSONALTAB.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.  

   IF AONRTAB.AONRAVDATUM = 01/01/1991 OR AONRTAB.AONRAVDATUM >= kolladatumvar THEN Guru.Konstanter:globforetag = Guru.Konstanter:globforetag .
   ELSE DO:
      CREATE felmeddtemp.  
      felmeddtemp.felmedd = Guru.Konstanter:gaol +  AONRTAB.AONR + " " +  STRING(AONRTAB.DELNR,"999") + "�r redan avslutat.".
      RETURN.     
   END.

   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:      
       
      IF PERSONALTAB.OMREGTID = 1 THEN DO:
         IF AONRTAB.AONR = "160" OR AONRTAB.AONR = "161" THEN DO:
            CREATE felmeddtemp.  
            felmeddtemp.felmedd = "Du har inte r�tt till arbetstidf�rkortning enligt avtal".
            RETURN.
            
         END.
      END.
      IF PERSONALTAB.ANSTALLNING = "Ej tidskrivande personal" THEN DO:      
         IF AONRTAB.OMRADE NE "" THEN DO:
            CREATE felmeddtemp.  
            felmeddtemp.felmedd = "Detta " + LC(Guru.Konstanter:gaok) + " f�r inte anv�ndas f�r ditt " + LC(Guru.Konstanter:gomrk).
            RETURN.
         END.      
      END.
      
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:   
         IF AONRTAB.AONR = "940" OR AONRTAB.AONR = "941" OR AONRTAB.AONR = "949"  THEN DO:
            CREATE felmeddtemp.  
            felmeddtemp.felmedd = "Detta " + LC(Guru.Konstanter:gaok) + " f�r inte anv�ndas f�r ditt " + LC(Guru.Konstanter:gomrk).
            RETURN.
         END.   
      END.    
      IF AONRTAB.AONR = "160" OR AONRTAB.AONR = "161" THEN DO:
         IF AONRTAB.AONR = "160"  THEN DO:
            FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
            FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
            FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
            IF AVAILABLE JURPERS  THEN DO:
               IF JURPERS.JUDID = "ELN�T" THEN Guru.Konstanter:globforetag = Guru.Konstanter:globforetag.
               ELSE IF JURPERS.JUDID = "SEAB" THEN Guru.Konstanter:globforetag = Guru.Konstanter:globforetag.
               ELSE IF JURPERS.JUDID = "ServaNet" THEN Guru.Konstanter:globforetag = Guru.Konstanter:globforetag.               
               ELSE IF JURPERS.JUDID = "REKO" THEN Guru.Konstanter:globforetag = Guru.Konstanter:globforetag.               
               ELSE DO:
                  CREATE felmeddtemp.  
                  felmeddtemp.felmedd = LC(Guru.Konstanter:gaok) + " 160 tillh�r en annan " + LC(Guru.Konstanter:gjul).        
                  RETURN.            
               END.
            END.
         END.
         IF AONRTAB.AONR = "161"  THEN DO:
            FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
            FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
            FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
            IF AVAILABLE JURPERS  THEN DO:
               IF JURPERS.JUDID = "SVAB" THEN Guru.Konstanter:globforetag = Guru.Konstanter:globforetag.               
               ELSE DO:
                  CREATE felmeddtemp.  
                  felmeddtemp.felmedd = LC(Guru.Konstanter:gaok) + " 161 tillh�r en annan " + LC(Guru.Konstanter:gjul).        
                  RETURN.            
               END.
            END.
         END.
      END.
      IF AONRTAB.AONR = "140" THEN DO:         
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
         FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
         FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
         IF AVAILABLE JURPERS  THEN DO:
            IF JURPERS.JUDID = "ELN�T" THEN DO: 
               CREATE felmeddtemp.  
               felmeddtemp.felmedd = LC(Guru.Konstanter:gaok) + " 140 tillh�r en annan " + LC(Guru.Konstanter:gjul).        
               RETURN.            
            END.               
            /*ELSE IF JURPERS.JUDID = "SVAB" THEN DO: 
               CREATE felmeddtemp.  
               felmeddtemp.felmedd = LC(Guru.Konstanter:gaok) + " 140 tillh�r en annan " + LC(Guru.Konstanter:gjul).        
               RETURN.            
            END.               */
         END.         
      END.
   END.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF PERSONALTAB.OMREGTID = 1 THEN DO:
         IF AONRTAB.AONR = "160" AND AONRTAB.DELNR = 0 THEN DO:
            CREATE felmeddtemp.  
            felmeddtemp.felmedd = "Arbetstidf�rkortning sp�rrad, h�r med l�nepersonal".
            RETURN.
            
         END.
      END.
   END.
   FIND FIRST AONRTIDLAGE WHERE 
   AONRTIDLAGE.AONR = AONRTAB.AONR AND 
   AONRTIDLAGE.DELNR = AONRTAB.DELNR AND
   AONRTIDLAGE.IDTIDLAG = "TIDSTOPP"
   NO-LOCK NO-ERROR.
   IF AVAILABLE AONRTIDLAGE THEN DO:
      IF AONRTIDLAGE.DATUM1 NE ? THEN DO:        
         IF kolladatumvar > AONRTIDLAGE.DATUM1 THEN DO:
            CREATE felmeddtemp.  
            felmeddtemp.felmedd = "Detta " + LC(Guru.Konstanter:gaok) + " f�r inte anv�ndas f�r tidskrivning.".
            RETURN.
         END.
      END.
   END.     
   DEFINE BUFFER oradebuff FOR OMRADETAB.
   DEFINE BUFFER avdbuff FOR AVDELNING.
   DEFINE BUFFER jurbuff FOR JURPERS.
   /*bolagssp�rr*/
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
      FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
      FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
      FIND FIRST oradebuff WHERE oradebuff.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
      FIND FIRST avdbuff WHERE avdbuff.AVDELNINGNR = oradebuff.AVDELNINGNR NO-LOCK NO-ERROR.
      FIND FIRST jurbuff WHERE jurbuff.JUDID = avdbuff.POSTANST NO-LOCK NO-ERROR.
      IF AVAILABLE jurbuff THEN DO:      
         IF AVAILABLE JURPERS THEN DO:                  
            IF JURPERS.JUDID NE jurbuff.JUDID AND oradebuff.OMRADE NE ""  THEN DO:
               CREATE felmeddtemp.  
               felmeddtemp.felmedd = "Du f�r inte anv�nda en annan " + LC(Guru.Konstanter:gjul)+ " " + LC(Guru.Konstanter:gaok).         
               RETURN.            
            END.
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "cGKAL" THEN DO:
      /*bara ok mellan GKEAB och KEV*/
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
      FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
      FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
      FIND FIRST oradebuff WHERE oradebuff.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
      FIND FIRST avdbuff WHERE avdbuff.AVDELNINGNR = oradebuff.AVDELNINGNR NO-LOCK NO-ERROR.
      FIND FIRST jurbuff WHERE jurbuff.JUDID = avdbuff.POSTANST NO-LOCK NO-ERROR.
      IF AVAILABLE jurbuff THEN DO:      
         IF AVAILABLE JURPERS THEN DO:                  
            IF JURPERS.JUDID NE jurbuff.JUDID AND oradebuff.OMRADE NE ""  THEN DO:
               IF JURPERS.JUDID = "GKEAB" AND jurbuff.JUDID = "KEV" THEN .
               ELSE DO:
                  CREATE felmeddtemp.  
                  felmeddtemp.felmedd = "Du f�r inte anv�nda en annan " + LC(Guru.Konstanter:gjul)+ " " + LC(Guru.Konstanter:gaok).         
                  RETURN.
               END.               
            END.
         END.
      END.
   END.   
   RUN aotidpers_UI.
   
END PROCEDURE.

PROCEDURE aotidpers_UI :
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      RUN FINNSTABELL.P (INPUT "EXTRAKOPPLINGAR", OUTPUT bloblog).
      IF bloblog = FALSE THEN RETURN.
      RUN EXTRATABHMT.P PERSISTENT SET edataapph.                  
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "AOTIDPERS"                   
      inextrakopptemp.KOPPLACHAR1 = AONRTAB.AONR               
      inextrakopptemp.KOPPLAINT1 = AONRTAB.DELNR
      inextrakopptemp.KOPPLACHAR2 = ?
      inextrakopptemp.KOPPLAINT2 =  ?.
      RUN finnsextra_UI IN edataapph (INPUT TABLE inextrakopptemp, OUTPUT bloblog).
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
      IF bloblog = FALSE THEN DO:
         IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.                   
         edataapph = ?.
         RETURN.
      END.
      /*SP�RR FINNS*/     
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "AOTIDPERS"                   
      inextrakopptemp.KOPPLACHAR1 = AONRTAB.AONR               
      inextrakopptemp.KOPPLAINT1 = AONRTAB.DELNR
      inextrakopptemp.KOPPLACHAR2 = PERSONALTAB.PERSONALKOD            
      inextrakopptemp.KOPPLAINT2 =  ?.
      RUN finnsextra_UI IN edataapph (INPUT TABLE inextrakopptemp, OUTPUT bloblog).
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
      /*SP�RR personen ok FINNS*/
      IF bloblog = TRUE THEN DO:
         IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.                   
         edataapph = ?.
         RETURN.
      END.
      CREATE felmeddtemp.  
      felmeddtemp.felmedd = "Detta " + LC(Guru.Konstanter:gaok) + " f�r inte anv�ndas f�r tidskrivning f�r " + PERSONALTAB.PERSONALKOD + ".".
      IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.                         
      edataapph = ?.
   END.
END PROCEDURE.

