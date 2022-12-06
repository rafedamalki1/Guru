/*DAGTEMPBOLAG4.I*/
DEFINE BUFFER dagbuff FOR dagtemp.
DEFINE BUFFER resbuff FOR restid.
IF Guru.Konstanter:varforetypval[18] = 0 OR Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)   THEN.
ELSE DO:
   FOR EACH dagtemp:
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = dagtemp.AONR AND AONRTAB.DELNR = dagtemp.DELNR NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:
         IF AONRTAB.OMRADE = "" THEN DO:
            FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = dagtemp.PERSONALKOD NO-LOCK NO-ERROR.
            IF AVAILABLE PERSONALTAB THEN DO:
               FIND FIRST omvtemp WHERE omvtemp.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
               IF AVAILABLE omvtemp THEN DO:
                  FIND FIRST BOLAGSEK WHERE BOLAGSEK.OMRADE = omvtemp.JUDID AND BOLAGSEK.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR. 
                  IF NOT AVAILABLE BOLAGSEK THEN DO:
                     FOR EACH dagbuff WHERE dagbuff.AONR = dagtemp.AONR AND dagbuff.DELNR = dagtemp.DELNR AND dagbuff.POMRADE = dagtemp.POMRADE:
                        DELETE dagbuff.
                     END. 
                  END.   
                     
               END.                                 
            END.
         END.    
      END.
   END.
   FOR EACH restid:
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = restid.AONR AND AONRTAB.DELNR = restid.DELNR NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:
         IF AONRTAB.OMRADE = "" THEN DO:
            FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = restid.PERSONALKOD NO-LOCK NO-ERROR.
            IF AVAILABLE PERSONALTAB THEN DO:
               FIND FIRST omvtemp WHERE omvtemp.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
               IF AVAILABLE omvtemp THEN DO:
                  FIND FIRST BOLAGSEK WHERE BOLAGSEK.OMRADE = omvtemp.JUDID AND BOLAGSEK.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR. 
                  IF NOT AVAILABLE BOLAGSEK THEN DO:
                     FOR EACH resbuff WHERE resbuff.AONR = restid.AONR AND resbuff.DELNR = restid.DELNR AND restid.OMRADE = restid.OMRADE:
                        DELETE resbuff.
                     END. 
                  END.   
                     
               END.                                 
            END.
         END.    
      END.
   END.   
END.      