/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: FLERBER.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2009.03.30 14:00 ELPAO   
     Modified: 2009.04.29 11:52 ELPAO    
     Modified: 
*/
{FLBERTEMP.I}


PROCEDURE flerbertemph_UI :
   DEFINE OUTPUT PARAMETER TABLE FOR flerbertemp.
END PROCEDURE.
PROCEDURE flerber_UI :
   FOR EACH kon_val BREAK BY kon_val.OMRADE BY kon_val.BERAONR:
      IF LAST-OF(kon_val.BERAONR) THEN DO:
         FIND FIRST flerbertemp WHERE flerbertemp.BERAONR = kon_val.BERAONR AND flerbertemp.OMRADE = kon_val.OMRADE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE flerbertemp THEN CREATE flerbertemp.
         ASSIGN
         flerbertemp.BERAONR = kon_val.BERAONR.
         flerbertemp.OMRADE = kon_val.OMRADE.
      END.
   END. 
   FOR EACH flerbertemp WHERE flerbertemp.BERAONR = "":
      DELETE flerbertemp.
   END.
   FOR EACH flerbertemp:
      FIND FIRST BEREDNING WHERE BEREDNING.BERNR = INTEGER(flerbertemp.BERAONR) AND 
      BEREDNING.OMRADE = flerbertemp.OMRADE USE-INDEX BERNR NO-LOCK NO-ERROR.
      IF AVAILABLE BEREDNING THEN DO:
         IF Guru.Konstanter:globforetag = "cELPA" OR Guru.Konstanter:globforetag = "UMEA" THEN DO:            
            FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = BEREDNING.ANVANDARE
            USE-INDEX ANDV NO-LOCK NO-ERROR.
            IF AVAILABLE ANVANDARE THEN DO:            
               FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = ANVANDARE.PERSONALKOD
               USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
               IF AVAILABLE PERSONALTAB THEN DO: 
                  flerbertemp.BEREDARE = PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN + "     " + PERSONALTAB.TELEFON + "     " + PERSONALTAB.MOBILTEL.
                  Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
               END.
            END.
         END.         
         IF BEREDNING.AONR NE ? THEN DO:
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = BEREDNING.AONR AND
            AONRTAB.DELNR = BEREDNING.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
            IF AVAILABLE AONRTAB THEN DO:
               ASSIGN
               flerbertemp.AONR = AONRTAB.AONR
               flerbertemp.DELNR = AONRTAB.DELNR.            
            END.
         END.
         ASSIGN
         flerbertemp.BERNR = BEREDNING.BERNR
         flerbertemp.BENAMNING = BEREDNING.BENAMNING.
      END.
   END.
END PROCEDURE.
