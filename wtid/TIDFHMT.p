/*TIDFHMT.P*/
&Scoped-define NEW NEW
{TIDFALLT.I}
DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER brwbdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER brwavdatum AS DATE NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tidfeltemp.

FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600. 

END FUNCTION.
RUN vad_UI.
{GDPRLOGGCLIENT.I}
PROCEDURE vad_UI:
   IF vadgora = 1 THEN DO:
      /*FÖRST GÅNGEN*/
      EMPTY TEMP-TABLE tidfeltemp NO-ERROR.       
      IF pkod = "" THEN DO:
         OPEN QUERY tq FOR EACH TIDFEL WHERE YEAR(TIDFEL.DATUM) = YEAR(brwbdatum)            
         NO-LOCK.            
      END.
      ELSE DO:
         OPEN QUERY tq FOR EACH TIDFEL WHERE TIDFEL.PERSONALKOD = pkod AND
         TIDFEL.DATUM = brwbdatum NO-LOCK.            
      END.          
      GET FIRST tq NO-LOCK.
      DO WHILE AVAILABLE(TIDFEL):         
         IF TIDFEL.TRAKTKOD NE "" THEN DO:
            IF TIDFEL.LONTILLAGG NE "" THEN DO:
               CREATE tidfeltemp.
               BUFFER-COPY TIDFEL EXCEPT TIDFEL.TRAKTAUTO 
               TIDFEL.TRAKTKOD TIDFEL.TRAKTANTAL TO tidfeltemp.
               ASSIGN
               tidfeltemp.TYP = "TID"
               tidfeltemp.RECTIDVIS = RECID(TIDFEL).
               tidfeltemp.TOTALT = klock100(TIDFEL.TOTALT).
               RUN kord_UI.
               CREATE tidfeltemp.
               ASSIGN
               tidfeltemp.TIDLOG = FALSE
               tidfeltemp.PERSONALKOD = TIDFEL.PERSONALKOD
               tidfeltemp.DATUM      = TIDFEL.DATUM      
               tidfeltemp.GODKAND    = TIDFEL.GODKAND    
               tidfeltemp.VECKOKORD  = TIDFEL.VECKOKORD  
               tidfeltemp.DAG         = TIDFEL.DAG
               tidfeltemp.VECKONUMMER = TIDFEL.VECKONUMMER
               tidfeltemp.AONR        = TIDFEL.AONR  
               tidfeltemp.DELNR       = TIDFEL.DELNR
               tidfeltemp.TRAKTAUTO  = TIDFEL.TRAKTAUTO 
               tidfeltemp.TRAKTKOD   = TIDFEL.TRAKTKOD 
               tidfeltemp.TRAKTANTAL = TIDFEL.TRAKTANTAL 
               tidfeltemp.TRAKTAMENTE = TIDFEL.TRAKTAMENTE 
               tidfeltemp.TRAKTTOT   = TIDFEL.TRAKTTOT 
               tidfeltemp.TYP = "TID"
               tidfeltemp.RECTIDVIS = RECID(TIDFEL).
               tidfeltemp.TOTALT = klock100(TIDFEL.TOTALT).
               Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + TIDFEL.PERSONALKOD.
               RUN kord_UI.               
            END.
            ELSE DO:
               CREATE tidfeltemp.
               BUFFER-COPY TIDFEL TO tidfeltemp.               
               ASSIGN
               tidfeltemp.TYP = "TID"
               tidfeltemp.RECTIDVIS = RECID(TIDFEL).
               tidfeltemp.TOTALT = klock100(TIDFEL.TOTALT).
               Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + TIDFEL.PERSONALKOD.
               RUN kord_UI.
            END.
         END.
         ELSE DO:
            CREATE tidfeltemp.
            BUFFER-COPY TIDFEL TO tidfeltemp.
            ASSIGN
            tidfeltemp.TYP = "TID"
            tidfeltemp.RECTIDVIS = RECID(TIDFEL).
            tidfeltemp.TOTALT = klock100(TIDFEL.TOTALT).
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + TIDFEL.PERSONALKOD.
            RUN kord_UI.
         END.
         GET NEXT tq NO-LOCK.
      END.
      pkod = "".
      OPEN QUERY toq FOR EACH tidfeltemp USE-INDEX PERSONALKOD.
      GET FIRST toq NO-LOCK.
      DO WHILE AVAILABLE(tidfeltemp):
         IF pkod NE tidfeltemp.PERSONALKOD THEN DO:
            pkod = tidfeltemp.PERSONALKOD.
            FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
            FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
            USE-INDEX ANSTF NO-LOCK NO-ERROR.
         END.
         IF tidfeltemp.LONTILLAGG NE "" THEN DO:
            FIND FIRST LONTILL WHERE LONTILL.KOD = ANSTFORMTAB.KOD AND 
            LONTILL.LONTILLAGG = tidfeltemp.LONTILLAGG NO-LOCK NO-ERROR.
            IF AVAILABLE LONTILL THEN DO:
               ASSIGN
               tidfeltemp.VILART = LONTILL.VILART 
               tidfeltemp.ENHET = LONTILL.ENHET
               tidfeltemp.PRIS = LONTILL.ERSATTNING.
            END.
            tidfeltemp.TYP = "LON".
         END.
         IF tidfeltemp.BEREDSKAP NE "" THEN DO:
            FIND FIRST BERKOD WHERE BERKOD.BEREDSKAPSAVTAL = PERSONALTAB.BEREDSKAPSAVTAL AND
            BERKOD.BEREDSKAP = tidfeltemp.BEREDSKAP NO-LOCK NO-ERROR.
            IF AVAILABLE BERKOD THEN DO:
               tidfeltemp.VILART = BERKOD.VILART. 
            END.
            tidfeltemp.TYP = "BER".
         END.
         IF tidfeltemp.TRAKTKOD NE "" THEN DO:
            FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAAVTAL = PERSONALTAB.TRAAVTAL AND 
            TRAKTATAB.TRAKTKOD = tidfeltemp.TRAKTKOD NO-LOCK NO-ERROR.    
            IF AVAILABLE TRAKTATAB THEN DO:
               tidfeltemp.VILART = TRAKTATAB.VILART. 
            END.
            tidfeltemp.TYP = "TRA".        
         END.
         GET NEXT toq NO-LOCK.
      END.
   END.
   IF vadgora = 3 THEN DO:
      /*NYA POSTER*/
      FOR EACH tidfeltemp WHERE tidfeltemp.PERSONALKOD = "":
         FIND TIDFEL WHERE RECID(TIDFEL) = tidfeltemp.RECTIDVIS NO-LOCK NO-ERROR.
         IF AVAILABLE TIDFEL THEN DO:
            BUFFER-COPY TIDFEL TO tidfeltemp.
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + TIDFEL.PERSONALKOD.
         END.
         ELSE DELETE tidfeltemp.
      END.
   END.
END PROCEDURE.
PROCEDURE kord_UI:
   ASSIGN
   tidfeltemp.FELKORD   = TIDFEL.FELKORD 
   tidfeltemp.FELKLOCKA = TIDFEL.FELKLOCKA    
   tidfeltemp.FELDATUM  = TIDFEL.FELDATUM 
   tidfeltemp.FELANVAND = TIDFEL.FELANVAND.
   IF tidfeltemp.FELKORD = "" THEN tidfeltemp.TKORD = FALSE.
   ELSE tidfeltemp.TKORD = TRUE.
   FIND FIRST BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = TIDFEL.OVERTIDTILL
   USE-INDEX BEF NO-LOCK NO-ERROR.  
   IF AVAILABLE BEFATTNINGSTAB THEN DO: 
      ASSIGN
      tidfeltemp.VIBEFATTNING = BEFATTNINGSTAB.NAMN.
   END.     
END PROCEDURE.
