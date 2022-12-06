/*SKAPLPER.P*/
  
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE tillrec AS RECID NO-UNDO.   
DEFINE NEW SHARED VARIABLE  regdatum AS DATE NO-UNDO.


DEFINE VARIABLE bdatum AS DATE NO-UNDO.
DEFINE VARIABLE avdatum AS DATE NO-UNDO.
DEFINE VARIABLE arrhjsum LIKE TIDREGITAB.BERANTAL NO-UNDO.    
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE forsta LIKE TIDREGITAB.LONTILLAGG. 
DEFINE VARIABLE vpers LIKE PERSONALTAB.PERSONALKOD. 
DEFINE VARIABLE vomr LIKE PERSONALTAB.OMRADE.
DEFINE VARIABLE tot AS DECIMAL FORMAT "->>>9.99" NO-UNDO.
DEFINE VARIABLE sekunder1 AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE sekunder2 AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE sekunder3 AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE totalt1 AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE luft AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE luft1 AS DECIMAL FORMAT "->>>9.99" NO-UNDO.

DEFINE VARIABLE musz AS LOGICAL NO-UNDO.         

DEFINE NEW SHARED TEMP-TABLE perstill    
   FIELD EFTERNAMN LIKE PERSONALTAB.EFTERNAMN 
   FIELD FORNAMN LIKE PERSONALTAB.FORNAMN 
   FIELD PERSONALKOD LIKE PERSONALTAB.PERSONALKOD   
   FIELD PERSTILLREC AS RECID
   FIELD OMRADE LIKE PERSONALTAB.OMRADE
   FIELD ANSVARIGTIDR LIKE PERSONALTAB.ANSVARIGTIDR
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING. 
   
DEFINE NEW SHARED TEMP-TABLE tillpers
   FIELD LONTILLAGG LIKE LONTILL.LONTILLAGG
   FIELD VILART LIKE LONTILL.LONTILLAGG
   FIELD LONKODTEXT LIKE LONTILL.LONKODTEXT
   FIELD ENHET LIKE LONTILL.ENHET
   FIELD TILLPERSREC AS RECID 
   INDEX LONTILLAGG IS PRIMARY LONTILLAGG ASCENDING.        
   
DEFINE TEMP-TABLE slutsum           
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD NAMN AS CHARACTER FORMAT "X(30)"
   FIELD LONTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD VILART LIKE LONTILL.LONTILLAGG
   FIELD OMRADE LIKE PERSONALTAB.OMRADE
   FIELD ANSVARIGTIDR LIKE PERSONALTAB.ANSVARIGTIDR 
   FIELD ANTAL AS DECIMAL FORMAT "->>>>>9.99" 
   FIELD LONKODTEXT LIKE LONTILL.LONKODTEXT 
   FIELD ENHET LIKE LONTILL.ENHET
   FIELD RESMAL AS CHARACTER
   FIELD DATUM AS DATE
   INDEX PERS IS PRIMARY PERSONALKOD LONTILLAGG
   INDEX LON LONTILLAGG PERSONALKOD 
   INDEX ANSV ANSVARIGTIDR PERSONALKOD LONTILLAGG 
   INDEX ANSV2 ANSVARIGTIDR LONTILLAGG PERSONALKOD
   INDEX OMR OMRADE PERSONALKOD LONTILLAGG 
   INDEX OMR2 OMRADE LONTILLAGG PERSONALKOD.   
    
DEFINE TEMP-TABLE slutsum1           
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD NAMN AS CHARACTER FORMAT "X(5)"
   FIELD GEOMRADE LIKE SUMTID.GEOMRADE 
   FIELD OMRADE LIKE SUMTID.OMRADE.
   
DEFINE TEMP-TABLE dagtemp
   FIELD DATUM LIKE TIDREGITAB.DATUM  
   FIELD LONTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD VILART LIKE TIDREGITAB.LONTILLAGG
   FIELD LONTILLANTAL AS DECIMAL FORMAT "->>>>>9.99"                
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD LONKODTEXT LIKE LONTILL.LONKODTEXT
   FIELD ENHET LIKE LONTILL.ENHET
   FIELD OMRADE LIKE PERSONALTAB.OMRADE
   FIELD ANSVARIGTIDR LIKE PERSONALTAB.ANSVARIGTIDR 
   FIELD PERSTILLREC AS RECID 
   FIELD NAMN AS CHARACTER FORMAT "X(30)" 
   FIELD GEOMRADE AS CHARACTER  
   FIELD RESMAL AS CHARACTER
   INDEX LONTILL IS PRIMARY LONTILLAGG ASCENDING. 
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{KODERAVT.I}
   
/*DEFINE NEW SHARED TEMP-TABLE koder 
   FIELD LONTILLAGG LIKE LONTILL.LONTILLAGG
   FIELD VILART LIKE LONTILL.LONTILLAGG
   FIELD LONKODTEXT LIKE LONTILL.LONKODTEXT
   FIELD ENHET LIKE LONTILL.ENHET 
   INDEX LONTILLAGG IS PRIMARY LONTILLAGG ASCENDING.*/                     
   
{TIDUTTTNEW.I}

DEFINE TEMP-TABLE uppfoltemp   
   FIELD FORETAG LIKE FORETAG.FORETAG
   FIELD SORT AS INTEGER   
   FIELD ALLVAL AS INTEGER   
   FIELD ALLTID AS INTEGER
   FIELD INDATUM AS DATE
   FIELD UTDATUM AS DATE.
   
DEFINE VARIABLE RAD_SORT AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Personal", 1,
"Tillägg", 2
     SIZE 11 BY 3 BGCOLOR 8 NO-UNDO.   
     
DEFINE VARIABLE RAD_ALLTID AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1
     SIZE 70 BY .82
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ansvarig tidredovisare", 1,
"Område", 2,
"Alla", 3,
"Enhet/signatur",4
     SIZE 76.5 BY 1.5
     BGCOLOR 8  NO-UNDO.

DEFINE QUERY persq FOR TIDREGITAB.
DEFINE INPUT PARAMETER TABLE FOR uppfoltemp.
DEFINE INPUT PARAMETER TABLE FOR perstill.
DEFINE INPUT PARAMETER TABLE FOR tillpers.
DEFINE INPUT PARAMETER TABLE FOR koder.
DEFINE INPUT  PARAMETER ejsum AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.

FIND FIRST uppfoltemp NO-LOCK NO-ERROR.
ASSIGN
Guru.Konstanter:globforetag = uppfoltemp.FORETAG 


/*valar = uppfoltemp.VALTAR */
RAD_SORT = uppfoltemp.SORT  

RAD_ALLVAL = uppfoltemp.ALLVAL     
RAD_ALLTID = uppfoltemp.ALLTID     
bdatum = uppfoltemp.INDATUM  
avdatum = uppfoltemp.UTDATUM.


str=
"=====================================================================================".      
   RUN huvud_UI.
   IF musz = FALSE THEN RUN summa_UI.    



{GDPRLOGGCLIENT.I}

PROCEDURE allval1_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
      FIND FIRST slutsum USE-INDEX ANSV NO-LOCK NO-ERROR.
      IF AVAILABLE slutsum THEN DO:
         ASSIGN    
         vpers = " " 
         forsta = " ".         
         FOR EACH slutsum BREAK BY slutsum.ANSVARIGTIDR BY slutsum.PERSONALKOD BY
         slutsum.LONTILLAGG: 
            IF slutsum.ANSVARIGTIDR NE vpers THEN DO:               
               RUN ansvtid_UI.
               RUN rubrik1_UI.
            END.
            RUN sort1_UI.                             
         END.
      END.
      ELSE DO:
         MESSAGE "Det finns inga tillägg att visa." VIEW-AS ALERT-BOX.
         musz = TRUE.
         RETURN.
      END.                                         
END PROCEDURE.


PROCEDURE allval21_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST slutsum USE-INDEX ANSV2 NO-LOCK NO-ERROR.
   IF AVAILABLE slutsum THEN DO:
      ASSIGN    
      vpers = " " 
      forsta = " ".         
      FOR EACH slutsum BREAK BY slutsum.ANSVARIGTIDR BY slutsum.LONTILLAGG BY
      slutsum.PERSONALKOD: 
         IF slutsum.ANSVARIGTIDR NE vpers THEN DO: 
            IF vpers NE " " THEN RUN totsum_UI.                  
            RUN ansvtid_UI.
            RUN rubrik2_UI.   
         END.             
         RUN sort2_UI.
      END. 
      RUN totsum_UI.
   END.      
   ELSE DO:
      MESSAGE "Det finns inga tillägg att visa." VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN.
   END.                                     
END PROCEDURE.


PROCEDURE allval22_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST slutsum USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE slutsum THEN DO:
      ASSIGN    
      vomr = " " 
      forsta = " ".         
      FOR EACH slutsum BREAK BY slutsum.OMRADE BY slutsum.LONTILLAGG BY
      slutsum.PERSONALKOD: 
         IF slutsum.OMRADE NE vomr THEN DO:
            IF vomr NE " " THEN RUN totsum_UI.                
            RUN omrade_UI. 
            RUN rubrik2_UI.                             
         END.             
         RUN sort2_UI.
      END.
      RUN totsum_UI.            
   END.
   ELSE DO:
      MESSAGE "Det finns inga tillägg att visa." VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN.
   END.                                         
END PROCEDURE.

PROCEDURE allval31_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST slutsum USE-INDEX ANSV2 NO-LOCK NO-ERROR.
   IF AVAILABLE slutsum THEN DO:
      ASSIGN    
      vpers = " " 
      forsta = " ".         
      FOR EACH slutsum BREAK BY slutsum.ANSVARIGTIDR BY
      slutsum.PERSONALKOD BY slutsum.LONTILLAGG: 
         IF slutsum.ANSVARIGTIDR NE vpers THEN DO: 
            IF vpers NE " " THEN RUN totsum_UI.                  
            RUN ansvtid_UI.
            RUN rubrik2_UI.   
         END.             
         RUN sort3_UI.
      END. 
      RUN totsum_UI.
   END.      
   ELSE DO:
      MESSAGE "Det finns inga tillägg att visa." VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN.
   END.                                     
END PROCEDURE.


PROCEDURE allval32_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST slutsum USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE slutsum THEN DO:
      ASSIGN    
      vomr = " " 
      forsta = " ".         
      FOR EACH slutsum BREAK BY slutsum.OMRADE BY
      slutsum.PERSONALKOD BY slutsum.LONTILLAGG: 
         IF slutsum.OMRADE NE vomr THEN DO:
            IF vomr NE " " THEN RUN totsum_UI.                
            RUN omrade_UI. 
            RUN rubrik2_UI.                             
         END.             
         RUN sort3_UI.
      END.
      RUN totsum_UI.            
   END.
   ELSE DO:
      MESSAGE "Det finns inga tillägg att visa." VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN.
   END.                                         
END PROCEDURE.


PROCEDURE allval2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST slutsum USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE slutsum THEN DO:
      ASSIGN    
      vomr = " " 
      forsta = " ".         
      FOR EACH slutsum BREAK BY slutsum.OMRADE BY slutsum.PERSONALKOD BY
      slutsum.LONTILLAGG: 
         IF slutsum.OMRADE NE vomr THEN DO:
            RUN omrade_UI.
            RUN rubrik1_UI.
         END. 
         RUN sort1_UI.
      END.                    
   END.
   ELSE DO:
      MESSAGE "Det finns inga tillägg att visa." VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN.
   END.                                         
END PROCEDURE.


PROCEDURE ansvtid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = slutsum.ANSVARIGTIDR
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   vpers = PERSONALTAB.PERSONALKOD.
   CREATE tidut.                             
   CREATE tidut.                  
   SUBSTRING(tidut.UT,1) = "============================================
=================================".
   CREATE tidut.             
   ASSIGN 
   forsta = " "
   SUBSTRING(tidut.UT,1) = "ANSVARIG TIDREDOVISARE"
   SUBSTRING(tidut.UT,27) = PERSONALTAB.PERSONALKOD
   SUBSTRING(tidut.UT,33) = PERSONALTAB.FORNAMN
   SUBSTRING(tidut.UT,49) = PERSONALTAB.EFTERNAMN.
   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD. 
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "============================================
=================================".
   CREATE tidut.    
END PROCEDURE.


PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/ 
   DO TRANSACTION:   
      
      IF musz = TRUE THEN musz = musz.   
      ELSE DO:          
         CREATE tidut. 
         SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").           
         CREATE tidut.
         CREATE tidut.
         IF RAD_SORT = 1 THEN DO:
            IF RAD_ALLTID = 1 THEN      tidut.UT = "PERSONAL-LÖNETILLÄGG    PERIOD".
            ELSE IF RAD_ALLTID = 2 THEN tidut.UT = "PERSONAL-BEREDSKAP      PERIOD".
            ELSE IF RAD_ALLTID = 3 THEN tidut.UT = "PERSONAL-TRAKTAMENTE    PERIOD".
            ELSE IF RAD_ALLTID = 4 THEN tidut.UT = "PERSONAL-ÖVERTID        PERIOD".
            SUBSTRING(tidut.UT,34) = STRING(bdatum) + " - " + STRING(avdatum).
            CREATE tidut.
         END. 
         ELSE DO:
            IF RAD_ALLTID = 1 THEN      tidut.UT = "LÖNETILLÄGG-PERSONAL    PERIOD".
            ELSE IF RAD_ALLTID = 2 THEN tidut.UT = "BEREDSKAP-PERSONAL      PERIOD".             
            ELSE IF RAD_ALLTID = 3 THEN tidut.UT = "TRAKTAMENTE-PERSONAL    PERIOD".
            ELSE IF RAD_ALLTID = 4 THEN tidut.UT = "ÖVERTID-PERSONAL        PERIOD".
            SUBSTRING(tidut.UT,34) = STRING(bdatum) + " - " + STRING(avdatum).
            CREATE tidut.
         END.                                                                             
      END.
   END.                    
END PROCEDURE.

PROCEDURE summa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   EMPTY TEMP-TABLE dagtemp NO-ERROR.    
   EMPTY TEMP-TABLE slutsum NO-ERROR.    
   /*SORTERA PÅ PERSON*/
                       
   IF RAD_SORT = 1 THEN DO:    
      FOR EACH perstill:
         persrec = perstill.PERSTILLREC.         
         IF RAD_ALLTID = 1  THEN DO:         
            FOR EACH tillpers:
               tillrec = tillpers.TILLPERSREC.
               /*FIND koder WHERE RECID(koder) = tillrec NO-LOCK NO-ERROR. */
               FIND FIRST koder WHERE koder.LONTILLAGG = tillpers.LONTILLAGG
               AND koder.VILART = tillpers.VILART AND koder.ENHET = tillpers.ENHET NO-LOCK NO-ERROR.
               OPEN QUERY persq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
               TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.PERSONALKOD = 
               perstill.PERSONALKOD AND TIDREGITAB.LONTILLAGG = koder.LONTILLAGG NO-LOCK.
               GET FIRST persq NO-LOCK.           
               DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
                  CREATE dagtemp.
                  ASSIGN    
                  dagtemp.NAMN = SUBSTRING(perstill.FORNAMN,1,10) + " " + 
                  SUBSTRING(perstill.EFTERNAMN,1,20)
                  dagtemp.LONTILLAGG = TIDREGITAB.LONTILLAGG
                  dagtemp.LONTILLANTAL = TIDREGITAB.LONTILLANTAL 
                  dagtemp.OMRADE = perstill.OMRADE
                  dagtemp.ANSVARIGTIDR = perstill.ANSVARIGTIDR
                  dagtemp.LONKODTEXT = koder.LONKODTEXT
                  dagtemp.ENHET = koder.ENHET            
                  dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  dagtemp.VILART = koder.VILART 
                  dagtemp.RESMAL = TIDREGITAB.RESMAL
                  dagtemp.DATUM = TIDREGITAB.DATUM.
                  IF dagtemp.ENHET = "TI" THEN DO:
                      nytid = TIDREGITAB.LONTILLANTAL.
                      RUN TIMSEK.P.
                      dagtemp.LONTILLANTAL = sekunder / 3600.
                  END.
                  GET NEXT persq NO-LOCK. 
               END.
               CLOSE QUERY persq.
            END.
         END.
         ELSE IF RAD_ALLTID = 2  THEN DO:         
            FOR EACH tillpers:
               tillrec = tillpers.TILLPERSREC.
               /*FIND koder WHERE RECID(koder) = tillrec NO-LOCK NO-ERROR.          */
               FIND FIRST koder WHERE koder.LONTILLAGG = tillpers.LONTILLAGG
               AND koder.VILART = tillpers.VILART AND koder.ENHET = tillpers.ENHET NO-LOCK NO-ERROR.
               OPEN QUERY persq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
               TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.PERSONALKOD = 
               perstill.PERSONALKOD AND TIDREGITAB.BEREDSKAP = koder.LONTILLAGG NO-LOCK.
               GET FIRST persq NO-LOCK.           
               DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
                  CREATE dagtemp.
                  ASSIGN    
                  dagtemp.NAMN = SUBSTRING(perstill.FORNAMN,1,10) + " " + 
                  SUBSTRING(perstill.EFTERNAMN,1,20)
                  dagtemp.LONTILLAGG = TIDREGITAB.BEREDSKAP
                  dagtemp.LONTILLANTAL = TIDREGITAB.BERANTAL 
                  dagtemp.OMRADE = perstill.OMRADE
                  dagtemp.ANSVARIGTIDR = perstill.ANSVARIGTIDR
                  dagtemp.LONKODTEXT = koder.LONKODTEXT
                  dagtemp.ENHET = koder.ENHET            
                  dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  dagtemp.VILART = koder.VILART
                  dagtemp.RESMAL = TIDREGITAB.RESMAL
                  dagtemp.DATUM = TIDREGITAB.DATUM.
                  IF dagtemp.ENHET = "TI" THEN DO:
                      nytid = TIDREGITAB.BERANTAL.
                      RUN TIMSEK.P.
                      dagtemp.LONTILLANTAL = sekunder / 3600.
                  END.
                  GET NEXT persq NO-LOCK. 
               END.
               CLOSE QUERY persq.
            END.
         END.
         ELSE IF RAD_ALLTID = 3  THEN DO:         
            FOR EACH tillpers:
               tillrec = tillpers.TILLPERSREC.
               /*FIND koder WHERE RECID(koder) = tillrec NO-LOCK NO-ERROR.          */
               FIND FIRST koder WHERE koder.LONTILLAGG = tillpers.LONTILLAGG
               AND koder.VILART = tillpers.VILART AND koder.ENHET = tillpers.ENHET NO-LOCK NO-ERROR.
               OPEN QUERY persq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
               TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.PERSONALKOD = 
               perstill.PERSONALKOD AND TIDREGITAB.TRAKTKOD = koder.LONTILLAGG NO-LOCK.
               GET FIRST persq NO-LOCK.           
               DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
                  CREATE dagtemp.
                  ASSIGN    
                  dagtemp.NAMN = SUBSTRING(perstill.FORNAMN,1,10) + " " + 
                  SUBSTRING(perstill.EFTERNAMN,1,20)
                  dagtemp.LONTILLAGG = TIDREGITAB.TRAKTKOD
                  dagtemp.LONTILLANTAL = TIDREGITAB.TRAKTANTAL 
                  dagtemp.OMRADE = perstill.OMRADE
                  dagtemp.ANSVARIGTIDR = perstill.ANSVARIGTIDR
                  dagtemp.LONKODTEXT = koder.LONKODTEXT
                  dagtemp.ENHET = koder.ENHET            
                  dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  dagtemp.VILART = koder.VILART 
                  dagtemp.RESMAL = TIDREGITAB.RESMAL
                  dagtemp.DATUM = TIDREGITAB.DATUM.
                  GET NEXT persq NO-LOCK. 
               END.
               CLOSE QUERY persq.
            END.
         END.
         ELSE IF RAD_ALLTID = 4  THEN DO:         
            FOR EACH tillpers:
               tillrec = tillpers.TILLPERSREC.
               /*FIND koder WHERE RECID(koder) = tillrec NO-LOCK NO-ERROR.          */
               FIND FIRST koder WHERE koder.LONTILLAGG = tillpers.LONTILLAGG
               AND koder.VILART = tillpers.VILART AND koder.ENHET = tillpers.ENHET NO-LOCK NO-ERROR.
               OPEN QUERY persq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
               TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.PERSONALKOD = 
               perstill.PERSONALKOD AND TIDREGITAB.OKOD1 = koder.LONTILLAGG NO-LOCK.
               GET FIRST persq NO-LOCK.           
               DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
                  nytid = TIDREGITAB.OANT1.
                  RUN TIMSEK.P.
                  nytid = sekunder / 3600.
                  CREATE dagtemp.
                  ASSIGN    
                  dagtemp.NAMN = SUBSTRING(perstill.FORNAMN,1,10) + " " + 
                  SUBSTRING(perstill.EFTERNAMN,1,20)
                  dagtemp.LONTILLAGG = TIDREGITAB.OKOD1
                  dagtemp.LONTILLANTAL = nytid
                  dagtemp.OMRADE = perstill.OMRADE
                  dagtemp.ANSVARIGTIDR = perstill.ANSVARIGTIDR
                  dagtemp.LONKODTEXT = koder.LONKODTEXT
                  dagtemp.ENHET = koder.ENHET            
                  dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  dagtemp.VILART = koder.VILART
                  dagtemp.RESMAL = TIDREGITAB.RESMAL
                  dagtemp.DATUM = TIDREGITAB.DATUM.
                  /*luften räknas ut om okod1 > 0*/
                  IF TIDREGITAB.UTRYCKNING = TRUE AND 
                  TIDREGITAB.OVERAUTO = TRUE THEN DO: 
                     ASSIGN
                     luft = 0  
                     luft1 = 0                                      
                     sekunder1 = 0
                     sekunder2 = 0
                     sekunder3 = 0.          
                     IF TIDREGITAB.PRISTYP NE "FRÅNVARO." THEN DO:   
                        IF TIDREGITAB.PRISTYP NE "RESTID..." THEN DO:
                           IF TIDREGITAB.OANT1 > 0  THEN DO:                  
                              nytid = TIDREGITAB.OANT1.
                              RUN TIMSEK.P.
                              sekunder1 = sekunder.                
                           END. 
                           IF TIDREGITAB.OANT2 > 0  THEN DO:                     
                              nytid = TIDREGITAB.OANT2.
                              RUN TIMSEK.P.
                              sekunder2 = sekunder.                
                           END.                    
                           IF TIDREGITAB.OANT3 > 0  THEN DO:                     
                              nytid = TIDREGITAB.OANT3.
                              RUN TIMSEK.P.
                              sekunder3 = sekunder.                
                           END.
                           nytid = TIDREGITAB.TOTALT.
                           RUN TIMSEK.P.
                           totalt1 = sekunder.  
                           /*LUFT*/                           
                           IF TIDREGITAB.OANT1 = 0 AND TIDREGITAB.OANT2 = 0 AND TIDREGITAB.OANT3 = 0
                           THEN nytid = nytid.
                           ELSE DO:
                              luft = luft + ((sekunder1 + sekunder2 + sekunder3) - totalt1).            
                           END.                  
                           sekunder = luft.
                        /*   RUN FSEKTIM.P.*/
                           luft1 = luft / 3600.         
                           luft1 = luft1 * (-1).
                           IF luft1 NE 0 THEN DO:                           
                              CREATE dagtemp.
                              ASSIGN    
                              dagtemp.NAMN = SUBSTRING(perstill.FORNAMN,1,10) + " " + 
                              SUBSTRING(perstill.EFTERNAMN,1,20)
                              dagtemp.LONTILLAGG = "Luft"
                              dagtemp.LONTILLANTAL = luft1
                              dagtemp.OMRADE = perstill.OMRADE
                              dagtemp.ANSVARIGTIDR = perstill.ANSVARIGTIDR
                              dagtemp.LONKODTEXT = "Lufttid"
                              dagtemp.ENHET = "TI"            
                              dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
                              dagtemp.VILART = "Luft". 
                           END.
                        END.
                     END.                     
                  END.
                  GET NEXT persq NO-LOCK. 
               END.
               CLOSE QUERY persq.
               OPEN QUERY pers2q FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
               TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.PERSONALKOD = 
               perstill.PERSONALKOD AND TIDREGITAB.OKOD2 = koder.LONTILLAGG NO-LOCK.
               GET FIRST pers2q NO-LOCK.           
               DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
                  nytid = TIDREGITAB.OANT2.
                  RUN TIMSEK.P.
                  nytid = sekunder / 3600.
                  CREATE dagtemp.
                  ASSIGN    
                  dagtemp.NAMN = SUBSTRING(perstill.FORNAMN,1,10) + " " + 
                  SUBSTRING(perstill.EFTERNAMN,1,20)
                  dagtemp.LONTILLAGG = TIDREGITAB.OKOD2
                  dagtemp.LONTILLANTAL = nytid
                  dagtemp.OMRADE = perstill.OMRADE
                  dagtemp.ANSVARIGTIDR = perstill.ANSVARIGTIDR
                  dagtemp.LONKODTEXT = koder.LONKODTEXT
                  dagtemp.ENHET = koder.ENHET            
                  dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  dagtemp.VILART = koder.VILART
                  dagtemp.RESMAL = TIDREGITAB.RESMAL
                  dagtemp.DATUM = TIDREGITAB.DATUM.                  
                  GET NEXT pers2q NO-LOCK. 
               END.
               CLOSE QUERY pers2q.
               OPEN QUERY pers3q FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
               TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.PERSONALKOD = 
               perstill.PERSONALKOD AND TIDREGITAB.OKOD3 = koder.LONTILLAGG NO-LOCK.
               GET FIRST pers3q NO-LOCK.           
               DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
                  nytid = TIDREGITAB.OANT3.
                  RUN TIMSEK.P.
                  nytid = sekunder / 3600.
                  CREATE dagtemp.
                  ASSIGN    
                  dagtemp.NAMN = SUBSTRING(perstill.FORNAMN,1,10) + " " + 
                  SUBSTRING(perstill.EFTERNAMN,1,20)
                  dagtemp.LONTILLAGG = TIDREGITAB.OKOD3
                  dagtemp.LONTILLANTAL = nytid
                  dagtemp.OMRADE = perstill.OMRADE
                  dagtemp.ANSVARIGTIDR = perstill.ANSVARIGTIDR
                  dagtemp.LONKODTEXT = koder.LONKODTEXT
                  dagtemp.ENHET = koder.ENHET            
                  dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  dagtemp.VILART = koder.VILART
                  dagtemp.RESMAL = TIDREGITAB.RESMAL
                  dagtemp.DATUM = TIDREGITAB.DATUM.                   
                  GET NEXT pers3q NO-LOCK. 
               END.
               CLOSE QUERY pers3q.
            END.
         END.
      END.
      IF RAD_ALLTID = 4 THEN DO:  /*Övertid skall ha summa per person*/
         RUN lontill2_UI.
         IF RAD_ALLVAL = 1 THEN DO:
            RUN allval31_UI.
         END. 
         ELSE IF RAD_ALLVAL = 2 THEN DO:
            RUN allval32_UI.
         END.
         ELSE DO: 
            RUN rubrik1_UI. 
            FIND FIRST slutsum USE-INDEX PERS NO-LOCK NO-ERROR.
            IF AVAILABLE slutsum THEN DO:            
               forsta = slutsum.PERSONALKOD.            
               FOR EACH slutsum BREAK  BY slutsum.PERSONALKOD BY slutsum.LONTILLAGG: 
                  RUN sort3_UI.
               END.
               RUN totsum_UI.
            END.   
            ELSE DO:
               MESSAGE "Det finns inga tillägg att visa." VIEW-AS ALERT-BOX.
               musz = TRUE.
               RETURN.
            END.                                   
         END.
      END.
      ELSE DO:
         RUN lontill_UI.
         IF RAD_ALLVAL = 1 THEN DO:
            RUN allval1_UI.
         END. 
         ELSE IF RAD_ALLVAL = 2 THEN DO:
            RUN allval2_UI.
         END.
         ELSE DO:
            RUN rubrik1_UI.  
            FIND FIRST slutsum USE-INDEX PERS NO-LOCK NO-ERROR.
            IF AVAILABLE slutsum THEN DO:
               ASSIGN                
               forsta = slutsum.PERSONALKOD.         
               FOR EACH slutsum BREAK BY slutsum.PERSONALKOD BY slutsum.LONTILLAGG:             
                  RUN sort1_UI. 
               END.
            END.
            ELSE DO:
               MESSAGE "Det finns inga tillägg att visa." VIEW-AS ALERT-BOX.
               musz = TRUE.
               RETURN.
            END.
         END.  
      END.
   END.                             
   /*SORTERA PÅ TILLÄGG*/   
   IF RAD_SORT = 2 THEN DO:
      FOR EACH tillpers:
         tillrec = tillpers.TILLPERSREC.
         /*FIND koder WHERE RECID(koder) = tillrec NO-LOCK NO-ERROR.          */
         FIND FIRST koder WHERE koder.LONTILLAGG = tillpers.LONTILLAGG
               AND koder.VILART = tillpers.VILART AND koder.ENHET = tillpers.ENHET NO-LOCK NO-ERROR.
         FOR EACH perstill:
            persrec = perstill.PERSTILLREC.                  
            IF RAD_ALLTID = 1  THEN DO:
               OPEN QUERY persq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
               TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.PERSONALKOD = 
               perstill.PERSONALKOD AND TIDREGITAB.LONTILLAGG = koder.LONTILLAGG NO-LOCK.            
               GET FIRST persq NO-LOCK.
               DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
                  CREATE dagtemp.
                  ASSIGN          
                  dagtemp.NAMN = SUBSTRING(perstill.FORNAMN,1,10) + " " + 
                  SUBSTRING(perstill.EFTERNAMN,1,20)
                  dagtemp.LONTILLAGG = TIDREGITAB.LONTILLAGG
                  dagtemp.LONTILLANTAL = TIDREGITAB.LONTILLANTAL
                  dagtemp.LONKODTEXT = koder.LONKODTEXT  
                  dagtemp.OMRADE = perstill.OMRADE
                  dagtemp.ANSVARIGTIDR = perstill.ANSVARIGTIDR
                  dagtemp.ENHET = koder.ENHET            
                  dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  dagtemp.VILART = koder.VILART
                  dagtemp.RESMAL = TIDREGITAB.RESMAL
                  dagtemp.DATUM = TIDREGITAB.DATUM.
                  IF dagtemp.ENHET = "TI" THEN DO:
                      nytid = TIDREGITAB.LONTILLANTAL.
                      RUN TIMSEK.P.
                     dagtemp.LONTILLANTAL = sekunder / 3600.
                  END.
                  GET NEXT persq NO-LOCK. 
               END. 
               CLOSE QUERY persq.  
            END.
            ELSE IF RAD_ALLTID = 2  THEN DO:
               OPEN QUERY persq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
               TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.PERSONALKOD = 
               perstill.PERSONALKOD AND TIDREGITAB.BEREDSKAP = koder.LONTILLAGG NO-LOCK.            
               GET FIRST persq NO-LOCK.
               DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
                  CREATE dagtemp.
                  ASSIGN          
                  dagtemp.NAMN = SUBSTRING(perstill.FORNAMN,1,10) + " " + 
                  SUBSTRING(perstill.EFTERNAMN,1,20)
                  dagtemp.LONTILLAGG = TIDREGITAB.BEREDSKAP
                  dagtemp.LONTILLANTAL = TIDREGITAB.BERANTAL
                  dagtemp.LONKODTEXT = koder.LONKODTEXT  
                  dagtemp.OMRADE = perstill.OMRADE
                  dagtemp.ANSVARIGTIDR = perstill.ANSVARIGTIDR
                  dagtemp.ENHET = koder.ENHET            
                  dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  dagtemp.VILART = koder.VILART 
                  dagtemp.RESMAL = TIDREGITAB.RESMAL
                  dagtemp.DATUM = TIDREGITAB.DATUM.
                  IF dagtemp.ENHET = "TI" THEN DO:
                     nytid = TIDREGITAB.BERANTAL.
                     RUN TIMSEK.P.
                     dagtemp.LONTILLANTAL = sekunder / 3600.
                  END.
                  GET NEXT persq NO-LOCK. 
               END. 
               CLOSE QUERY persq.  
            END.
            ELSE IF RAD_ALLTID = 3  THEN DO:
               OPEN QUERY persq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
               TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.PERSONALKOD = 
               perstill.PERSONALKOD AND TIDREGITAB.TRAKTKOD = koder.LONTILLAGG NO-LOCK.            
               GET FIRST persq NO-LOCK.
               DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
                  CREATE dagtemp.
                  ASSIGN          
                  dagtemp.NAMN = SUBSTRING(perstill.FORNAMN,1,10) + " " + 
                  SUBSTRING(perstill.EFTERNAMN,1,20)
                  dagtemp.LONTILLAGG = TIDREGITAB.TRAKTKOD
                  dagtemp.LONTILLANTAL = TIDREGITAB.TRAKTANTAL
                  dagtemp.LONKODTEXT = koder.LONKODTEXT  
                  dagtemp.OMRADE = perstill.OMRADE
                  dagtemp.ANSVARIGTIDR = perstill.ANSVARIGTIDR
                  dagtemp.ENHET = koder.ENHET            
                  dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  dagtemp.VILART = koder.VILART
                  dagtemp.RESMAL = TIDREGITAB.RESMAL
                  dagtemp.DATUM = TIDREGITAB.DATUM.
                  GET NEXT persq NO-LOCK. 
               END. 
               CLOSE QUERY persq.  
            END.
            ELSE IF RAD_ALLTID = 4  THEN DO:
               OPEN QUERY persq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
               TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.PERSONALKOD = 
               perstill.PERSONALKOD AND TIDREGITAB.OKOD1 = koder.LONTILLAGG NO-LOCK.            
               GET FIRST persq NO-LOCK.
               DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
                  nytid = TIDREGITAB.OANT1.
                  RUN TIMSEK.P.
                  nytid = sekunder / 3600.
                  CREATE dagtemp.
                  ASSIGN          
                  dagtemp.NAMN = SUBSTRING(perstill.FORNAMN,1,10) + " " + 
                  SUBSTRING(perstill.EFTERNAMN,1,20)
                  dagtemp.LONTILLAGG = TIDREGITAB.OKOD1
                  dagtemp.LONTILLANTAL = nytid
                  dagtemp.LONKODTEXT = koder.LONKODTEXT  
                  dagtemp.OMRADE = perstill.OMRADE
                  dagtemp.ANSVARIGTIDR = perstill.ANSVARIGTIDR
                  dagtemp.ENHET = koder.ENHET            
                  dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  dagtemp.VILART = koder.VILART
                  dagtemp.RESMAL = TIDREGITAB.RESMAL
                  dagtemp.DATUM = TIDREGITAB.DATUM. 
                  GET NEXT persq NO-LOCK. 
               END. 
               CLOSE QUERY persq. 
               OPEN QUERY pers2q FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
               TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.PERSONALKOD = 
               perstill.PERSONALKOD AND TIDREGITAB.OKOD2 = koder.LONTILLAGG NO-LOCK.            
               GET FIRST pers2q NO-LOCK.
               DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
                  nytid = TIDREGITAB.OANT2.
                  RUN TIMSEK.P.
                  nytid = sekunder / 3600.
                  CREATE dagtemp.
                  ASSIGN          
                  dagtemp.NAMN = SUBSTRING(perstill.FORNAMN,1,10) + " " + 
                  SUBSTRING(perstill.EFTERNAMN,1,20)
                  dagtemp.LONTILLAGG = TIDREGITAB.OKOD2
                  dagtemp.LONTILLANTAL = nytid
                  dagtemp.LONKODTEXT = koder.LONKODTEXT  
                  dagtemp.OMRADE = perstill.OMRADE
                  dagtemp.ANSVARIGTIDR = perstill.ANSVARIGTIDR
                  dagtemp.ENHET = koder.ENHET            
                  dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  dagtemp.VILART = koder.VILART
                  dagtemp.RESMAL = TIDREGITAB.RESMAL
                  dagtemp.DATUM = TIDREGITAB.DATUM. 
                  GET NEXT pers2q NO-LOCK. 
               END. 
               CLOSE QUERY pers2q.
               OPEN QUERY pers3q FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND
               TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.PERSONALKOD = 
               perstill.PERSONALKOD AND TIDREGITAB.OKOD3 = koder.LONTILLAGG NO-LOCK.            
               GET FIRST pers3q NO-LOCK.
               DO WHILE AVAILABLE(TIDREGITAB) TRANSACTION:
                  nytid = TIDREGITAB.OANT3.
                  RUN TIMSEK.P.
                  nytid = sekunder / 3600.
                  CREATE dagtemp.
                  ASSIGN          
                  dagtemp.NAMN = SUBSTRING(perstill.FORNAMN,1,10) + " " + 
                  SUBSTRING(perstill.EFTERNAMN,1,20)
                  dagtemp.LONTILLAGG = TIDREGITAB.OKOD3
                  dagtemp.LONTILLANTAL = nytid
                  dagtemp.LONKODTEXT = koder.LONKODTEXT  
                  dagtemp.OMRADE = perstill.OMRADE
                  dagtemp.ANSVARIGTIDR = perstill.ANSVARIGTIDR
                  dagtemp.ENHET = koder.ENHET            
                  dagtemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
                  dagtemp.VILART = koder.VILART
                  dagtemp.RESMAL = TIDREGITAB.RESMAL
                  dagtemp.DATUM = TIDREGITAB.DATUM. 
                  GET NEXT pers3q NO-LOCK. 
               END. 
               CLOSE QUERY pers3q.
            END.
         END.
      END.      
      RUN lontill2_UI.
      IF RAD_ALLVAL = 1 THEN DO:
         RUN allval21_UI.
      END. 
      ELSE IF RAD_ALLVAL = 2 THEN DO:
         RUN allval22_UI.
      END.
      ELSE DO: 
         RUN rubrik2_UI. 
         FIND FIRST slutsum USE-INDEX LON NO-LOCK NO-ERROR.
         IF AVAILABLE slutsum THEN DO:            
            forsta = slutsum.LONTILLAGG.            
            FOR EACH slutsum BREAK BY slutsum.LONTILLAGG BY slutsum.PERSONALKOD: 
               RUN sort2_UI.
            END.
            RUN totsum_UI.
         END.   
         ELSE DO:
            MESSAGE "Det finns inga tillägg att visa." VIEW-AS ALERT-BOX.
            musz = TRUE.
            RETURN.
         END.                                   
      END.
   END.       
END PROCEDURE.




PROCEDURE totsum_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   CREATE tidut.
   SUBSTRING(tidut.UT,69) = "==========".
   CREATE tidut. 
   SUBSTRING(tidut.UT,60) = "TOTALT: ". 
   IF tot > 0 THEN DO:
      IF tot < 100 THEN DO:
         SUBSTRING(tidut.UT,69) = STRING(tot,"99.99").
      END.   
      ELSE IF tot < 1000 THEN DO:                                                  
         SUBSTRING(tidut.UT,69) = STRING(tot,">99.99" ).
      END.
      ELSE IF tot < 10000 THEN DO:                                                  
         SUBSTRING(tidut.UT,69) = STRING(tot,">>99.99" ).
      END.            
      ELSE DO:                                                  
         SUBSTRING(tidut.UT,69) = STRING(tot,">>>99.99" ).
      END.            
   END.   
   ASSIGN
   tot = 0.             
   CREATE tidut.    
   SUBSTRING(tidut.UT,1) = str.           
   CREATE tidut.                                              
END PROCEDURE.


PROCEDURE lontill2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*LÖNETILLAGG - PERSON*/
   IF ejsum = FALSE THEN DO:
      ASSIGN 
      arrhjsum = 0. 
      FOR EACH dagtemp BREAK BY dagtemp.LONTILLAGG BY dagtemp.PERSONALKOD:    
         ACCUMULATE dagtemp.LONTILLANTAL (TOTAL BY dagtemp.LONTILLAGG BY dagtemp.PERSONALKOD).        
         IF LAST-OF(dagtemp.PERSONALKOD) THEN DO:
            CREATE slutsum.
            ASSIGN          
            slutsum.LONTILLAGG = dagtemp.LONTILLAGG 
            slutsum.VILART = dagtemp.VILART           
            slutsum.LONKODTEXT = dagtemp.LONKODTEXT
            slutsum.ENHET = dagtemp.ENHET                        
            slutsum.PERSONALKOD = dagtemp.PERSONALKOD 
            slutsum.NAMN = dagtemp.NAMN
            slutsum.OMRADE = dagtemp.OMRADE
            slutsum.ANSVARIGTIDR = dagtemp.ANSVARIGTIDR            
            slutsum.ANTAL = (ACCUM TOTAL BY dagtemp.PERSONALKOD dagtemp.LONTILLANTAL).                  
            /*arrhjsum = ACCUM TOTAL dagtemp.LONTILLANTAL.                              */            
         END.
      END.
   END.   
   ELSE DO:
      FOR EACH dagtemp :    
         /*EJ SUMMERAT*/         
         CREATE slutsum.
         ASSIGN          
         slutsum.DATUM = dagtemp.DATUM
         slutsum.LONTILLAGG = dagtemp.LONTILLAGG  
         slutsum.VILART = dagtemp.VILART          
         slutsum.LONKODTEXT = dagtemp.LONKODTEXT
         slutsum.ENHET = dagtemp.ENHET                        
         slutsum.PERSONALKOD = dagtemp.PERSONALKOD 
         slutsum.NAMN = dagtemp.NAMN 
         slutsum.OMRADE = dagtemp.OMRADE
         slutsum.ANSVARIGTIDR = dagtemp.ANSVARIGTIDR        
         slutsum.ANTAL = dagtemp.LONTILLANTAL
         slutsum.RESMAL = dagtemp.RESMAL.                                                
      
      END.
   END.
      
END PROCEDURE.


PROCEDURE lontill_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*PERSON - LÖNETILLAGG*/
   IF ejsum = FALSE THEN DO:
      ASSIGN 
      arrhjsum = 0. 
      FOR EACH dagtemp BREAK BY dagtemp.LONTILLAGG BY dagtemp.PERSONALKOD:    
         ACCUMULATE dagtemp.LONTILLANTAL (TOTAL BY dagtemp.LONTILLAGG BY dagtemp.PERSONALKOD).        
         IF LAST-OF(dagtemp.PERSONALKOD) THEN DO:
         
            CREATE slutsum.
            ASSIGN          
            slutsum.LONTILLAGG = dagtemp.LONTILLAGG  
            slutsum.VILART = dagtemp.VILART          
            slutsum.LONKODTEXT = dagtemp.LONKODTEXT
            slutsum.ENHET = dagtemp.ENHET                        
            slutsum.PERSONALKOD = dagtemp.PERSONALKOD 
            slutsum.NAMN = dagtemp.NAMN 
            slutsum.OMRADE = dagtemp.OMRADE
            slutsum.ANSVARIGTIDR = dagtemp.ANSVARIGTIDR        
            slutsum.ANTAL = (ACCUM TOTAL BY dagtemp.PERSONALKOD dagtemp.LONTILLANTAL).                  
            /*arrhjsum = ACCUM TOTAL dagtemp.LONTILLANTAL.                              */                  
         END.
      END.
   END.
   ELSE DO:
      FOR EACH dagtemp :    
         /*EJ SUMMERAT*/         
         CREATE slutsum.
         ASSIGN          
         slutsum.DATUM = dagtemp.DATUM
         slutsum.LONTILLAGG = dagtemp.LONTILLAGG  
         slutsum.VILART = dagtemp.VILART          
         slutsum.LONKODTEXT = dagtemp.LONKODTEXT
         slutsum.ENHET = dagtemp.ENHET                        
         slutsum.PERSONALKOD = dagtemp.PERSONALKOD 
         slutsum.NAMN = dagtemp.NAMN 
         slutsum.OMRADE = dagtemp.OMRADE
         slutsum.ANSVARIGTIDR = dagtemp.ANSVARIGTIDR        
         slutsum.ANTAL = dagtemp.LONTILLANTAL
         slutsum.RESMAL = dagtemp.RESMAL.                                                
   
      END.
   END.          

END PROCEDURE.


PROCEDURE omrade_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = slutsum.OMRADE
   USE-INDEX OMR NO-LOCK NO-ERROR.                             
   vomr = OMRADETAB.OMRADE. 
   CREATE tidut. 
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "============================================
=================================".      
   CREATE tidut.                                  
   ASSIGN 
   forsta = " "
   SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrl) + " / ORGANISATION"
   SUBSTRING(tidut.UT,27) = OMRADETAB.OMRADE
   SUBSTRING(tidut.UT,34) = OMRADETAB.NAMN.                            
   CREATE tidut.                  
   SUBSTRING(tidut.UT,1) = "============================================
=================================".
   CREATE tidut.                          
END PROCEDURE.


PROCEDURE rubrik1_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
      ASSIGN
      str=                                                                    
"======.=========================.=====.=======================.====.=========".                  
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = "ENHET/".                                 
      CREATE tidut.      
      ASSIGN                                
      SUBSTRING(tidut.UT,1) = "SIGN"
      SUBSTRING(tidut.UT,8) = "NAMN"                      
      SUBSTRING(tidut.UT,34) = "KOD"  
      SUBSTRING(tidut.UT,40) = "BENÄMNING"
      SUBSTRING(tidut.UT,64) = "ENH"
      SUBSTRING(tidut.UT,69) = "ANTAL".
      IF ejsum = TRUE THEN DO:
         SUBSTRING(tidut.UT,78) = "DATUM".
         SUBSTRING(tidut.UT,88) = "KOMMENTAR".
          str=                                                                    
"======.=========================.=====.=======================.====.========.=========.==============================".      
      END.                                  
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.        
END PROCEDURE.


PROCEDURE rubrik2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
      ASSIGN 
      str = 
"=====.=======================.====.======.=========================.==========".            
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,36) = "ENHET/".                                 
      CREATE tidut.      
      ASSIGN                                
      SUBSTRING(tidut.UT,1) = "KOD"
      SUBSTRING(tidut.UT,7) = "BENÄMNING"                      
      SUBSTRING(tidut.UT,31) = "ENH"  
      SUBSTRING(tidut.UT,36) = "SIGN"
      SUBSTRING(tidut.UT,43) = "NAMN"
      SUBSTRING(tidut.UT,69) = "ANTAL".
      IF ejsum = TRUE THEN DO:
         SUBSTRING(tidut.UT,78) = "DATUM".
         SUBSTRING(tidut.UT,88) = "KOMMENTAR".
         str=                                                                    
"======.=========================.=====.=======================.====.========.=========.=============================".
      END.                         
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.                       
END PROCEDURE.


PROCEDURE sort1_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   CREATE tidut. 
   IF forsta = " " THEN DO:
   END.
   ELSE IF forsta NE slutsum.PERSONALKOD THEN DO:            
      SUBSTRING(tidut.UT,1) = str.           
      CREATE tidut.
   END.       
   ASSIGN  
   SUBSTRING(tidut.UT,1) = slutsum.PERSONALKOD 
   SUBSTRING(tidut.UT,8) = slutsum.NAMN          
   SUBSTRING(tidut.UT,34) = slutsum.VILART
   SUBSTRING(tidut.UT,40) = slutsum.LONKODTEXT 
   SUBSTRING(tidut.UT,64) = slutsum.ENHET.
   IF slutsum.ANTAL > 0 THEN DO:
      IF slutsum.ANTAL < 100 THEN DO:
         SUBSTRING(tidut.UT,69) = STRING(slutsum.ANTAL,"99.99").
      END.   
      ELSE IF slutsum.ANTAL < 1000 THEN DO:                                                  
         SUBSTRING(tidut.UT,69) = STRING(slutsum.ANTAL,">99.99" ).
      END.
      ELSE IF slutsum.ANTAL < 10000 THEN DO:                                                  
         SUBSTRING(tidut.UT,69) = STRING(slutsum.ANTAL,">>99.99" ).
      END.            
      IF ejsum = TRUE THEN DO:
         SUBSTRING(tidut.UT,78) = STRING(slutsum.DATUM,"99/99/99" ).
         SUBSTRING(tidut.UT,88) = slutsum.RESMAL.
      END.
   END.  
   forsta = slutsum.PERSONALKOD.                  
        
END PROCEDURE.




PROCEDURE sort2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
   IF forsta = " " THEN DO:
   END.
   ELSE IF forsta NE slutsum.LONTILLAGG THEN DO:
      RUN totsum_UI.      
   END.    
   CREATE tidut.                              
   ASSIGN  
   SUBSTRING(tidut.UT,1) = slutsum.VILART 
   SUBSTRING(tidut.UT,7) = slutsum.LONKODTEXT          
   SUBSTRING(tidut.UT,31) = slutsum.ENHET
   SUBSTRING(tidut.UT,36) = slutsum.PERSONALKOD 
   SUBSTRING(tidut.UT,43) = slutsum.NAMN.
   
   IF slutsum.ANTAL > 0 THEN DO:
      IF slutsum.ANTAL < 100 THEN DO:
         SUBSTRING(tidut.UT,69) = STRING(slutsum.ANTAL,"99.99").
      END.
      ELSE IF slutsum.ANTAL < 1000 THEN DO:                                                  
         SUBSTRING(tidut.UT,69) = STRING(slutsum.ANTAL,">99.99" ).
      END.
      ELSE IF slutsum.ANTAL < 10000 THEN DO:                                                  
         SUBSTRING(tidut.UT,69) = STRING(slutsum.ANTAL,">>99.99" ).
      END.           
   END.
   IF ejsum = TRUE THEN DO:
      SUBSTRING(tidut.UT,78) = STRING(slutsum.DATUM,"99/99/99" ).
      SUBSTRING(tidut.UT,88) = slutsum.RESMAL.
   END.   
    
   tot = tot + slutsum.ANTAL.
   forsta = slutsum.LONTILLAGG.                                                
END PROCEDURE.

PROCEDURE sort3_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
   IF forsta = " " THEN DO:
   END.
   ELSE IF forsta NE slutsum.PERSONALKOD THEN DO:
      RUN totsum_UI.      
   END.    
   CREATE tidut.  
   ASSIGN  
   SUBSTRING(tidut.UT,1) = slutsum.PERSONALKOD 
   SUBSTRING(tidut.UT,8) = slutsum.NAMN          
   SUBSTRING(tidut.UT,34) = slutsum.VILART
   SUBSTRING(tidut.UT,40) = slutsum.LONKODTEXT 
   SUBSTRING(tidut.UT,64) = slutsum.ENHET.
   IF slutsum.ANTAL > 0 THEN DO:
      IF slutsum.ANTAL < 100 THEN DO:
         SUBSTRING(tidut.UT,69) = STRING(slutsum.ANTAL,"99.99").
      END.
      ELSE IF slutsum.ANTAL < 1000 THEN DO:                                                  
         SUBSTRING(tidut.UT,69) = STRING(slutsum.ANTAL,">99.99" ).
      END.
      ELSE IF slutsum.ANTAL < 10000 THEN DO:                                                  
         SUBSTRING(tidut.UT,69) = STRING(slutsum.ANTAL,">>99.99" ).
      END.            
   END. 
   ELSE DO: 
      SUBSTRING(tidut.UT,67) = STRING(slutsum.ANTAL,"->99.99").      
   END.
   IF ejsum = TRUE THEN DO:
      SUBSTRING(tidut.UT,78) = STRING(slutsum.DATUM,"99/99/99" ).
      SUBSTRING(tidut.UT,88) = slutsum.RESMAL.
   END.
   tot = tot + slutsum.ANTAL.
   forsta = slutsum.PERSONALKOD.                                                
END PROCEDURE.
