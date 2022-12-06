  /*xESINOMK.P*/
FUNCTION klockan100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.


FUNCTION klockan60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60 . 

END FUNCTION.

DEFINE VARIABLE  vkdatum AS DATE NO-UNDO. 
DEFINE VARIABLE str AS CHARACTER FORMAT "X(140)" NO-UNDO.
DEFINE VARIABLE progrest AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnold AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnold2 AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.

DEFINE VARIABLE typdatum AS CHARACTER FORMAT "999999" NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE pkod LIKE PERSONALTAB.PERSONALKOD NO-UNDO.
DEFINE VARIABLE aonummer LIKE AONRTAB.AONR NO-UNDO.
DEFINE VARIABLE delnummer LIKE AONRTAB.DELNR NO-UNDO.
DEFINE VARIABLE aoomrade LIKE AONRTAB.OMRADE NO-UNDO.
DEFINE VARIABLE aokategori AS CHARACTER NO-UNDO.
DEFINE VARIABLE kodanst LIKE ANSTFORMTAB.KOD NO-UNDO.
DEFINE TEMP-TABLE ekoforst
   FIELD BOLAG LIKE AVDELNING.AVDELNINGNR
   FIELD GEBOLAG LIKE AVDELNING.AVDELNINGNR
   FIELD BOKDATUM AS CHARACTER
   FIELD DEBKRED AS INTEGER
   FIELD PERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD OMRADE LIKE OMRADETAB.OMRADE
   FIELD GEOMRADE LIKE OMRADETAB.OMRADE
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR
   FIELD ANSTALL AS CHARACTER 
   FIELD ANTAL AS DECIMAL
   FIELD DKONTO1 AS CHARACTER      
   FIELD DKONTO2 AS CHARACTER
   FIELD DKONTO3 AS CHARACTER
   FIELD KKONTO1 AS CHARACTER      
   FIELD KKONTO2 AS CHARACTER
   FIELD KKONTO3 AS CHARACTER
   FIELD BELOPP1 AS DECIMAL 
   FIELD BELOPP2 AS DECIMAL
   FIELD BELOPP3 AS DECIMAL
   FIELD TIDFELKOLL AS LOGICAL 
   FIELD DEBKREDFEL AS LOGICAL
   INDEX PERSORG IS PRIMARY PERSONALKOD OMRADE GEOMRADE ANSTALL.
DEFINE TEMP-TABLE ekout
   FIELD BOLAG LIKE AVDELNING.AVDELNINGNR
   FIELD GEBOLAG LIKE AVDELNING.AVDELNINGNR
   FIELD DATUM AS CHARACTER
   FIELD DEBKRED AS INTEGER  
   FIELD OMRADE LIKE OMRADETAB.OMRADE
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR
   FIELD ANTAL AS DECIMAL
   FIELD KONTO AS CHARACTER      
   FIELD BELOPP AS DECIMAL 
   FIELD TIDFELKOLL AS LOGICAL 
   FIELD DEBKREDFEL AS LOGICAL
   FIELD VERTEXT AS CHARACTER
   INDEX PERSORG IS PRIMARY BOLAG AONR DELNR OMRADE KONTO.      
vkdatum = TODAY.
pkod = "".     
typdatum = STRING(DATE(MONTH(vkdatum),01,YEAR(vkdatum)),"99999999").
RUN infak_UI.
RUN konto_UI.
RUN ut_UI.
RETURN.
PROCEDURE ut_UI:
   IF OPSYS = "UNIX" THEN DO: 
      ASSIGN
      prognamndat = "/u10/guru/export/GURUHBOK.DAT".
      prognamnold = "/u12/guru/export/GURUHBOK." + STRING(TODAY,"99999999").   
   END.    
   ELSE DO: 
      ASSIGN
      prognamndat = "d:\delad\pro8\guru\export\GURUHBOK.DAT"
      prognamnold = "d:\delad\pro8\guru\export\GURUHBOK." + STRING(TODAY,"99999999").   
   END. 
   OUTPUT TO VALUE(prognamndat) APPEND NO-ECHO CONVERT TARGET "swedish-7-bit" SOURCE "iso8859-1".
   FOR EACH ekout:
      str = "".
      SUBSTRING(str,1,3) = STRING(ekout.BOLAG,"999").
      SUBSTRING(str,4,8) = ekout.DATUM.
      SUBSTRING(str,12,5) = ekout.KONTO.      
      SUBSTRING(str,17,4) = ekout.OMRADE.
      SUBSTRING(str,21,8) = STRING(INTEGER(ekout.AONR),"99999999").      
      SUBSTRING(str,29,15) = STRING(ekout.BELOPP * 100,"999999999999999").
      SUBSTRING(str,44,1) = STRING(ekout.DEBKRED,"9").      
      SUBSTRING(str,45,15) = STRING(ekout.ANTAL * 100,"999999999999999").
      SUBSTRING(str,60,1) = STRING(ekout.DEBKRED,"9").
      SUBSTRING(str,61,20) = ekout.VERTEXT.

      PUT UNFORMATTED str AT 1 SKIP. 
   END.
   OUTPUT CLOSE.
   OS-APPEND VALUE(prognamndat) VALUE(prognamnold).   
END PROCEDURE.
PROCEDURE konto_UI:
   FOR EACH ekoforst:
      /*DEBET OMK1*/
      IF ekoforst.BELOPP1 = 0 AND 
         ekoforst.BELOPP2 = 0 AND
         ekoforst.BELOPP3 = 0 THEN DO:
         DELETE ekoforst.
      END.
      ELSE DO:
         IF ekoforst.BELOPP1 NE 0 THEN DO:       
            CREATE ekout.
            ASSIGN
            ekout.BOLAG = ekoforst.GEBOLAG
            ekout.DATUM = typdatum
            ekout.KONTO = ekoforst.DKONTO1
            ekout.OMRADE = ekoforst.GEOMRADE
            ekout.AONR = ekoforst.AONR + STRING(ekoforst.DELNR,"99")
            ekout.BELOPP = ekoforst.BELOPP1
            ekout.ANTAL = ekoforst.ANTAL
            ekout.VERTEXT = "DEBET OMK1"
            ekout.DEBKRED = 1.            
            /*TIDFEL*/
            IF ekoforst.TIDFELKOLL = TRUE THEN DO:
               IF ekoforst.DEBKREDFEL = FALSE THEN DO:
                  ASSIGN
                  ekout.VERTEXT = "FEL OMK1"
                  ekout.DEBKRED = 2.
               END.
            END.
            /*KREDIT OMK1*/
            CREATE ekout.
            ASSIGN
            ekout.BOLAG = ekoforst.BOLAG
            ekout.DATUM = typdatum
            ekout.KONTO = ekoforst.KKONTO1
            ekout.OMRADE = ekoforst.OMRADE
            ekout.AONR = "10999997"
            ekout.BELOPP = ekoforst.BELOPP1
            ekout.ANTAL = ekoforst.ANTAL  
            ekout.VERTEXT = "KREDIT OMK1"
            ekout.DEBKRED = 2.
            /*TIDFEL*/
            IF ekoforst.TIDFELKOLL = TRUE THEN DO:
               IF ekoforst.DEBKREDFEL = FALSE THEN DO:
                  ASSIGN
                  ekout.VERTEXT = "FEL OMK1"
                  ekout.DEBKRED = 1.
               END.
            END.
         END.
         IF ekoforst.BELOPP2 NE 0 THEN DO:
            /*DEBET OMK2*/
            CREATE ekout.
            ASSIGN
            ekout.BOLAG = ekoforst.GEBOLAG
            ekout.DATUM = typdatum
            ekout.KONTO = ekoforst.DKONTO2
            ekout.OMRADE = ekoforst.GEOMRADE
            ekout.AONR = ekoforst.AONR + STRING(ekoforst.DELNR,"99")
            ekout.BELOPP = ekoforst.BELOPP2
            ekout.ANTAL = ekoforst.ANTAL      
            ekout.VERTEXT = "DEBET OMK2"
            ekout.DEBKRED = 1.
            /*TIDFEL*/
            IF ekoforst.TIDFELKOLL = TRUE THEN DO:
               IF ekoforst.DEBKREDFEL = FALSE THEN DO:
                  ASSIGN
                  ekout.VERTEXT = "FEL OMK2"
                  ekout.DEBKRED = 2.
               END.
            END.
            /*KREDIT OMK2*/
            CREATE ekout.
            ASSIGN
            ekout.BOLAG = ekoforst.BOLAG
            ekout.DATUM = typdatum
            ekout.KONTO = ekoforst.KKONTO2
            ekout.OMRADE = ekoforst.OMRADE
            ekout.AONR = "10999997"
            ekout.BELOPP = ekoforst.BELOPP2
            ekout.ANTAL = ekoforst.ANTAL      
            ekout.VERTEXT = "KREDIT OMK2"
            ekout.DEBKRED = 2.
            /*TIDFEL*/
            IF ekoforst.TIDFELKOLL = TRUE THEN DO:
               IF ekoforst.DEBKREDFEL = FALSE THEN DO:
                  ASSIGN
                  ekout.VERTEXT = "FEL OMK2"
                  ekout.DEBKRED = 1.
               END.
            END.
         END.
         IF ekoforst.BELOPP3 NE 0 THEN DO:      
            /*DEBET OMK3*/
            CREATE ekout.
            ASSIGN
            ekout.BOLAG = ekoforst.GEBOLAG
            ekout.DATUM = typdatum
            ekout.KONTO = ekoforst.DKONTO3
            ekout.OMRADE = ekoforst.GEOMRADE
            ekout.AONR = ekoforst.AONR + STRING(ekoforst.DELNR,"99")
            ekout.BELOPP = ekoforst.BELOPP3
            ekout.ANTAL = ekoforst.ANTAL   
            ekout.VERTEXT = "DEBET OMK3"
            ekout.DEBKRED = 1.
            /*TIDFEL*/
            IF ekoforst.TIDFELKOLL = TRUE THEN DO:
               IF ekoforst.DEBKREDFEL = FALSE THEN DO:
                  ASSIGN
                  ekout.VERTEXT = "FEL OMK3"
                  ekout.DEBKRED = 2.
               END.
            END.
            /*KREDIT OMK3*/
            CREATE ekout.
            ASSIGN
            ekout.BOLAG = ekoforst.BOLAG
            ekout.DATUM = typdatum
            ekout.KONTO = ekoforst.KKONTO3
            ekout.OMRADE = ekoforst.OMRADE
            /*ekout.AONR = "1098" + ekoforst.OMRADE PETER N 00/12/01*/
            ekout.AONR = "10999997"
            ekout.BELOPP = ekoforst.BELOPP3
            ekout.ANTAL = ekoforst.ANTAL   
            ekout.VERTEXT = "KREDIT OMK3"
            ekout.DEBKRED = 2.
            /*TIDFEL*/
            IF ekoforst.TIDFELKOLL = TRUE THEN DO:
               IF ekoforst.DEBKREDFEL = FALSE THEN DO:
                  ASSIGN
                  ekout.VERTEXT = "FEL OMK3"
                  ekout.DEBKRED = 1.
               END.
            END.
         END.      
      END.
   END.
   /*TIDFEL*/
   FOR EACH ekoforst WHERE ekoforst.BOLAG NE ekoforst.GEBOLAG AND 
   ekoforst.TIDFELKOLL = FALSE 
   BREAK 
   BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR:
      ACCUMULATE ekoforst.ANTAL   (TOTAL BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP1 (TOTAL BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP2 (TOTAL BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP3 (TOTAL BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR).  
      IF LAST-OF(ekoforst.DELNR) THEN DO:
         /*DEBET UTJÄMIMG*/
         CREATE ekout.
         ASSIGN
         ekout.BOLAG = ekoforst.BOLAG
         ekout.DATUM = typdatum
         ekout.KONTO = "09791"
         ekout.OMRADE = ""
         ekout.AONR = "0" + STRING(ekoforst.GEBOLAG) + ekoforst.OMRADE
         ekout.BELOPP = (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP1) +
                        (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP2) +
                        (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP3)          
         ekout.ANTAL = (ACCUM TOTAL BY ekoforst.DELNR ekoforst.ANTAL) 
         ekout.VERTEXT = "UTJÄMNING DEBET"
         ekout.DEBKRED = 1.
         IF ekoforst.GEBOLAG = 905 THEN ekout.AONR = "0" + STRING(405) + ekoforst.OMRADE.
         IF ekout.BELOPP = 0 THEN DELETE ekout. 
      END.
   END.
   /*TIDFEL*/
   FOR EACH ekoforst WHERE ekoforst.BOLAG NE ekoforst.GEBOLAG AND 
   ekoforst.TIDFELKOLL = FALSE 
   BREAK 
   BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR:
      ACCUMULATE ekoforst.ANTAL   (TOTAL BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP1 (TOTAL BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP2 (TOTAL BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP3 (TOTAL BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR).  
      IF LAST-OF(ekoforst.DELNR) THEN DO:        
         /*KREDIT UTJÄMIMG*/
         CREATE ekout.
         ASSIGN
         ekout.BOLAG = ekoforst.GEBOLAG
         ekout.DATUM = typdatum
         ekout.KONTO = "09790"
         ekout.OMRADE = ""
         ekout.AONR = "0" + STRING(ekoforst.BOLAG) + ekoforst.GEOMRADE
         ekout.BELOPP = (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP1) +
                        (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP2) +
                        (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP3)          
         ekout.ANTAL = (ACCUM TOTAL BY ekoforst.DELNR ekoforst.ANTAL) 
         ekout.VERTEXT = "UTJÄMNING KREDIT"
         ekout.DEBKRED = 2.
         IF ekoforst.BOLAG = 905 THEN ekout.AONR = "0" + STRING(405) + ekoforst.GEOMRADE.
         IF ekout.BELOPP = 0 THEN DELETE ekout.
      END.
   END.
   /*TIDFEL UTJÄMNING*/
   FOR EACH ekoforst WHERE ekoforst.BOLAG NE ekoforst.GEBOLAG AND 
   ekoforst.TIDFELKOLL = TRUE AND ekoforst.DEBKREDFEL = TRUE
   BREAK 
   BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR:
      ACCUMULATE ekoforst.ANTAL   (TOTAL BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP1 (TOTAL BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP2 (TOTAL BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP3 (TOTAL BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR).  
      IF LAST-OF(ekoforst.DELNR) THEN DO:
         /*DEBET UTJÄMIMG*/
         CREATE ekout.
         ASSIGN
         ekout.BOLAG = ekoforst.BOLAG
         ekout.DATUM = typdatum
         ekout.KONTO = "09791"
         ekout.OMRADE = ""
         ekout.AONR = "0" + STRING(ekoforst.GEBOLAG) + ekoforst.OMRADE
         ekout.BELOPP = (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP1) +
                        (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP2) +
                        (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP3)          
         ekout.ANTAL = (ACCUM TOTAL BY ekoforst.DELNR ekoforst.ANTAL) 
         ekout.VERTEXT = "FEL UTJÄMNING DEBET"
         ekout.DEBKRED = 1.
         IF ekoforst.GEBOLAG = 905 THEN ekout.AONR = "0" + STRING(405) + ekoforst.OMRADE.
         IF ekout.BELOPP = 0 THEN DELETE ekout. 
      END.
   END.
   /*TIDFEL UTJÄMNING*/
   FOR EACH ekoforst WHERE ekoforst.BOLAG NE ekoforst.GEBOLAG AND 
   ekoforst.TIDFELKOLL = TRUE AND ekoforst.DEBKREDFEL = TRUE
   BREAK 
   BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR:
      ACCUMULATE ekoforst.ANTAL   (TOTAL BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP1 (TOTAL BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP2 (TOTAL BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP3 (TOTAL BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR).  
      IF LAST-OF(ekoforst.DELNR) THEN DO:        
         /*KREDIT UTJÄMIMG*/
         CREATE ekout.
         ASSIGN
         ekout.BOLAG = ekoforst.GEBOLAG
         ekout.DATUM = typdatum
         ekout.KONTO = "09790"
         ekout.OMRADE = ""
         ekout.AONR = "0" + STRING(ekoforst.BOLAG) + ekoforst.GEOMRADE
         ekout.BELOPP = (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP1) +
                        (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP2) +
                        (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP3)          
         ekout.ANTAL = (ACCUM TOTAL BY ekoforst.DELNR ekoforst.ANTAL) 
         ekout.VERTEXT = "FEL UTJÄMNING KREDIT"
         ekout.DEBKRED = 2.
         IF ekoforst.BOLAG = 905 THEN ekout.AONR = "0" + STRING(405) + ekoforst.GEOMRADE.
         IF ekout.BELOPP = 0 THEN DELETE ekout.
      END.
   END.
   /*TIDFEL UTJÄMNING*/
   FOR EACH ekoforst WHERE ekoforst.BOLAG NE ekoforst.GEBOLAG AND 
   ekoforst.TIDFELKOLL = TRUE AND ekoforst.DEBKREDFEL = FALSE
   BREAK 
   BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR:
      ACCUMULATE ekoforst.ANTAL   (TOTAL BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP1 (TOTAL BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP2 (TOTAL BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP3 (TOTAL BY ekoforst.BOLAG BY ekoforst.GEBOLAG BY ekoforst.OMRADE BY ekoforst.AONR BY ekoforst.DELNR).  
      IF LAST-OF(ekoforst.DELNR) THEN DO:
         /*kredit UTJÄMIMG*/
         CREATE ekout.
         ASSIGN
         ekout.BOLAG = ekoforst.BOLAG
         ekout.DATUM = typdatum
         ekout.KONTO = "09791"
         ekout.OMRADE = ""
         ekout.AONR = "0" + STRING(ekoforst.GEBOLAG) + ekoforst.OMRADE
         ekout.BELOPP = (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP1) +
                        (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP2) +
                        (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP3)          
         ekout.ANTAL = (ACCUM TOTAL BY ekoforst.DELNR ekoforst.ANTAL) 
         ekout.VERTEXT = "FEL UTJÄMNING OMVÄNT"
         ekout.DEBKRED = 2.
         IF ekoforst.GEBOLAG = 905 THEN ekout.AONR = "0" + STRING(405) + ekoforst.OMRADE.
         IF ekout.BELOPP = 0 THEN DELETE ekout. 
      END.
   END.
   /*TIDFEL UTJÄMNING*/
   FOR EACH ekoforst WHERE ekoforst.BOLAG NE ekoforst.GEBOLAG AND 
   ekoforst.TIDFELKOLL = TRUE AND ekoforst.DEBKREDFEL = FALSE
   BREAK 
   BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR:
      ACCUMULATE ekoforst.ANTAL   (TOTAL BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP1 (TOTAL BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP2 (TOTAL BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR).
      ACCUMULATE ekoforst.BELOPP3 (TOTAL BY ekoforst.GEBOLAG BY ekoforst.BOLAG BY ekoforst.GEOMRADE BY ekoforst.AONR BY ekoforst.DELNR).  
      IF LAST-OF(ekoforst.DELNR) THEN DO:        
         /*debet UTJÄMIMG*/
         CREATE ekout.
         ASSIGN
         ekout.BOLAG = ekoforst.GEBOLAG
         ekout.DATUM = typdatum
         ekout.KONTO = "09790"
         ekout.OMRADE = ""
         ekout.AONR = "0" + STRING(ekoforst.BOLAG) + ekoforst.GEOMRADE
         ekout.BELOPP = (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP1) +
                        (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP2) +
                        (ACCUM TOTAL BY ekoforst.DELNR ekoforst.BELOPP3)          
         ekout.ANTAL = (ACCUM TOTAL BY ekoforst.DELNR ekoforst.ANTAL) 
         ekout.VERTEXT = "FEL UTJÄMNING OMVÄNT"
         ekout.DEBKRED = 1.
         IF ekoforst.BOLAG = 905 THEN ekout.AONR = "0" + STRING(405) + ekoforst.GEOMRADE.
         IF ekout.BELOPP = 0 THEN DELETE ekout.
      END.
   END.
END PROCEDURE.
PROCEDURE infak_UI:
   OPEN QUERY persq 
   FOR EACH PERSONALTAB NO-LOCK, 
   EACH OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK,  
   EACH AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR 
   NO-LOCK.
   GET FIRST persq NO-LOCK.
   DO WHILE AVAILABLE(PERSONALTAB):
      ASSIGN
      aoomrade = ""
      aonummer = ""
      aokategori = "".      
      IF pkod NE PERSONALTAB.PERSONALKOD THEN DO:
         pkod = PERSONALTAB.PERSONALKOD.             
         FIND FIRST ANSTFORMTAB WHERE
         ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.
         kodanst = ANSTFORMTAB.KOD.         
      END.  
      OPEN QUERY tq FOR EACH TIDREGITAB WHERE
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.VECKOKORD NE "" 
      NO-LOCK BY TIDREGITAB.AONR BY TIDREGITAB.DELNR. 
      GET FIRST tq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):            
         RUN koll_UI.            
         GET NEXT tq NO-LOCK.    
      END.
      /*TIDFEL*/
      ASSIGN
      aoomrade = ""
      aonummer = ""
      aokategori = "".
      
      OPEN QUERY felq FOR EACH TIDFEL WHERE 
      TIDFEL.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDFEL.FELKORD NE " "  AND TIDFEL.TIDLOG = TRUE 
      NO-LOCK BY TIDFEL.AONR BY TIDFEL.DELNR.
      GET FIRST felq NO-LOCK.
      DO WHILE AVAILABLE(TIDFEL):            
         IF TIDFEL.SKICKA = FALSE THEN vkdatum = vkdatum.
         ELSE RUN kollfel_UI.
         GET NEXT felq NO-LOCK.
      END.
       
      GET NEXT persq NO-LOCK.
   END.        
   OPEN QUERY ebolagq FOR EACH ekoforst, 
   EACH OMRADETAB WHERE OMRADETAB.OMRADE = ekoforst.GEOMRADE NO-LOCK,  
   EACH AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK.
   GET FIRST ebolagq NO-LOCK.
   DO WHILE AVAILABLE(ekoforst):
      ekoforst.GEBOLAG = AVDELNING.AVDELNINGNR.
      GET NEXT ebolagq NO-LOCK.
   END.
END PROCEDURE.
PROCEDURE koll_UI:                            /*DESSA KODER FINNS I N2.P OCH LON_UI*/   
   IF TIDREGITAB.AONR = "" THEN RETURN.
   IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN RETURN.
   IF TIDREGITAB.PRISTYP = "RESTID..." THEN RETURN.   
   IF TIDREGITAB.TIDLOG = FALSE THEN RETURN.
   IF aonummer = TIDREGITAB.AONR AND delnummer = TIDREGITAB.DELNR THEN musz = musz.
   ELSE DO:
      ASSIGN
      aonummer = TIDREGITAB.AONR 
      delnummer = TIDREGITAB.DELNR.
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND
      AONRTAB.DELNR = TIDREGITAB.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE AONRTAB THEN DO:
         aonummer = "".       
         RETURN.            
      END.
      aoomrade = AONRTAB.OMRADE.   
      IF AONRTAB.OMRADE = "" THEN DO:
         aoomrade = PERSONALTAB.OMRADE.
      END.
      FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR = TIDREGITAB.AONR AND
      AONRKONTKOD.DELNR = TIDREGITAB.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE AONRKONTKOD THEN aokategori = AONRKONTKOD.K3.
      IF aoomrade = PERSONALTAB.OMRADE THEN RETURN.
      IF SUBSTRING(aokategori,1,1) = "Z" THEN RETURN.            
   END.
   /*IF aoomrade = PERSONALTAB.OMRADE THEN RETURN. PETER 010516*/
   IF aoomrade NE PERSONALTAB.OMRADE THEN RETURN. 
   IF SUBSTRING(aokategori,1,1) = "Z" THEN RETURN.    
   FIND FIRST OMKOSTNADTAB WHERE OMKOSTNADTAB.OMRADE = PERSONALTAB.OMRADE AND
   SUBSTRING(OMKOSTNADTAB.ANSTALL,1,1) = SUBSTRING(kodanst,1,1)
   NO-LOCK NO-ERROR.   
   /*TIDFEL*/
   FIND FIRST ekoforst WHERE ekoforst.OMRADE = PERSONALTAB.OMRADE AND 
   ekoforst.GEOMRADE = aoomrade AND ekoforst.AONR = aonummer AND 
   ekoforst.DELNR = delnummer AND ekoforst.ANSTALL = SUBSTRING(kodanst,1,1) AND 
   ekoforst.TIDFELKOLL = FALSE 
   NO-ERROR.
   IF NOT AVAILABLE ekoforst THEN CREATE ekoforst. 
   ASSIGN    
   ekoforst.TIDFELKOLL = FALSE
   ekoforst.ANTAL = ekoforst.ANTAL + ROUND(klockan100(TIDREGITAB.TOTALT),2)
   ekoforst.BOLAG = AVDELNING.AVDELNINGNR
   ekoforst.ANSTALL = SUBSTRING(kodanst,1,1)
   ekoforst.OMRADE = PERSONALTAB.OMRADE 
   ekoforst.GEOMRADE = aoomrade
   ekoforst.AONR = aonummer 
   ekoforst.DELNR = delnummer.
   
   IF ekoforst.ANSTALL = "T" THEN DO:
      ASSIGN 
      ekoforst.DKONTO1 = "09720"
      ekoforst.DKONTO2 = "09725"
      ekoforst.DKONTO3 = "09710"
      ekoforst.KKONTO1 = "09721"
      ekoforst.KKONTO2 = "09726"
      ekoforst.KKONTO3 = "09711"
      ekoforst.BELOPP1 = ekoforst.BELOPP1 + 
      ROUND(klockan100(TIDREGITAB.TOTALT),2) * OMKOSTNADTAB.OMK1 
      ekoforst.BELOPP2 = ekoforst.BELOPP2 +
      ROUND(klockan100(TIDREGITAB.TOTALT),2) * OMKOSTNADTAB.OMK2
      ekoforst.BELOPP3 = ekoforst.BELOPP3 + 
      ROUND(klockan100(TIDREGITAB.TOTALT),2) * OMKOSTNADTAB.OMK3.
   END.
   ELSE DO:
      ASSIGN
      ekoforst.DKONTO1 = "09720"
      ekoforst.DKONTO2 = "09725"
      ekoforst.DKONTO3 = "09710"
      ekoforst.KKONTO1 = "09721"
      ekoforst.KKONTO2 = "09726"
      ekoforst.KKONTO3 = "09711"
      ekoforst.BELOPP1 = ekoforst.BELOPP1 + 
      ROUND(klockan100(TIDREGITAB.TOTALT),2) * OMKOSTNADTAB.OMK1 
      ekoforst.BELOPP2 = ekoforst.BELOPP2 +
      ROUND(klockan100(TIDREGITAB.TOTALT),2) * OMKOSTNADTAB.OMK2 
      ekoforst.BELOPP3 = ekoforst.BELOPP3 + 
      ROUND(klockan100(TIDREGITAB.TOTALT),2) * OMKOSTNADTAB.OMK3.   
   END.
   
END PROCEDURE.                 

PROCEDURE kollfel_UI:                            /*DESSA KODER FINNS I N2.P OCH LON_UI*/   
   IF TIDFEL.AONR = "" THEN RETURN.
   IF TIDFEL.PRISTYP = "FRÅNVARO." THEN RETURN.
   IF TIDFEL.PRISTYP = "RESTID..." THEN RETURN.   
   IF TIDFEL.TIDLOG = FALSE THEN RETURN.
   IF aonummer = TIDFEL.AONR AND delnummer = TIDFEL.DELNR THEN musz = musz.
   ELSE DO:
      ASSIGN
      aonummer = TIDFEL.AONR 
      delnummer = TIDFEL.DELNR.
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDFEL.AONR AND
      AONRTAB.DELNR = TIDFEL.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE AONRTAB THEN DO:
         aonummer = "".       
         RETURN.            
      END.
      aoomrade = AONRTAB.OMRADE.   
      IF AONRTAB.OMRADE = "" THEN DO:
         aoomrade = PERSONALTAB.OMRADE.
         RETURN.  
      END.
      FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR = TIDFEL.AONR AND
      AONRKONTKOD.DELNR = TIDFEL.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE AONRKONTKOD THEN aokategori = AONRKONTKOD.K3.
      IF aoomrade = PERSONALTAB.OMRADE THEN RETURN.
      IF SUBSTRING(aokategori,1,1) = "Z" THEN RETURN.            
   END.
   IF aoomrade = PERSONALTAB.OMRADE THEN RETURN.
   IF SUBSTRING(aokategori,1,1) = "Z" THEN RETURN.    
   FIND FIRST OMKOSTNADTAB WHERE OMKOSTNADTAB.OMRADE = PERSONALTAB.OMRADE AND
   SUBSTRING(OMKOSTNADTAB.ANSTALL,1,1) = SUBSTRING(kodanst,1,1)
   NO-LOCK NO-ERROR. 
   /*TIDFEL*/
   FIND FIRST ekoforst WHERE ekoforst.OMRADE = PERSONALTAB.OMRADE AND 
   ekoforst.GEOMRADE = aoomrade AND ekoforst.AONR = aonummer AND 
   ekoforst.DELNR = delnummer AND ekoforst.ANSTALL = SUBSTRING(kodanst,1,1) AND 
   ekoforst.TIDFELKOLL = TRUE AND ekoforst.DEBKREDFEL = TIDFEL.DEBET
   NO-ERROR.
   IF NOT AVAILABLE ekoforst THEN CREATE ekoforst. 
   ASSIGN 
   ekoforst.DEBKREDFEL = TIDFEL.DEBET
   ekoforst.TIDFELKOLL = TRUE
   ekoforst.ANTAL = ekoforst.ANTAL + ROUND(klockan100(TIDFEL.TOTALT),2)
   ekoforst.BOLAG = AVDELNING.AVDELNINGNR
   ekoforst.ANSTALL = SUBSTRING(kodanst,1,1)
   ekoforst.OMRADE = PERSONALTAB.OMRADE 
   ekoforst.GEOMRADE = aoomrade
   ekoforst.AONR = aonummer 
   ekoforst.DELNR = delnummer.   
   IF ekoforst.ANSTALL = "T" THEN DO:
      ASSIGN 
      ekoforst.DKONTO1 = "09720"
      ekoforst.DKONTO2 = "09725"
      ekoforst.DKONTO3 = "09710"
      ekoforst.KKONTO1 = "09721"
      ekoforst.KKONTO2 = "09726"
      ekoforst.KKONTO3 = "09711"
      ekoforst.BELOPP1 = ekoforst.BELOPP1 + 
      ROUND(klockan100(TIDFEL.TOTALT),2) * OMKOSTNADTAB.OMK1 
      ekoforst.BELOPP2 = ekoforst.BELOPP2 +
      ROUND(klockan100(TIDFEL.TOTALT),2) * OMKOSTNADTAB.OMK2
      ekoforst.BELOPP3 = ekoforst.BELOPP3 + 
      ROUND(klockan100(TIDFEL.TOTALT),2) * OMKOSTNADTAB.OMK3.
   END.
   ELSE DO:
      ASSIGN
      ekoforst.DKONTO1 = "09720"
      ekoforst.DKONTO2 = "09725"
      ekoforst.DKONTO3 = "09710"
      ekoforst.KKONTO1 = "09721"
      ekoforst.KKONTO2 = "09726"
      ekoforst.KKONTO3 = "09711"
      ekoforst.BELOPP1 = ekoforst.BELOPP1 + 
      ROUND(klockan100(TIDFEL.TOTALT),2) * OMKOSTNADTAB.OMK1 
      ekoforst.BELOPP2 = ekoforst.BELOPP2 +
      ROUND(klockan100(TIDFEL.TOTALT),2) * OMKOSTNADTAB.OMK2 
      ekoforst.BELOPP3 = ekoforst.BELOPP3 + 
      ROUND(klockan100(TIDFEL.TOTALT),2) * OMKOSTNADTAB.OMK3.   
   END.
   
END PROCEDURE.                 
