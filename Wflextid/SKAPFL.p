/*SKAPFL.P*/
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.
/*DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/

DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.


DEFINE VARIABLE ingsaldo AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE ftot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE mtot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE isaldo AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE hjdat AS DATE NO-UNDO.
DEFINE VARIABLE splus AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(140)" NO-UNDO.
DEFINE VARIABLE manadnr AS INTEGER FORMAT "99" NO-UNDO.

{TIDUTTT.I}
 
DEFINE TEMP-TABLE fltid
   FIELD DATUM LIKE TIDREGITAB.DATUM  
   FIELD PLUS AS DECIMAL
   FIELD KONTROLL LIKE FLEXDAG.KONTROLL                
   FIELD FELMED LIKE FLEXDAG.FELMED FORMAT "X(25)"
   FIELD FELOK LIKE FLEXDAG.FELOK
   FIELD INTID LIKE FLEXTID.TID
   FIELD INKNAPP LIKE FLEXTID.KNAPP
   FIELD INAUTO LIKE FLEXTID.AUTO
   FIELD UTTID LIKE FLEXTID.TID
   FIELD UTKNAPP LIKE FLEXTID.KNAPP
   FIELD UTAUTO LIKE FLEXTID.AUTO
   FIELD LUUT LIKE FLEXTID.TID
   FIELD LUIN LIKE FLEXTID.TID
   FIELD FLUTFM LIKE FLEXTID.TID
   FIELD FLUTEM LIKE FLEXTID.TID
   FIELD FLUTFM2 LIKE FLEXTID.TID
   FIELD FLUTEM2 LIKE FLEXTID.TID
   FIELD FLINFM LIKE FLEXTID.TID
   FIELD FLINFM2 LIKE FLEXTID.TID
   FIELD FLINEM LIKE FLEXTID.TID
   FIELD FLINEM2 LIKE FLEXTID.TID
   FIELD ORSAK LIKE FLEXTID.ORSAK
   FIELD DAG LIKE TIDREGITAB.DAG
   INDEX DATUM IS PRIMARY DATUM ASCENDING.

DEFINE TEMP-TABLE ftid
   FIELD DATUM LIKE FLEXTID.DATUM  
   FIELD KNAPP LIKE FLEXTID.KNAPP
   FIELD TID LIKE FLEXTID.TID
   FIELD KOM LIKE FLEXTID.KOM
   FIELD GICK LIKE FLEXTID.GICK
   FIELD KORD LIKE FLEXTID.KORD
   FIELD AUTO LIKE FLEXTID.AUTO
   FIELD ORSAK LIKE FLEXTID.ORSAK
   INDEX DATUM IS PRIMARY DATUM ASCENDING.

DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER globforetag2 LIKE FORETAG.FORETAG NO-UNDO.
DEFINE INPUT PARAMETER manad AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER sign AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.


FIND PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.  
persrec = RECID(PERSONALTAB).
RUN skapaflex_UI.  
RUN huvud_UI.  
RUN skapaut_UI.

{GDPRLOGGCLIENT.I}
PROCEDURE huvud_UI :
     /*HUVUD*/
   CREATE tidut. 
   IF manad = 1 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Flexrapport ej körda registreringar".
      IF sign = TRUE THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,60) = STRING(TODAY)
         SUBSTRING(tidut.UT,70) = STRING(TIME,"HH:MM:SS").
      END.
      CREATE tidut. 
      ASSIGN
      SUBSTRING(tidut.UT,1) = "======================================" .
      CREATE tidut.
   END.
   ELSE DO:
      ASSIGN
      SUBSTRING(tidut.UT,4) = "Flexrapport för senast körda registreringar" .
      IF sign = TRUE THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,60) = STRING(TODAY)
         SUBSTRING(tidut.UT,70) = STRING(TIME,"HH:MM:SS").
      END.
      CREATE tidut. 
      ASSIGN
      SUBSTRING(tidut.UT,4) = "===============================================" .
      IF sign = TRUE THEN DO:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,4) = "Senaste körning:" 
         SUBSTRING(tidut.UT,21) = STRING(FLEXREG.SALDOKORD).    
         CREATE tidut.
      END.
   END.   
   IF sign = TRUE THEN DO:
      CREATE tidut.  
      ASSIGN
      SUBSTRING(tidut.UT,4) = "Namn:"
      SUBSTRING(tidut.UT,10) = PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN   
      SUBSTRING(tidut.UT,60) = STRING(PERSONALTAB.PERSONNUMMER,"999999-9999") 
      SUBSTRING(tidut.UT,72) = "Sign:"
      SUBSTRING(tidut.UT,78) = PERSONALTAB.PERSONALKOD.  
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      CREATE tidut.  
   END.
   CREATE tidut.                 
   ASSIGN 
   SUBSTRING(tidut.UT,1) = "Ingående flexsaldo:"
   SUBSTRING(tidut.UT,46) = STRING(ingsaldo,"->>9.99").   
   CREATE tidut.                 
   ASSIGN 
   SUBSTRING(tidut.UT,1) = "====================".
   CREATE tidut.
   IF Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE"  THEN DO:  
      CREATE tidut.                                  
      ASSIGN
      SUBSTRING(tidut.UT,2) = "Datum"
      SUBSTRING(tidut.UT,10) = "Dag"
      SUBSTRING(tidut.UT,14) = "In-knapp"
/*      SUBSTRING(tidut.UT,25) = "Auto"*/
      SUBSTRING(tidut.UT,25) = "In"             
      SUBSTRING(tidut.UT,31) = "Ut"
      SUBSTRING(tidut.UT,37) = "Ut-knapp"
     /* SUBSTRING(tidut.UT,53) = "Auto"*/
      SUBSTRING(tidut.UT,48) = "Plus"
      SUBSTRING(tidut.UT,54) = "Ok"
      SUBSTRING(tidut.UT,58) = "Orsak"
      SUBSTRING(tidut.UT,79) = "Felmeddelande"
      SUBSTRING(tidut.UT,109) = "Flex"
      SUBSTRING(tidut.UT,115) = "Flex"
      SUBSTRING(tidut.UT,121) = "Flex"   
      SUBSTRING(tidut.UT,127) = "Flex"   .  
      CREATE tidut.                                  
      ASSIGN
      SUBSTRING(tidut.UT,109) = " Ut"
      SUBSTRING(tidut.UT,115) = " In"
      SUBSTRING(tidut.UT,121) = " Ut"   
      SUBSTRING(tidut.UT,127) = " In".     
      str =                                                                                                                            
      "========.===.==========.=====.=====.==========.=====.===.====================.==============================.=====.=====.=====.=====".             
      CREATE tidut.                  
      ASSIGN
      SUBSTRING(tidut.UT,1) = str.
   END.
   ELSE DO:                                        
      CREATE tidut.                                  
      ASSIGN
      SUBSTRING(tidut.UT,2) = "Datum"
      SUBSTRING(tidut.UT,10) = "In-knapp"
      SUBSTRING(tidut.UT,21) = "Auto"
      SUBSTRING(tidut.UT,26) = "In"     
      SUBSTRING(tidut.UT,32) = "Flex"
      SUBSTRING(tidut.UT,38) = "Flex"
      SUBSTRING(tidut.UT,44) = "Lunch"
      SUBSTRING(tidut.UT,50) = "Lunch"
      SUBSTRING(tidut.UT,56) = "Flex"   
      SUBSTRING(tidut.UT,62) = "Flex"     
      SUBSTRING(tidut.UT,68) = "Ut"
      SUBSTRING(tidut.UT,74) = "Ut-knapp"
      SUBSTRING(tidut.UT,84) = "Auto"
      SUBSTRING(tidut.UT,90) = "Plus"
      SUBSTRING(tidut.UT,98) = "Ok"
      SUBSTRING(tidut.UT,101) = "Felmeddelande".  
      CREATE tidut.                                  
      ASSIGN
      SUBSTRING(tidut.UT,32) = " Ut"
      SUBSTRING(tidut.UT,38) = " In"
      SUBSTRING(tidut.UT,44) = " Ut"
      SUBSTRING(tidut.UT,50) = " In"
      SUBSTRING(tidut.UT,56) = " Ut"   
      SUBSTRING(tidut.UT,62) = " In".     
      str =                                                                                                                            
      "========.==========.====.=====.=====.=====.=====.=====.=====.=====.=====.==========.====.=====.=========================.".             
      CREATE tidut.                  
      ASSIGN
      SUBSTRING(tidut.UT,1) = str.   
   END.      
END PROCEDURE.

PROCEDURE skapaflex_UI :
   FIND FIRST FLEXREG USE-INDEX FLEX NO-LOCK NO-ERROR.
   IF manad = 1 THEN DO:
      OPEN QUERY flgam FOR EACH FLEXTID 
      WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      FLEXTID.KORD = 01/01/97 USE-INDEX KORD NO-LOCK.   
      GET FIRST flgam NO-LOCK.
      DO WHILE AVAILABLE(FLEXTID):
         CREATE ftid.
         ASSIGN
         ftid.DATUM = FLEXTID.DATUM
         ftid.KNAPP = FLEXTID.KNAPP
         ftid.TID = FLEXTID.TID      
         ftid.GICK = FLEXTID.GICK          
         ftid.KOM = FLEXTID.KOM
         ftid.KORD = FLEXTID.KORD
         ftid.AUTO = FLEXTID.AUTO.
         IF FLEXTID.AUTO = "PERI" THEN.
         ELSE ftid.ORSAK = FLEXTID.ORSAK.         
         GET NEXT flgam NO-LOCK.
      END.              
      CLOSE QUERY flgam.    
   END.
   ELSE IF FLEXREG.SALDOKORD > 01/01/97 THEN DO:   
      OPEN QUERY fgam FOR EACH FLEXTID WHERE 
      FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      FLEXTID.KORD = FLEXREG.SALDOKORD USE-INDEX KORD NO-LOCK. 
      GET FIRST fgam NO-LOCK.
      DO WHILE AVAILABLE(FLEXTID):
         CREATE ftid.
         ASSIGN
         ftid.DATUM = FLEXTID.DATUM
         ftid.KNAPP = FLEXTID.KNAPP
         ftid.TID = FLEXTID.TID      
         ftid.GICK = FLEXTID.GICK          
         ftid.KOM = FLEXTID.KOM
         ftid.KORD = FLEXTID.KORD
         ftid.AUTO = FLEXTID.AUTO.
         IF FLEXTID.AUTO = "PERI" THEN.
         ELSE ftid.ORSAK = FLEXTID.ORSAK.
         GET NEXT fgam NO-LOCK.
      END.         
      CLOSE QUERY fgam.                 
   END.
   
   OPEN QUERY fdgam FOR EACH FLEXDAG WHERE FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   FLEXDAG.KORD = 01/01/97 AND FLEXDAG.FELMED = "Ingen registrering gjord" USE-INDEX FLEX NO-LOCK.   
   GET FIRST fdgam NO-LOCK.
   DO WHILE AVAILABLE(FLEXDAG):
      CREATE ftid.
      ASSIGN
      ftid.DATUM = FLEXDAG.DATUM.         
      GET NEXT fdgam NO-LOCK.
   END.              
   CLOSE QUERY fdgam.    
    
   hjdat = 01/01/97.
   OPEN QUERY frapp FOR EACH ftid USE-INDEX DATUM NO-LOCK. 
   GET FIRST frapp NO-LOCK.
   DO WHILE AVAILABLE(ftid):
      IF ftid.DATUM > hjdat THEN DO:    
         CREATE fltid.
         ASSIGN
         fltid.DATUM = ftid.DATUM
         regdatum = ftid.DATUM.
         RUN REGDAG.P.
         RUN REGVEC.P.
         ASSIGN fltid.DAG = regdagnamn.
         RUN SLUTARB.P.
         FIND FIRST FLEXDAG WHERE FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         FLEXDAG.DATUM = ftid.DATUM USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF AVAILABLE FLEXDAG THEN DO:
            ASSIGN
            nytid = FLEXDAG.PLUS.
            RUN TIMSEK.P.
            ASSIGN
            splus = sekunder
            nytid = FLEXDAG.OVUTPLUS.
            RUN TIMSEK.P.
            ASSIGN
            splus = splus + sekunder
            nytid = FLEXDAG.FLARB.
            RUN TIMSEK.P.
            sekunder = splus + sekunder.
            RUN FSEKTIM.P.
            ASSIGN
            fltid.PLUS = fnytid
            fltid.KONTROLL = FLEXDAG.KONTROLL
            fltid.FELMED = FLEXDAG.FELMED
            fltid.FELOK = FLEXDAG.FELOK.
         END.
         hjdat = ftid.DATUM.   
      END.   
      IF ftid.KOM = TRUE THEN DO:     
         ASSIGN
         fltid.INKNAPP = ftid.KNAPP
         fltid.INAUTO = ftid.AUTO
         fltid.INTID = ftid.TID.      
         IF ftid.ORSAK ne "" THEN ASSIGN fltid.ORSAK = ftid.ORSAK.
      END.
      ELSE IF ftid.GICK = TRUE THEN DO:
         ASSIGN
         fltid.UTKNAPP = ftid.KNAPP
         fltid.UTAUTO = ftid.AUTO
         fltid.UTTID = ftid.TID.   
         IF ftid.ORSAK ne "" THEN DO:
            IF fltid.UTAUTO = "GLOM" THEN DO:   
               IF fltid.ORSAK NE ""  THEN ASSIGN fltid.ORSAK = fltid.ORSAK + " " + SUBSTRING(ftid.ORSAK,19,40). 
               ELSE ASSIGN fltid.ORSAK = ftid.ORSAK. 
            END.
            ELSE IF fltid.UTAUTO = "PERI" THEN DO:    
               ASSIGN fltid.ORSAK = ftid.ORSAK. 
            END.
            ELSE DO:            
               IF fltid.ORSAK NE ""  THEN ASSIGN fltid.ORSAK = fltid.ORSAK + " " + SUBSTRING(ftid.ORSAK,1,59).
               ELSE ASSIGN fltid.ORSAK = ftid.ORSAK. 
            END.            
         END.
      END.
      ELSE IF ftid.KNAPP = "LUNCH UT" THEN DO:
         ASSIGN fltid.LUUT = ftid.TID.   
      END.
      ELSE IF ftid.KNAPP = "LUNCH IN" THEN DO:
         ASSIGN fltid.LUIN = ftid.TID.   
      END.
      ELSE IF ftid.KNAPP = "FLEX UT" THEN DO:
         IF ftid.TID < lunchslutet THEN DO:
            IF fltid.FLUTFM > 0 THEN DO:
               ASSIGN fltid.FLUTFM2 = ftid.TID. 
            END.
            ELSE ASSIGN fltid.FLUTFM = ftid.TID.     
         END.   
         ELSE IF ftid.TID GE lunchslutet THEN DO:
            IF fltid.FLUTEM > 0 THEN DO:
               ASSIGN fltid.FLUTEM2 = ftid.TID. 
            END.
            ELSE ASSIGN fltid.FLUTEM = ftid.TID.   
         END.   
      END.
      ELSE IF ftid.KNAPP = "FLEX IN" THEN DO:
         IF ftid.TID LE lunchslutet THEN DO:
            IF fltid.FLINFM > 0 THEN DO:
               ASSIGN fltid.FLINFM2 = ftid.TID. 
            END.
            ELSE ASSIGN fltid.FLINFM = ftid.TID.   
         END.   
         ELSE IF ftid.TID > lunchslutet THEN DO:
            IF fltid.FLINEM > 0 THEN DO:
               ASSIGN fltid.FLINEM2 = ftid.TID. 
            END.
            ELSE ASSIGN fltid.FLINEM = ftid.TID.   
         END.   
      END.
      GET NEXT frapp NO-LOCK.
   END.   
   CLOSE QUERY frapp.   
   isaldo = 0.
   FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = PERSONALTAB.PERSONALKOD USE-INDEX PKOD NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE FLEXSALDO THEN DO:
      CREATE FLEXSALDO.
      ASSIGN FLEXSALDO.PERSONALKOD = PERSONALTAB.PERSONALKOD.
   END.   
   IF manad = 1 THEN DO:
      nytid = FLEXSALDO.ACCFLEX.
      RUN TIMSEK.P.
      ASSIGN
      isaldo = sekunder
      nytid = FLEXSALDO.PERIODFLEX.
      RUN TIMSEK.P.
      ASSIGN
      isaldo = isaldo + sekunder.
      sekunder = isaldo.
      RUN FSEKTIM.P.
      ASSIGN
      ingsaldo = fnytid
      mtot = FLEXSALDO.EJKORDFLEX
      nytid = FLEXSALDO.EJKORDFLEX.
      RUN TIMSEK.P.
      isaldo = isaldo + sekunder.
      sekunder = isaldo.
      RUN FSEKTIM.P.
      ftot = fnytid.
   END.   
   ELSE IF manad = 0 THEN DO:
      ingsaldo = FLEXSALDO.ACCFLEX.
      nytid = FLEXSALDO.ACCFLEX.
      RUN TIMSEK.P.
      ASSIGN
      isaldo = sekunder
      mtot = FLEXSALDO.PERIODFLEX
      nytid = FLEXSALDO.PERIODFLEX.
      RUN TIMSEK.P.
      ASSIGN
      isaldo = isaldo + sekunder.
      sekunder = isaldo.
      RUN FSEKTIM.P.     
      ftot = fnytid.
   END.    
END PROCEDURE.

PROCEDURE skapaut_UI :
    FOR EACH fltid USE-INDEX DATUM: 
      IF Guru.Konstanter:globforetag = "SUFL" OR  Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
         CREATE tidut. 
         ASSIGN
         SUBSTRING(tidut.UT,1) = STRING(fltid.DATUM,"99/99/99")          
         SUBSTRING(tidut.UT,10) = STRING(fltid.DAG)
         SUBSTRING(tidut.UT,14) = SUBSTRING(fltid.INKNAPP,1,10)
        /* SUBSTRING(tidut.UT,25) = SUBSTRING(fltid.INAUTO,1,4)*/
         SUBSTRING(tidut.UT,25) = STRING(fltid.INTID,">9.99")
         SUBSTRING(tidut.UT,30) = "-".   
         ASSIGN
         SUBSTRING(tidut.UT,31) = STRING(fltid.UTTID,">9.99")
         SUBSTRING(tidut.UT,37) = SUBSTRING(fltid.UTKNAPP,1,10)
         /*SUBSTRING(tidut.UT,53) = SUBSTRING(fltid.UTAUTO,1,4)*/
         SUBSTRING(tidut.UT,47) = STRING(fltid.PLUS,"->9.99")
         SUBSTRING(tidut.UT,54) = STRING(fltid.FELOK,"Ja/Nej").         
         IF LENGTH(fltid.ORSAK) > 20 THEN DO:         
            IF fltid.INAUTO = "GLOM" OR fltid.UTAUTO = "GLOM" THEN DO:   
               SUBSTRING(tidut.UT,58) = SUBSTRING(fltid.ORSAK,1,18).
            END.
            ELSE SUBSTRING(tidut.UT,58) = SUBSTRING(fltid.ORSAK,1,18).
         END. 
         ELSE SUBSTRING(tidut.UT,58) = fltid.ORSAK.
         ASSIGN SUBSTRING(tidut.UT,79) = fltid.FELMED.           
         IF fltid.FLUTFM > 0 THEN ASSIGN SUBSTRING(tidut.UT,109) = STRING(fltid.FLUTFM,">9.99").
         IF fltid.FLINFM > 0 THEN ASSIGN SUBSTRING(tidut.UT,115) = STRING(fltid.FLINFM,">9.99").
         IF fltid.FLUTEM > 0 THEN ASSIGN SUBSTRING(tidut.UT,121) = STRING(fltid.FLUTEM,">9.99").
         IF fltid.FLINEM > 0 THEN ASSIGN SUBSTRING(tidut.UT,127) = STRING(fltid.FLINEM,">9.99").
         IF fltid.FLUTFM2 > 0  OR fltid.FLINFM2 > 0 OR fltid.FLUTEM2 > 0 
         OR fltid.FLINEM2 > 0 THEN DO:
            CREATE tidut. 
            CREATE tidut.         
            IF fltid.FLUTFM2 > 0 THEN ASSIGN SUBSTRING(tidut.UT,109) = STRING(fltid.FLUTFM2,">9.99").
            IF fltid.FLINFM2 > 0 THEN ASSIGN SUBSTRING(tidut.UT,115) = STRING(fltid.FLINFM2,">9.99").
            IF fltid.FLUTEM2 > 0 THEN ASSIGN SUBSTRING(tidut.UT,121) = STRING(fltid.FLUTEM2,">9.99").
            IF fltid.FLINEM2 > 0 THEN ASSIGN SUBSTRING(tidut.UT,127) = STRING(fltid.FLINEM2,">9.99").
         END.   
         IF LENGTH(fltid.ORSAK) > 20 THEN DO:
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(fltid.DATUM,"99/99/99")          
            SUBSTRING(tidut.UT,10) = STRING(fltid.DAG)            
            SUBSTRING(tidut.UT,58) = SUBSTRING(fltid.ORSAK,19,80).
         END.         

      END.
      ELSE DO:   
         CREATE tidut. 
         ASSIGN
         SUBSTRING(tidut.UT,1) = STRING(fltid.DATUM,"99/99/99")          
         SUBSTRING(tidut.UT,10) = SUBSTRING(fltid.INKNAPP,1,10)
         SUBSTRING(tidut.UT,21) = SUBSTRING(fltid.INAUTO,1,4)
         SUBSTRING(tidut.UT,26) = STRING(fltid.INTID,">9.99").   
         IF fltid.FLUTFM > 0 THEN ASSIGN SUBSTRING(tidut.UT,32) = STRING(fltid.FLUTFM,">9.99").
         IF fltid.FLINFM > 0 THEN ASSIGN SUBSTRING(tidut.UT,38) = STRING(fltid.FLINFM,">9.99").
         IF fltid.LUUT > 0 THEN ASSIGN SUBSTRING(tidut.UT,44) = STRING(fltid.LUUT,">9.99").
         IF fltid.LUIN > 0 THEN ASSIGN SUBSTRING(tidut.UT,50) = STRING(fltid.LUIN,">9.99").
         IF fltid.FLUTEM > 0 THEN ASSIGN SUBSTRING(tidut.UT,56) = STRING(fltid.FLUTEM,">9.99").
         IF fltid.FLINEM > 0 THEN ASSIGN SUBSTRING(tidut.UT,62) = STRING(fltid.FLINEM,">9.99").
         ASSIGN
         SUBSTRING(tidut.UT,68) = STRING(fltid.UTTID,">9.99")
         SUBSTRING(tidut.UT,74) = SUBSTRING(fltid.UTKNAPP,1,10)
         SUBSTRING(tidut.UT,85) = SUBSTRING(fltid.UTAUTO,1,4)
         SUBSTRING(tidut.UT,89) = STRING(fltid.PLUS,"->9.99")
         SUBSTRING(tidut.UT,96) = STRING(fltid.FELOK,"Ja/Nej")
         SUBSTRING(tidut.UT,100) = fltid.FELMED.  
         IF fltid.FLUTFM2 > 0  OR fltid.FLINFM2 > 0 OR fltid.FLUTEM2 > 0 
         OR fltid.FLINEM2 > 0 THEN DO:
            CREATE tidut. 
            CREATE tidut.         
            IF fltid.FLUTFM2 > 0 THEN ASSIGN SUBSTRING(tidut.UT,32) = STRING(fltid.FLUTFM2,">9.99").
            IF fltid.FLINFM2 > 0 THEN ASSIGN SUBSTRING(tidut.UT,38) = STRING(fltid.FLINFM2,">9.99").
            IF fltid.FLUTEM2 > 0 THEN ASSIGN SUBSTRING(tidut.UT,56) = STRING(fltid.FLUTEM2,">9.99").
            IF fltid.FLINEM2 > 0 THEN ASSIGN SUBSTRING(tidut.UT,62) = STRING(fltid.FLINEM2,">9.99").
         END.   
      END.
   END.            
   CREATE tidut. 
   ASSIGN 
   tidut.UT = str.
   CREATE tidut.  
   CREATE tidut.                 
   ASSIGN 
   SUBSTRING(tidut.UT,1) = "Periodens flexsaldo:"
   SUBSTRING(tidut.UT,46) = STRING(mtot,"->>9.99").  
   CREATE tidut.                 
   ASSIGN 
   SUBSTRING(tidut.UT,1) = "====================".
   CREATE tidut.  
   CREATE tidut.                 
   ASSIGN 
   SUBSTRING(tidut.UT,1) = "Totalt flexsaldo:"
   SUBSTRING(tidut.UT,46) = STRING(ftot,"->>9.99").   
   CREATE tidut.                 
   ASSIGN 
   SUBSTRING(tidut.UT,1) = "====================".
END PROCEDURE.


