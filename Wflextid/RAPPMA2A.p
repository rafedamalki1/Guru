/*RAPPMA2A.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
{TIDUTTTNEW.I} 
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
DEFINE INPUT PARAMETER arnr AS INTEGER FORMAT "9999" NO-UNDO.
DEFINE INPUT PARAMETER manad AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER manadnamn AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER manadnr AS INTEGER FORMAT "99" NO-UNDO. 
DEFINE OUTPUT PARAMETER TABLE FOR tidut.

DEFINE VARIABLE ingsaldo AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE ftot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE mtot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE isaldo AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE hjdat AS DATE NO-UNDO.
DEFINE VARIABLE splus AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(140)" NO-UNDO.

FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
persrec = RECID(PERSONALTAB).
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR. 
FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
FIND FIRST FLEXREG WHERE FLEXREG.KOD = FLEXAVT.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.     
RUN tolk_UI.
{GDPRLOGGCLIENT.I}
PROCEDURE tolk_UI :
   RUN skapaflex_UI.  
   RUN huvud_UI.  
   RUN skapaut_UI.                     
END PROCEDURE.

PROCEDURE huvud_UI :
     /*HUVUD*/
   CREATE tidut. 
   IF manad = 1 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,4) = "Flexrapport ej körda registreringar" 
      SUBSTRING(tidut.UT,60) = STRING(TODAY)
      SUBSTRING(tidut.UT,70) = STRING(TIME,"HH:MM:SS").
      CREATE tidut. 
      ASSIGN
      SUBSTRING(tidut.UT,4) = "======================================" .
      CREATE tidut.
   END.
   ELSE DO:
      ASSIGN
      SUBSTRING(tidut.UT,4) = "Flexrapport för År:"
      SUBSTRING(tidut.UT,25) = STRING(arnr)
      SUBSTRING(tidut.UT,30) = "Månad:" 
      SUBSTRING(tidut.UT,40) = STRING(manadnr)  
      SUBSTRING(tidut.UT,60) = STRING(TODAY)
      SUBSTRING(tidut.UT,70) = STRING(TIME,"HH:MM:SS").
      CREATE tidut. 
      ASSIGN
      SUBSTRING(tidut.UT,4) = "===============================================" .
      CREATE tidut.
   END.   
   CREATE tidut.  
   ASSIGN
   SUBSTRING(tidut.UT,4) = "Namn:"
   SUBSTRING(tidut.UT,10) = PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN   
   SUBSTRING(tidut.UT,60) = STRING(PERSONALTAB.PERSONNUMMER,"999999-9999") 
   SUBSTRING(tidut.UT,72) = "Sign:"
   SUBSTRING(tidut.UT,78) = PERSONALTAB.PERSONALKOD.  
   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   
   CREATE tidut.  
   CREATE tidut.                 
   ASSIGN 
   SUBSTRING(tidut.UT,1) = "Ingående flexsaldo:"
   SUBSTRING(tidut.UT,20) = STRING(ingsaldo,"->>9.99").   
   CREATE tidut.                 
   ASSIGN 
   SUBSTRING(tidut.UT,1) = "====================".
   CREATE tidut.
   IF Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" THEN DO:  
      CREATE tidut.                                  
      ASSIGN
      SUBSTRING(tidut.UT,2) = "Datum"
      SUBSTRING(tidut.UT,10) = "Dag"
      SUBSTRING(tidut.UT,14) = "In-knapp"
      SUBSTRING(tidut.UT,25) = "In"             
      SUBSTRING(tidut.UT,31) = "Ut"
      SUBSTRING(tidut.UT,37) = "Ut-knapp"     
      SUBSTRING(tidut.UT,48) = "Plus"
      SUBSTRING(tidut.UT,54) = "Ok"
      SUBSTRING(tidut.UT,58) = "Orsak"
      SUBSTRING(tidut.UT,79) = "Felmeddelande"
      SUBSTRING(tidut.UT,99) = "Flex"
      SUBSTRING(tidut.UT,105) = "Flex"
      SUBSTRING(tidut.UT,111) = "Flex"   
      SUBSTRING(tidut.UT,117) = "Flex"   .  
      CREATE tidut.                                  
      ASSIGN
      SUBSTRING(tidut.UT,99) = " Ut"
      SUBSTRING(tidut.UT,105) = " In"
      SUBSTRING(tidut.UT,111) = " Ut"   
      SUBSTRING(tidut.UT,117) = " In".     
      str =                                                                                                                            
      "========.===.==========.=====.=====.==========.=====.===.====================.====================.=====.=====.=====.=====".             
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
   OPEN QUERY flgam FOR EACH FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   YEAR(FLEXTID.DATUM)  = arnr AND MONTH(FLEXTID.DATUM)  = manadnr USE-INDEX KORD NO-LOCK.   
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
      ftid.AUTO = FLEXTID.AUTO
      ftid.ORSAK = FLEXTID.ORSAK.
      GET NEXT flgam NO-LOCK.
   END.              
   CLOSE QUERY flgam.   
   OPEN QUERY fdgam FOR EACH FLEXDAG WHERE FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   YEAR(FLEXTID.DATUM)  = arnr AND MONTH(FLEXTID.DATUM)  = manadnr
   AND FLEXDAG.FELMED = "Ingen registrering gjord" USE-INDEX FLEX NO-LOCK.   
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
   GET FIRST frapp EXCLUSIVE-LOCK.
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
         IF ftid.ORSAK ne "" THEN ASSIGN fltid.ORSAK = ftid.ORSAK. 
      END.
      ELSE IF ftid.KNAPP = "LUNCH UT" THEN DO:
         ASSIGN fltid.LUUT = ftid.TID.   
      END.
      ELSE IF ftid.KNAPP = "LUNCH IN" THEN DO:
         ASSIGN fltid.LUIN = ftid.TID.   
      END.
      ELSE IF ftid.KNAPP = "FLEX UT" THEN DO:
         IF ftid.TID < lunchstarten THEN DO:
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
         IF ftid.TID LE lunchstarten THEN DO:
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
   FIND FIRST FSALDMAN WHERE FSALDMAN.PERSONALKOD = PERSONALTAB.PERSONALKOD
   AND YEAR(FSALDMAN.DATUM) = arnr AND MONTH(FSALDMAN.DATUM) = manadnr NO-LOCK NO-ERROR.     
   IF AVAILABLE FSALDMAN THEN DO:
      ingsaldo = FSALDMAN.ACCFLEX.
      nytid = FSALDMAN.ACCFLEX.
      RUN TIMSEK.P.
      ASSIGN
      isaldo = sekunder
      mtot = FSALDMAN.PERIODFLEX.
      nytid = FSALDMAN.PERIODFLEX.
      RUN TIMSEK.P.
      isaldo = isaldo + sekunder.
      sekunder = isaldo.
      RUN FSEKTIM.P.
      ftot = fnytid.
   END.   
END PROCEDURE.
               
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapaut_UI WINDOW-2 
PROCEDURE skapaut_UI :
   FOR EACH fltid USE-INDEX DATUM: 
      IF Guru.Konstanter:globforetag = "SUFL" OR  Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
         CREATE tidut. 
         ASSIGN
         SUBSTRING(tidut.UT,1) = STRING(fltid.DATUM)          
         SUBSTRING(tidut.UT,10) = STRING(fltid.DAG)
         SUBSTRING(tidut.UT,14) = SUBSTRING(fltid.INKNAPP,1,10)        
         SUBSTRING(tidut.UT,25) = STRING(fltid.INTID,">9.99")
         SUBSTRING(tidut.UT,30) = "-".   
         ASSIGN
         SUBSTRING(tidut.UT,31) = STRING(fltid.UTTID,">9.99")
         SUBSTRING(tidut.UT,37) = SUBSTRING(fltid.UTKNAPP,1,10)         
         SUBSTRING(tidut.UT,47) = STRING(fltid.PLUS,"->9.99")
         SUBSTRING(tidut.UT,54) = STRING(fltid.FELOK,"Ja/Nej")
         SUBSTRING(tidut.UT,58) = fltid.ORSAK
         SUBSTRING(tidut.UT,79) = fltid.FELMED.           
         IF fltid.FLUTFM > 0 THEN ASSIGN SUBSTRING(tidut.UT,99) = STRING(fltid.FLUTFM,">9.99").
         IF fltid.FLINFM > 0 THEN ASSIGN SUBSTRING(tidut.UT,105) = STRING(fltid.FLINFM,">9.99").
         IF fltid.FLUTEM > 0 THEN ASSIGN SUBSTRING(tidut.UT,111) = STRING(fltid.FLUTEM,">9.99").
         IF fltid.FLINEM > 0 THEN ASSIGN SUBSTRING(tidut.UT,117) = STRING(fltid.FLINEM,">9.99").
         IF fltid.FLUTFM2 > 0  OR fltid.FLINFM2 > 0 OR fltid.FLUTEM2 > 0 
         OR fltid.FLINEM2 > 0 THEN DO:
            CREATE tidut. 
            CREATE tidut.         
            IF fltid.FLUTFM2 > 0 THEN ASSIGN SUBSTRING(tidut.UT,99) = STRING(fltid.FLUTFM2,">9.99").
            IF fltid.FLINFM2 > 0 THEN ASSIGN SUBSTRING(tidut.UT,105) = STRING(fltid.FLINFM2,">9.99").
            IF fltid.FLUTEM2 > 0 THEN ASSIGN SUBSTRING(tidut.UT,111) = STRING(fltid.FLUTEM2,">9.99").
            IF fltid.FLINEM2 > 0 THEN ASSIGN SUBSTRING(tidut.UT,117) = STRING(fltid.FLINEM2,">9.99").
         END.   
      END.
      ELSE DO:   
         CREATE tidut. 
         ASSIGN
         SUBSTRING(tidut.UT,1) = STRING(fltid.DATUM)          
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
   SUBSTRING(tidut.UT,20) = STRING(ftot,"->>9.99").   
   CREATE tidut.                 
   ASSIGN 
   SUBSTRING(tidut.UT,1) = "====================".
END PROCEDURE.

