/*FLSALDOAP.P*/
&Scoped-define NEW NEW
{TIDPERS.I}
{TIDUTTTNEW.I}
{REGVAR.I}
{GLOBVAR2DEL1.I}
DEFINE VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE VARIABLE persrec AS RECID NO-UNDO.
DEFINE VARIABLE persrec2 AS RECID NO-UNDO.


DEFINE TEMP-TABLE namntemp
   FIELD EFTERNAMN AS CHARACTER
   FIELD FORNAMN AS CHARACTER  
   FIELD NAMN AS CHARACTER  
   FIELD NTEXT AS CHARACTER
   FIELD REGIS AS CHARACTER  
   FIELD DATUM AS DATE
   FIELD ALLVAL AS INTEGER
   FIELD MANADVAR AS INTEGER.     

DEFINE TEMP-TABLE invartemp   
   FIELD GA LIKE ANVANDARE.ANVANDARE 
   FIELD GM AS LOGICAL 
   FIELD SK AS LOGICAL 
   FIELD TI AS RECID 
   FIELD PER AS RECID 
   FIELD PER2 AS RECID 
   FIELD MU AS LOGICAL    
   FIELD REGST LIKE TIDREGITAB.START 
   FIELD REGSU LIKE TIDREGITAB.SLUT 
   FIELD RV AS INTEGER FORMAT "999" 
   FIELD RDAG AS CHARACTER FORMAT "X(3)"         
   FIELD RD AS DATE 
   FIELD RM AS INTEGER FORMAT "99" 
   FIELD RMN AS CHARACTER  
   FIELD REGA AS INTEGER FORMAT "99" 
   FIELD RT LIKE TIDREGITAB.TOTALT       
   FIELD BD AS DATE 
   FIELD AD AS DATE 
   FIELD NY AS DECIMAL 
   FIELD SEK AS INTEGER FORMAT "-9999999" 
   FIELD RSEK AS INTEGER 
   FIELD REGS AS INTEGER 
   FIELD GL LIKE FORETAG.FORETAG. 


DEFINE TEMP-TABLE ksal
   FIELD PERSONALKOD LIKE PERSONALTAB.PERSONALKOD  
   FIELD FORNAMN LIKE PERSONALTAB.FORNAMN 
   FIELD EFTERNAMN LIKE PERSONALTAB.EFTERNAMN 
   FIELD EJKORDKOMP AS DECIMAL  
   FIELD TOTALKOMP AS DECIMAL      
   FIELD GRANS AS CHARACTER FORMAT "X(1)"
   FIELD PLMIN AS DECIMAL
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING.

DEFINE TEMP-TABLE overtidhj2
   FIELD DATUM LIKE TIDREGITAB.DATUM
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD
   FIELD OVERANTAL AS DECIMAL
   FIELD OVERTIDTILL AS CHARACTER
   FIELD MANAD AS INTEGER
   FIELD MULTIP AS DECIMAL
   FIELD ANTMULT AS DECIMAL
   FIELD KOMPUTTAG AS DECIMAL    
   FIELD NAMN AS CHARACTER FORMAT "X(30)"
   FIELD OMRADE AS CHARACTER
   FIELD GEOMRADE AS CHARACTER
   FIELD OVERTIDUTTAG AS CHARACTER
   FIELD VERKOV AS DECIMAL
   FIELD VERKKO AS DECIMAL
   FIELD VECKOKORD AS CHARACTER 
   INDEX PKOD PERSONALKOD DATUM.

DEFINE TEMP-TABLE oversum
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD
   FIELD NAMN AS CHARACTER FORMAT "X(30)"
   FIELD MANAD AS INTEGER
   FIELD OVERANTAL AS DECIMAL
   FIELD ANTMULT AS DECIMAL   
   FIELD KOMPUTTAG AS DECIMAL       
   FIELD OMRADE AS CHARACTER
   FIELD VERKOV AS DECIMAL
   FIELD VERKKO AS DECIMAL.
DEFINE TEMP-TABLE overksum LIKE  oversum.   
DEFINE INPUT PARAMETER TABLE FOR invartemp.   
DEFINE INPUT PARAMETER RAD_ALLVAL AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tidpers.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR namntemp.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
DEFINE VARIABLE ingsaldo AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE ftot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE mtot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE isaldo AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE hjdat AS DATE NO-UNDO.
DEFINE VARIABLE splus AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(140)" NO-UNDO.
DEFINE VARIABLE plgrans AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE kompaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.
FUNCTION klock60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) * 60 / 100 ).

END FUNCTION.

FIND FIRST FORETAG NO-LOCK NO-ERROR.
FIND FIRST invartemp NO-ERROR.
ASSIGN
gvisatidpermanad = invartemp.GM 
skrivut = invartemp.SK   
tidtabrec = invartemp.TI   
persrec = invartemp.PER   
persrec2 = invartemp.PER2  
musz = invartemp.MU      
regstart = invartemp.REGST   
regslut = invartemp.REGSU  
regvnr = invartemp.RV   
regdagnamn = invartemp.RDAG          
regdatum = invartemp.RD   
regmnr = invartemp.RM  
regmannamn = invartemp.RMN  
regar = invartemp.REGA  
regtotalt = invartemp.RT        
bdatum = invartemp.BD  
avdatum = invartemp.AD  
nytid = invartemp.NY 
sekunder = invartemp.SEK 
regstartsek = invartemp.RSEK  
regslutsek = invartemp.REGS 
Guru.Konstanter:globforetag = invartemp.GL.
FIND FIRST namntemp NO-ERROR.
RUN tolk_UI.
{GDPRLOGGCLIENT.I}
PROCEDURE tolk_UI :
   RUN ejkordkomp_UI.
   RUN skapaflex_UI.     
   RUN huvud_UI.  
   RUN skapaut_UI.                     
END PROCEDURE.

PROCEDURE huvud_UI :
   FIND FIRST tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.      
   IF RAD_ALLVAL = 2 THEN DO:
      namntemp.REGIS = Guru.Konstanter:gomrk + ":".
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = tidpers.OMRADE USE-INDEX OMR NO-LOCK NO-ERROR.
      ASSIGN
      namntemp.NAMN = OMRADETAB.NAMN.
   END.
   IF RAD_ALLVAL = 3 THEN DO:
      namntemp.REGIS = "Alla".
   END.
    IF RAD_ALLVAL = 4 THEN DO:
      namntemp.REGIS = "".
   END.
   IF RAD_ALLVAL = 5 THEN namntemp.REGIS = "Markerade enheter".
   
   CREATE tidut. 
   ASSIGN
   SUBSTRING(tidut.UT,4) = "Kompsaldo till och med "  + string(avdatum).
   ASSIGN
   SUBSTRING(tidut.UT,65) = STRING(TODAY)
   SUBSTRING(tidut.UT,75) = STRING(TIME,"HH:MM:SS").
   CREATE tidut. 
   ASSIGN
   SUBSTRING(tidut.UT,4) = "=================================" .  
   CREATE tidut.    
   ASSIGN
   SUBSTRING(tidut.UT,4) = namntemp.REGIS.
   IF RAD_ALLVAL = 1 THEN ASSIGN SUBSTRING(tidut.UT,25) = namntemp.FORNAMN.
   IF RAD_ALLVAL = 2 THEN ASSIGN SUBSTRING(tidut.UT,25) = namntemp.NAMN.
   CREATE tidut.       
   ASSIGN
   SUBSTRING(tidut.UT,1) = "Enhet/"
   SUBSTRING(tidut.UT,7) = "Förnamn"
   SUBSTRING(tidut.UT,17) = "Efternamn"
   SUBSTRING(tidut.UT,33) = "Totalt"
   SUBSTRING(tidut.UT,44) = "Varav kompsaldo"   
   SUBSTRING(tidut.UT,67) = "+ Överstiger gränsvärde 80 tim".  
   CREATE tidut.    
   ASSIGN  
   SUBSTRING(tidut.UT,1) = "Sign"
   SUBSTRING(tidut.UT,33) = "kompsaldo"
   SUBSTRING(tidut.UT,44) = "inarbetad efter sista".
   CREATE tidut.    
   ASSIGN
   SUBSTRING(tidut.UT,44) = "månadskörningen"        
   str =                                                                                                                            
   "=====.=========.===============.==========.=======================.===============".             
   CREATE tidut.                  
   ASSIGN
   SUBSTRING(tidut.UT,1) = str.   
END PROCEDURE.

PROCEDURE skapaut_UI :  
   FOR EACH ksal USE-INDEX PERSONALKOD:       
      CREATE tidut. 
      ASSIGN
      SUBSTRING(tidut.UT,1) = ksal.PERSONALKOD
      SUBSTRING(tidut.UT,7) = ksal.FORNAMN
      SUBSTRING(tidut.UT,17) = ksal.EFTERNAMN
      SUBSTRING(tidut.UT,33) = STRING(klock60(ksal.TOTALKOMP),"->>9.99")          
      SUBSTRING(tidut.UT,43) = STRING(klock60(ksal.EJKORDKOMP),"->>9.99").            
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + ksal.PERSONALKOD.
      IF ksal.PLMIN <> 0 THEN DO:
         SUBSTRING(tidut.UT,67) = STRING(klock60(ksal.PLMIN),"->>9.99").   
      END.   
   END.            
   CREATE tidut. 
   ASSIGN 
   tidut.UT = str.  
END PROCEDURE.

PROCEDURE skapaflex_UI :
   DEBUGGER:SET-BREAK().   
   OPEN QUERY flgam FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
   EACH KOMPSALDO WHERE KOMPSALDO.PERSONALKOD = tidpers.PERSONALKOD USE-INDEX PKOD NO-LOCK.   
   GET FIRST flgam NO-LOCK.
   DO WHILE AVAILABLE(KOMPSALDO):    
      FIND FIRST overksum  WHERE overksum.PERSONALKOD = tidpers.PERSONALKOD NO-LOCK NO-ERROR.          
      CREATE ksal.
      ASSIGN
      ksal.PERSONALKOD = tidpers.PERSONALKOD
      ksal .FORNAMN = tidpers.FORNAMN
      ksal.EFTERNAMN = tidpers.EFTERNAMN.
      IF AVAILABLE overksum THEN DO:         
         ksal.TOTALKOMP = klock100(KOMPSALDO.ACCKOMP) + klock100(KOMPSALDO.PERIODKOMP) + overksum.ANTMULT - overksum.KOMPUTTAG.              
         ksal.EJKORDKOMP = overksum.ANTMULT - overksum.KOMPUTTAG.
      END.
      ELSE DO:
         ksal.TOTALKOMP = klock100(KOMPSALDO.ACCKOMP) + klock100(KOMPSALDO.PERIODKOMP) .              
         ksal.EJKORDKOMP = 0.
      END.            
      plgrans = 80.      
      IF ksal.TOTALKOMP > plgrans THEN DO:         
         ksal.PLMIN = ksal.TOTALKOMP - plgrans.         
      END.   
      
      GET NEXT flgam NO-LOCK.
   END.              
   CLOSE QUERY flgam.    
END PROCEDURE.

PROCEDURE ejkordkomp_UI :
   EMPTY TEMP-TABLE overtidhj2 NO-ERROR.
   EMPTY TEMP-TABLE overksum NO-ERROR. 
   IF FORETAG.FORETAG = "GKAL" THEN kompaonr = "171".
   ELSE kompaonr = "170".     
   OPEN QUERY pq FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK.   
   GET FIRST pq NO-LOCK.
   DO WHILE AVAILABLE(tidpers):
      FIND PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidpers.PERSONALKOD  NO-LOCK NO-ERROR. 
      FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.
      OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND TIDREGITAB.VECKOKORD = ""  AND
      TIDREGITAB.OKOD1 NE "" AND TIDREGITAB.OVERTIDUTTAG = "K"  USE-INDEX PSTART NO-LOCK.   
      GET FIRST tq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):               
         IF TIDREGITAB.OVERTIDUTTAG = "K"  THEN DO:                 
            IF TIDREGITAB.OANT1 > 0  THEN DO:      
               CREATE overtidhj2.                          
               ASSIGN
               overtidhj2.PERSONALKOD = TIDREGITAB.PERSONALKOD                                               
               overtidhj2.DATUM =  TIDREGITAB.DATUM
               overtidhj2.OVERANTAL = klock100(TIDREGITAB.OANT1) 
               overtidhj2.OVERTIDTILL = TIDREGITAB.OKOD1
               overtidhj2.MANAD = MONTH(TIDREGITAB.DATUM)
               overtidhj2.MULTIP = 1
               overtidhj2.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG.
               FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD1 AND OVERKOD.KOD = ANSTFORMTAB.KOD USE-INDEX OVER NO-LOCK NO-ERROR.
               IF AVAILABLE  OVERKOD THEN DO:                                               
                  IF FORETAG.FORETA = "gkal" THEN overtidhj2.MULTIP = OVERKOD.MULTIP.
                  ELSE overtidhj2.MULTIP =  OVERKOD.ERSATTNING.
               END.
               IF TIDREGITAB.OVERTIDUTTAG = "K" THEN DO: 
                  overtidhj2.ANTMULT = (1 + overtidhj2.MULTIP) * overtidhj2.OVERANTAL.
                  /*ändrad till verklig tid inte inte avrundad tid 20171122 Lena */               
                  overtidhj2.VERKKO = klock100(TIDREGITAB.TOTALT).
               END.               
            END.      
            IF TIDREGITAB.OANT2 > 0  THEN DO:      
               CREATE overtidhj2.                          
               ASSIGN
               overtidhj2.PERSONALKOD = TIDREGITAB.PERSONALKOD                                          
               overtidhj2.DATUM =  TIDREGITAB.DATUM            
               overtidhj2.OVERANTAL = klock100(TIDREGITAB.OANT2) 
               overtidhj2.OVERTIDTILL = TIDREGITAB.OKOD2
               overtidhj2.MANAD = MONTH(TIDREGITAB.DATUM)
               overtidhj2.MULTIP = 1
               overtidhj2.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG.
               FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD2 AND OVERKOD.KOD = ANSTFORMTAB.KOD USE-INDEX OVER NO-LOCK NO-ERROR.
               IF AVAILABLE  OVERKOD THEN DO:                                               
                  IF FORETAG.FORETA = "gkal" THEN overtidhj2.MULTIP = OVERKOD.MULTIP.
                  ELSE overtidhj2.MULTIP =  OVERKOD.ERSATTNING.
               END.               
               IF TIDREGITAB.OVERTIDUTTAG = "K" THEN DO: 
                  overtidhj2.ANTMULT = (1 + overtidhj2.MULTIP) * overtidhj2.OVERANTAL.
                  /*ändrad till verklig tid inte inte avrundad tid 20171122 Lena 
                  oant2 skall bara räknas om oant1 = 0*/
                  IF TIDREGITAB.OANT1 = 0  THEN   overtidhj2.VERKKO = klock100(TIDREGITAB.TOTALT).                
               END.                  
            END.      
            IF TIDREGITAB.OANT3 > 0  THEN DO:      
               CREATE overtidhj2.                          
               ASSIGN
               overtidhj2.PERSONALKOD = TIDREGITAB.PERSONALKOD                                          
               overtidhj2.DATUM =  TIDREGITAB.DATUM            
               overtidhj2.OVERANTAL = klock100(TIDREGITAB.OANT3) 
               overtidhj2.OVERTIDTILL = TIDREGITAB.OKOD3
               overtidhj2.MANAD = MONTH(TIDREGITAB.DATUM)
               overtidhj2.MULTIP = 1
               overtidhj2.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG.
               FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD3 AND OVERKOD.KOD = ANSTFORMTAB.KOD USE-INDEX OVER NO-LOCK NO-ERROR.
               IF AVAILABLE  OVERKOD THEN DO:                                               
                  IF FORETAG.FORETA = "gkal" THEN overtidhj2.MULTIP = OVERKOD.MULTIP.
                  ELSE overtidhj2.MULTIP =  OVERKOD.ERSATTNING.
               END.
               IF TIDREGITAB.OVERTIDUTTAG = "K" THEN DO:                   
                  overtidhj2.ANTMULT = (1 + overtidhj2.MULTIP) * overtidhj2.OVERANTAL.
                  /*ändrad till verklig tid inte inte avrundad tid 20171122 Lena 
                  oant3 skall bara räknas om oant1 = 0 och oant2= 0*/
                  IF TIDREGITAB.OANT1 = 0 AND  TIDREGITAB.OANT2 = 0  THEN   overtidhj2.VERKKO = klock100(TIDREGITAB.TOTALT).               
               END.                                 
            END.
         END.   
         GET NEXT tq NO-LOCK.           
      END.
      OPEN QUERY tuq FOR EACH TIDREGITAB WHERE  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD  AND TIDREGITAB.VECKOKORD = "" AND TIDREGITAB.AONR = kompaonr  USE-INDEX PSTART NO-LOCK.   
      GET FIRST tuq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):         
         CREATE overtidhj2.                          
         ASSIGN
         overtidhj2.PERSONALKOD = TIDREGITAB.PERSONALKOD                    
         overtidhj2.DATUM =  TIDREGITAB.DATUM.                     
         overtidhj2.MANAD = MONTH(TIDREGITAB.DATUM).
         overtidhj2.KOMPUTTAG = klock100(TIDREGITAB.TOTALT).         
         overtidhj2.MULTIP = 1.
         GET NEXT tuq NO-LOCK.      
      END.                       
      IF Guru.Konstanter:globforetag = "snat" THEN DO:
         /*berdskapstolkning när veckvila inträffar under klämdag genererar lart 260 = Fyllnadslön mot ledighet vid beredskap Lena 20190611*/         
         OPEN QUERY tblq FOR EACH TIDREGITAB WHERE
         TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD  AND TIDREGITAB.VECKOKORD = "" AND
         TIDREGITAB.LONTILLAGG = "260"  USE-INDEX PSTART NO-LOCK.   
         GET FIRST tblq NO-LOCK.
         DO WHILE AVAILABLE(TIDREGITAB):                           
            CREATE overtidhj2.                          
            ASSIGN
            overtidhj2.PERSONALKOD = TIDREGITAB.PERSONALKOD                                               
            overtidhj2.DATUM =  TIDREGITAB.DATUM
            overtidhj2.OVERANTAL = klock100(TIDREGITAB.LONTILLANTAL) 
            overtidhj2.OVERTIDTILL = TIDREGITAB.LONTILLAGG
            overtidhj2.MANAD = MONTH(TIDREGITAB.DATUM)
            overtidhj2.MULTIP = 0         
            overtidhj2.OVERTIDUTTAG = "K"          
            overtidhj2.ANTMULT =  overtidhj2.OVERANTAL
            /*ändrad till verklig tid inte inte avrundad tid 20171122 Lena */               
            overtidhj2.VERKKO = 0
            overtidhj2.VECKOKORD = TIDREGITAB.VECKOKORD.
            GET NEXT tblq NO-LOCK.      
         END.
         /*berdskapstolkning klämdagar halvdagar genererar lart 260 = Fyllnadslön mot ledighet vid beredksp Lena 20190611*/      
         OPEN QUERY tbq FOR EACH TIDREGITAB WHERE
         TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD  AND TIDREGITAB.VECKOKORD = "" AND
         TIDREGITAB.BEREDSKAP = "260"  USE-INDEX PSTART NO-LOCK.   
         GET FIRST tbq NO-LOCK.
         DO WHILE AVAILABLE(TIDREGITAB):
            CREATE overtidhj2.                          
            ASSIGN
            overtidhj2.PERSONALKOD = TIDREGITAB.PERSONALKOD                                               
            overtidhj2.DATUM =  TIDREGITAB.DATUM
            overtidhj2.OVERANTAL = klock100(TIDREGITAB.BERANTAL) 
            overtidhj2.OVERTIDTILL = TIDREGITAB.BEREDSKAP
            overtidhj2.MANAD = MONTH(TIDREGITAB.DATUM)
            overtidhj2.MULTIP = 0         
            overtidhj2.OVERTIDUTTAG = "K"          
            overtidhj2.ANTMULT =  overtidhj2.OVERANTAL
            /*ändrad till verklig tid inte inte avrundad tid 20171122 Lena */               
            overtidhj2.VERKKO = 0
            overtidhj2.VECKOKORD = TIDREGITAB.VECKOKORD.
            GET NEXT tbq NO-LOCK.      
         END.
      END.
      GET NEXT pq NO-LOCK.  
   END.   
   /*ej körd kompsaldo*/
   EMPTY TEMP-TABLE overksum NO-ERROR.    
   FOR EACH overtidhj2 WHERE overtidhj2.VECKOKORD = "" AND overtidhj2.DATUM LE avdatum BREAK BY overtidhj2.PERSONALKOD:    
      ACCUMULATE overtidhj2.OVERANTAL (TOTAL  BY overtidhj2.PERSONALKOD).        
      ACCUMULATE overtidhj2.ANTMULT (TOTAL  BY overtidhj2.PERSONALKOD).
      ACCUMULATE overtidhj2.KOMPUTTAG (TOTAL  BY overtidhj2.PERSONALKOD).
      ACCUMULATE overtidhj2.VERKKO (TOTAL  BY overtidhj2.PERSONALKOD).
      ACCUMULATE overtidhj2.VERKOV (TOTAL  BY overtidhj2.PERSONALKOD).              
      IF LAST-OF(overtidhj2.PERSONALKOD) THEN DO:      
         CREATE overksum.
         ASSIGN         
         overksum.PERSONALKOD = overtidhj2.PERSONALKOD                  
         overksum.NAMN = overtidhj2.NAMN                         
         overksum.OVERANTAL = (ACCUM TOTAL BY overtidhj2.PERSONALKOD overtidhj2.OVERANTAL).                  
         overksum.ANTMULT = (ACCUM TOTAL BY overtidhj2.PERSONALKOD overtidhj2.ANTMULT).
         overksum.KOMPUTTAG = (ACCUM TOTAL BY overtidhj2.PERSONALKOD overtidhj2.KOMPUTTAG).
         overksum.VERKKO = (ACCUM TOTAL BY overtidhj2.PERSONALKOD overtidhj2.VERKKO).
         overksum.VERKOV = (ACCUM TOTAL BY overtidhj2.PERSONALKOD overtidhj2.VERKOV).                                             
      END.
   END.         
END PROCEDURE.