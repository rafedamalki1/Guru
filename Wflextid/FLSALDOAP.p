/*FLSALDOAP.P*/
&Scoped-define NEW NEW
{TIDPERS.I}
{TIDUTTTNEW.I}
DEFINE TEMP-TABLE namntemp
   FIELD EFTERNAMN AS CHARACTER
   FIELD FORNAMN AS CHARACTER  
   FIELD NAMN AS CHARACTER  
   FIELD NTEXT AS CHARACTER
   FIELD REGIS AS CHARACTER  
   FIELD DATUM AS DATE
   FIELD ALLVAL AS INTEGER
   FIELD MANADVAR AS INTEGER.     

DEFINE TEMP-TABLE fsal
   FIELD PERSONALKOD LIKE PERSONALTAB.PERSONALKOD  
   FIELD FORNAMN LIKE PERSONALTAB.FORNAMN 
   FIELD EFTERNAMN LIKE PERSONALTAB.EFTERNAMN 
   FIELD EJKORDFLEX LIKE FLEXSALDO.EJKORDFLEX  
   FIELD PERIODFLEX LIKE FLEXSALDO.PERIODFLEX
   FIELD BACFLEX LIKE FLEXSALDO.BACFLEX
   FIELD ACCFLEX LIKE FLEXSALDO.ACCFLEX  
   FIELD TOTALT AS DECIMAL
   FIELD GRANS AS CHARACTER FORMAT "X(1)"
   FIELD PLMIN AS DECIMAL
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING.

DEFINE INPUT PARAMETER RAD_ALLVAL AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER manad AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tidpers.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR namntemp.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.

DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE ingsaldo AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE ftot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE mtot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE isaldo AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE hjdat AS DATE NO-UNDO.
DEFINE VARIABLE splus AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(140)" NO-UNDO.
DEFINE VARIABLE plgrans AS DECIMAL FORMAT "99.99" NO-UNDO.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
FIND FIRST namntemp NO-ERROR.
RUN tolk_UI.
{GDPRLOGGCLIENT.I}
PROCEDURE tolk_UI :
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
   IF manad = 1 THEN DO:
      CREATE tidut. 
      ASSIGN
      SUBSTRING(tidut.UT,4) = "Saldon till och med igår: " + STRING(TODAY - 1).
      ASSIGN
      SUBSTRING(tidut.UT,65) = STRING(TODAY)
      SUBSTRING(tidut.UT,75) = STRING(TIME,"HH:MM:SS").
      CREATE tidut. 
      ASSIGN
      SUBSTRING(tidut.UT,4) = "===================================" .
   END.  
   IF manad = 0 THEN DO:
      CREATE tidut. 
      ASSIGN         
      SUBSTRING(tidut.UT,4) = "Saldon till och med föregående månadsskifte: " + STRING(DATE(MONTH(TODAY),01,YEAR(TODAY))- 1).
      ASSIGN
      SUBSTRING(tidut.UT,65) = STRING(TODAY)
      SUBSTRING(tidut.UT,75) = STRING(TIME,"HH:MM:SS").
      CREATE tidut. 
      ASSIGN
      SUBSTRING(tidut.UT,4) = "=======================================================" .
   END.  
   
   CREATE tidut.    
   ASSIGN
   SUBSTRING(tidut.UT,4) = namntemp.REGIS.
   IF RAD_ALLVAL = 1 THEN ASSIGN SUBSTRING(tidut.UT,25) = namntemp.FORNAMN.
   IF RAD_ALLVAL = 2 THEN ASSIGN SUBSTRING(tidut.UT,25) = namntemp.NAMN.
   CREATE tidut.
   FIND FIRST FLEXREG NO-LOCK NO-ERROR.
   IF AVAILABLE FLEXREG THEN DO:
      IF FORETAG.FORETAG = "gkal" OR FORETAG.FORETAG = "lule" THEN musz = musz.
      ELSE DO:      
         CREATE tidut.    
         ASSIGN
         SUBSTRING(tidut.UT,4) = "Senaste körning tom:"
         SUBSTRING(tidut.UT,26) = STRING(FLEXREG.SALDOKORD).
      END.
   END.
   CREATE tidut.
   CREATE tidut.    
   ASSIGN
   SUBSTRING(tidut.UT,1) = "Enhet/"
   SUBSTRING(tidut.UT,7) = "Förnamn"
   SUBSTRING(tidut.UT,17) = "Efternamn"
   SUBSTRING(tidut.UT,33) = "Saldo"
   SUBSTRING(tidut.UT,43) = "Saldo ej"
   SUBSTRING(tidut.UT,53) = "Totalt"
   SUBSTRING(tidut.UT,63) = "+ Överstiger gränsvärde".  
   CREATE tidut.    
   ASSIGN  
   SUBSTRING(tidut.UT,1) = "Sign"
   SUBSTRING(tidut.UT,33) = "körd flex"
   SUBSTRING(tidut.UT,43) = "körd flex"
   SUBSTRING(tidut.UT,53) = "flexsaldo"
   SUBSTRING(tidut.UT,63) = "- Understiger gränsvärde".     
   str =                                                                                                                            
   "=====.=========.===============.=========.=========.=========.===============".             
   CREATE tidut.                  
   ASSIGN
   SUBSTRING(tidut.UT,1) = str.   
END PROCEDURE.

PROCEDURE skapaut_UI :
   FOR EACH fsal USE-INDEX PERSONALKOD: 
      CREATE tidut. 
      ASSIGN
      SUBSTRING(tidut.UT,1) = fsal.PERSONALKOD
      SUBSTRING(tidut.UT,7) = fsal.FORNAMN
      SUBSTRING(tidut.UT,17) = fsal.EFTERNAMN
      SUBSTRING(tidut.UT,33) = STRING(fsal.ACCFLEX,"->>9.99")          
      SUBSTRING(tidut.UT,43) = STRING(fsal.EJKORDFLEX,"->>9.99")
      SUBSTRING(tidut.UT,53) = STRING(fsal.TOTALT,"->>9.99")
      SUBSTRING(tidut.UT,63) = fsal.GRANS.
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + fsal.PERSONALKOD.
      IF fsal.PLMIN <> 0 THEN DO:
         SUBSTRING(tidut.UT,64) = STRING(fsal.PLMIN,"->>9.99").   
      END.   
   END.            
   CREATE tidut. 
   ASSIGN 
   tidut.UT = str.  
END PROCEDURE.

PROCEDURE skapaflex_UI :
   OPEN QUERY flgam FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
   EACH FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = tidpers.PERSONALKOD USE-INDEX PKOD NO-LOCK.   
   GET FIRST flgam NO-LOCK.
   DO WHILE AVAILABLE(FLEXSALDO):    
      FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD =  tidpers.PERSONALKOD AND FLEXAVT.FLEXTID = TRUE NO-LOCK NO-ERROR.
      IF AVAILABLE FLEXAVT THEN DO:    
         CREATE fsal.
         ASSIGN
         fsal.PERSONALKOD = tidpers.PERSONALKOD
         fsal .FORNAMN = tidpers.FORNAMN
         fsal.EFTERNAMN = tidpers.EFTERNAMN
         fsal.EJKORDFLEX = FLEXSALDO.EJKORDFLEX
         fsal.PERIODFLEX = FLEXSALDO.PERIODFLEX
         fsal.BACFLEX = FLEXSALDO.BACFLEX      
         fsal.ACCFLEX = FLEXSALDO.ACCFLEX.      
         IF manad = 0 THEN ASSIGN fsal.EJKORDFLEX = FLEXSALDO.EJKFLSISTA.
         splus = 0.
         nytid = fsal.ACCFLEX.
         RUN TIMSEK.P.      
         ASSIGN
         splus = splus + sekunder
         nytid = fsal.PERIODFLEX.
         RUN TIMSEK.P.      
         ASSIGN
         splus = splus + sekunder
         sekunder = splus.
         RUN FSEKTIM.P.
         fsal.ACCFLEX = fnytid.  /*   accflex + periodflex*/
         ASSIGN 
         nytid = fsal.EJKORDFLEX.
         RUN TIMSEK.P.      
         ASSIGN
         splus = splus + sekunder
         sekunder = splus.
         RUN FSEKTIM.P.
         ASSIGN 
         fsal.TOTALT = fnytid.
         plgrans = 40.
         IF FORETAG.FORETAG = "GKAL"  THEN plgrans = 25.
         IF FORETAG.FORETAG = "LULE"  THEN plgrans = 30.
         IF fsal.TOTALT > plgrans THEN DO:
            ASSIGN fsal.GRANS = "+"
            fsal.PLMIN = fsal.TOTALT - plgrans.         
         END.   
         IF fsal.TOTALT < -10 THEN DO:
            ASSIGN fsal.GRANS = "-".
            fsal.PLMIN = fsal.TOTALT + 10.
            fsal.PLMIN = fsal.PLMIN * (-1).
         END.
      END.      
      GET NEXT flgam NO-LOCK.
   END.              
   CLOSE QUERY flgam.    
END PROCEDURE.

