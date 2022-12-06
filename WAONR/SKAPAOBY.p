/*SKAPAOBY.P*/
DEFINE VARIABLE bdatum AS DATE NO-UNDO.
DEFINE VARIABLE avdatum AS DATE NO-UNDO.

DEFINE VARIABLE aonummer AS CHARACTER NO-UNDO.
DEFINE VARIABLE delnummer AS INTEGER NO-UNDO.
DEFINE VARIABLE baonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE bdelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE vadgora AS INTEGER NO-UNDO.
DEFINE VARIABLE vartal AS INTEGER NO-UNDO.
DEFINE VARIABLE ardatum AS INTEGER NO-UNDO.
DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE berindvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE utomr AS CHARACTER NO-UNDO.
DEFINE VARIABLE valdelnrlog AS LOGICAL NO-UNDO.
DEFINE VARIABLE ressummavar AS DECIMAL NO-UNDO.
DEFINE VARIABLE kollvecka AS CHARACTER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.         

{BYTAO.I}

{TIDUTTTNEW.I}




DEFINE INPUT-OUTPUT PARAMETER TABLE FOR bytaao.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR dagtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR indertemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR restid.
DEFINE OUTPUT PARAMETER medbytao AS CHARACTER NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tidut.
FIND FIRST bytaao.
ASSIGN
/*bytaonrmed = medbytao*/
vadgora = bytaao.VADGORA 
bdatum =  bytaao.STDATUM 
avdatum =  bytaao.SLDATUM  
vartal = bytaao.ARTAL 
ardatum = bytaao.RAD_PERIOD 
aonummer = bytaao.AONR 
delnummer = bytaao.DELNR 
globforetag = bytaao.GLOBFORETAG
inder = bytaao.INDER
utomr = bytaao.SOK1
kollvecka = bytaao.SOK2
valdelnrlog = bytaao.SOK5.


medbytao = "".
FIND FIRST indertemp NO-ERROR.
musz = FALSE.
/* om sökning på område skall aonummer vara blank*/
IF aonummer = "" THEN musz = TRUE.
ELSE DO:
   FIND FIRST BYTAONR WHERE BYTAONR.AONR = aonummer
   AND BYTAONR.DELNR = delnummer  NO-LOCK NO-ERROR.      
   IF AVAILABLE BYTAONR THEN DO:
      ASSIGN   
      baonr = BYTAONR.NAONR. 
      bdelnr = BYTAONR.NDELNR.
      musz = TRUE.
   END.
   IF NOT AVAILABLE BYTAONR THEN DO:
      FIND FIRST BYTAONR WHERE BYTAONR.NAONR = aonummer
      AND BYTAONR.NDELNR = delnummer  NO-LOCK NO-ERROR.
      IF AVAILABLE BYTAONR THEN DO:
         ASSIGN      
         baonr = BYTAONR.AONR 
         bdelnr = BYTAONR.DELNR.
         musz = TRUE.
      END.
   END.
END.
IF musz = TRUE THEN DO:
   musz = FALSE.
   IF AVAILABLE BYTAONR THEN DO:   
      medbytao = BYTAONR.AONR + " " + STRING(BYTAONR.DELNR,"99") + " har bytts till " + BYTAONR.NAONR + " " + STRING(BYTAONR.NDELNR ,"99").
   END.
   IF ardatum = 1 AND vadgora = 1 THEN DO:
     
   END.
   ELSE IF ardatum = 2 AND vadgora = 1 THEN DO:   
      OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND
      SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = baonr AND SUMTIDDAG.DELNR = bdelnr 
      USE-INDEX AONR NO-LOCK.
      GET FIRST dagsumq NO-LOCK.
      DO WHILE AVAILABLE(SUMTIDDAG) TRANSACTION:
          
         IF SUMTIDDAG.PRISTYP = "RESTID..." THEN DO: 
            FIND FIRST restid WHERE restid.AONR = SUMTIDDAG.AONR AND
            restid.DELNR = SUMTIDDAG.DELNR AND restid.OMRADE = SUMTIDDAG.OMRADE  USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE restid THEN CREATE restid.
            ASSIGN     
            restid.OMRADE = SUMTIDDAG.OMRADE      
            restid.AONR = aonummer
            restid.DELNR = delnummer 
            restid.TIMMAR = restid.TIMMAR + SUMTIDDAG.TIMMAR.  
            CREATE dagtemp.
            ASSIGN          
            dagtemp.AONR = aonummer
            dagtemp.DELNR = delnummer
            dagtemp.POMRADE = SUMTIDDAG.OMRADE 
            dagtemp.PRISTYP = SUMTIDDAG.PRISTYP            
            dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
            dagtemp.BELOPP = SUMTIDDAG.BELOPP 
            dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
            dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
            dagtemp.LONKOST = SUMTIDDAG.LONKOST.
         END.
         ELSE DO:
            CREATE dagtemp.
            ASSIGN          
            dagtemp.AONR = aonummer
            dagtemp.DELNR = delnummer
            dagtemp.PRISTYP = SUMTIDDAG.PRISTYP
            dagtemp.TIMMAR = SUMTIDDAG.TIMMAR
            dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
            dagtemp.BELOPP = SUMTIDDAG.BELOPP 
            dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
            dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
            dagtemp.LONKOST = SUMTIDDAG.LONKOST.
         END.
         GET NEXT dagsumq NO-LOCK. 
      END.          
   END.      
   ELSE IF ardatum = 1 AND vadgora = 3  THEN DO:
      
   END.
   ELSE IF ardatum = 2 AND vadgora = 3  THEN DO:
      IF valdelnrlog = FALSE THEN DO: 
         OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND
         SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = baonr AND 
         SUMTIDDAG.DELNR = bdelnr USE-INDEX AONR NO-LOCK.
      END.
      ELSE DO:
         OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND
         SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = baonr 
         USE-INDEX AONR NO-LOCK.
      END.
      GET FIRST dagsumq NO-LOCK.
      DO WHILE AVAILABLE(SUMTIDDAG) TRANSACTION: 
          
         IF SUMTIDDAG.PRISTYP = "RESTID..." THEN DO: 
            FIND FIRST restid WHERE restid.AONR = SUMTIDDAG.AONR AND
            restid.DELNR = SUMTIDDAG.DELNR AND restid.OMRADE = SUMTIDDAG.OMRADE USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE restid THEN CREATE restid.
            ASSIGN   
            restid.OMRADE = SUMTIDDAG.OMRADE       
            restid.AONR = aonummer
            restid.DELNR = delnummer 
            restid.TIMMAR = restid.TIMMAR + SUMTIDDAG.TIMMAR.   
            ressummavar = ressummavar + SUMTIDDAG.TIMMAR.
            bytaao.SOK6 = ressummavar.
            CREATE dagtemp.
            ASSIGN          
            dagtemp.AONR = aonummer
            dagtemp.DELNR = delnummer
            dagtemp.POMRADE = SUMTIDDAG.OMRADE
            dagtemp.PERSMASK = SUMTIDDAG.PERSMASK             
            dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
            dagtemp.BELOPP = SUMTIDDAG.BELOPP 
            dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
            dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
            dagtemp.LONKOST = SUMTIDDAG.LONKOST.
         END.
         ELSE DO:
            CREATE dagtemp.
            ASSIGN          
            dagtemp.AONR = aonummer
            dagtemp.DELNR = delnummer
            dagtemp.POMRADE = SUMTIDDAG.OMRADE
            dagtemp.PERSMASK = SUMTIDDAG.PERSMASK 
            dagtemp.TIMMAR = SUMTIDDAG.TIMMAR
            dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
            dagtemp.BELOPP = SUMTIDDAG.BELOPP 
            dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
            dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
            dagtemp.LONKOST = SUMTIDDAG.LONKOST.
         END.
         GET NEXT dagsumq NO-LOCK. 
      END.   
   END.
   ELSE IF vadgora = 2 THEN DO:
      OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND 
      TIDREGITAB.DATUM <= avdatum AND
      TIDREGITAB.AONR = baonr AND TIDREGITAB.DELNR = bdelnr AND TIDREGITAB.TIDLOG = TRUE         
      USE-INDEX AONR NO-LOCK.
      GET FIRST tq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB): 
         CREATE tidut.         
         ASSIGN
         SUBSTRING(tidut.UT,1) = TIDREGITAB.PERSONALKOD 
         SUBSTRING(tidut.UT,15) = aonummer
         SUBSTRING(tidut.UT,22) = STRING(delnummer,">99") 
         SUBSTRING(tidut.UT,26) = STRING(TIDREGITAB.DATUM) 
         SUBSTRING(tidut.UT,35) = STRING(TIDREGITAB.START,"99.99") 
         SUBSTRING(tidut.UT,41) = STRING(TIDREGITAB.SLUT,"99.99")
         SUBSTRING(tidut.UT,47) =                
         SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),1,2) +                 
         STRING(DECIMAL(SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),4,2)) / 60,".99")
         SUBSTRING(tidut.UT,54) = TIDREGITAB.PRISTYP
         SUBSTRING(tidut.UT,65) = SUBSTRING(TIDREGITAB.GODKAND,1,4)
         SUBSTRING(tidut.UT,71) = TIDREGITAB.VECKOKORD.
         GET NEXT tq NO-LOCK.
      END.            
   END.
   ELSE IF ardatum = 1 AND vadgora = 4 THEN DO:
      
        
   END.
   ELSE IF ardatum = 2 AND vadgora = 4 THEN DO:
      IF valdelnrlog = FALSE THEN DO: 
         OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND
         SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = baonr AND 
         SUMTIDDAG.DELNR = bdelnr USE-INDEX AONR NO-LOCK.
      END.
      ELSE DO:
         IF bytaao.SOK1 = "Kontroll mot ekonomisystem" THEN DO:            
            OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE 
            YEAR(SUMTIDDAG.DATUM) = YEAR(bdatum) AND
            MONTH(SUMTIDDAG.DATUM) = MONTH(bdatum) AND 
            SUMTIDDAG.AONR = baonr AND
            SUMTIDDAG.DELNR = bdelnr AND  
            SUMTIDDAG.VECKOKORD NE "" AND
            SUMTIDDAG.PRISTYP NE "FRÅNVARO."
            USE-INDEX AONR NO-LOCK. 
         END.
         ELSE IF bytaao.SOK1 = "ekonomi koll natt" THEN DO:            
            OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE 
            SUMTIDDAG.AONR = baonr AND SUMTIDDAG.DELNR = bdelnr AND
            SUMTIDDAG.VECKOKORD = kollvecka AND
            SUMTIDDAG.PRISTYP NE "FRÅNVARO."
            USE-INDEX AONR NO-LOCK. 
         END.       
         ELSE DO:
            OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE 
            SUMTIDDAG.DATUM >= bdatum AND
            SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = baonr  
            USE-INDEX AONR NO-LOCK.
         END.   
      END.      
      GET FIRST dagsumq NO-LOCK.
      DO WHILE AVAILABLE(SUMTIDDAG) TRANSACTION:
         berindvar = 1.
          
         IF SUMTIDDAG.PRISTYP = "RESTID..." THEN DO: 
            FIND FIRST restid WHERE restid.AONR = SUMTIDDAG.AONR AND
            restid.DELNR = SUMTIDDAG.DELNR AND restid.PERSONALKOD = SUMTIDDAG.PERSONALKOD AND 
            restid.OMRADE = SUMTIDDAG.OMRADE
            USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE restid THEN CREATE restid.
            ASSIGN          
            restid.AONR = aonummer
            restid.DELNR = delnummer
            restid.PERSONALKOD = SUMTIDDAG.PERSONALKOD
            restid.OMRADE = SUMTIDDAG.OMRADE 
            restid.TIMMAR = restid.TIMMAR + SUMTIDDAG.TIMMAR.
            CREATE dagtemp.
            ASSIGN          
            dagtemp.AONR = SUMTIDDAG.AONR
            dagtemp.DELNR = SUMTIDDAG.DELNR 
            dagtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD 
            dagtemp.NAMN = SUBSTRING(SUMTIDDAG.FORNAMN,1,1) + "." + 
            SUBSTRING(SUMTIDDAG.EFTERNAMN,1,3)
            dagtemp.GEOMRADE = SUMTIDDAG.GEOMRADE 
            dagtemp.POMRADE = SUMTIDDAG.OMRADE
            dagtemp.OMRADE = SUMTIDDAG.OMRADE 
            dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
            dagtemp.BELOPP = SUMTIDDAG.BELOPP 
            dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
            dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
            dagtemp.LONKOST = SUMTIDDAG.LONKOST
            dagtemp.IKOST = SUMTIDDAG.IKOSTNAD * berindvar
            dagtemp.PRIS = SUMTIDDAG.PRIS
            dagtemp.PRISTYP = SUMTIDDAG.PRISTYP
            dagtemp.PRISI = SUMTIDDAG.PRISI
            dagtemp.DATUM = SUMTIDDAG.DATUM.
         END.
         ELSE DO:
            CREATE dagtemp.
            ASSIGN          
            dagtemp.AONR = aonummer
            dagtemp.DELNR = delnummer
            dagtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD 
            dagtemp.NAMN = SUBSTRING(SUMTIDDAG.FORNAMN,1,1) + "." + 
            SUBSTRING(SUMTIDDAG.EFTERNAMN,1,3)
            dagtemp.GEOMRADE = SUMTIDDAG.GEOMRADE 
            dagtemp.OMRADE = SUMTIDDAG.OMRADE 
            dagtemp.POMRADE = SUMTIDDAG.OMRADE
            dagtemp.TIMMAR = SUMTIDDAG.TIMMAR
            dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
            dagtemp.BELOPP = SUMTIDDAG.BELOPP 
            dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
            dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
            dagtemp.LONKOST = SUMTIDDAG.LONKOST
            dagtemp.IKOST = SUMTIDDAG.IKOSTNAD * berindvar
            dagtemp.PRIS = SUMTIDDAG.PRIS
            dagtemp.PRISTYP = SUMTIDDAG.PRISTYP
            dagtemp.PRISI = SUMTIDDAG.PRISI
            dagtemp.DATUM = SUMTIDDAG.DATUM.        
         END.
         GET NEXT dagsumq NO-LOCK. 
      END.      
   END.
   ELSE IF vadgora = 5 THEN DO:     
      
 
   END.
   ELSE IF vadgora = 6 AND bytaao.RAD_PERIOD = 1 AND bytaao.SOK3 = 1 THEN DO:
       
              
   END.
   ELSE IF vadgora = 6 AND bytaao.RAD_PERIOD = 2 AND bytaao.SOK3 = 1 THEN DO:
       
      /* lägg till aonr som tidigare hade fel omrade . Det nya området skall gälla även för det gamla aonrret
      SUMTIDDAG.GEOMRADE behöver ej kollas då projektnummer har hittats */
      OPEN QUERY dagsumq2 FOR EACH BYTAONR WHERE BYTAONR.OMRADE = bytaao.SOK1 AND BYTAONR.OMRADE NE BYTAONR.NOMRADE NO-LOCK,
      EACH SUMTIDDAG WHERE SUMTIDDAG.AONR = BYTAONR.AONR AND SUMTIDDAG.DELNR = BYTAONR.DELNR AND SUMTIDDAG.OMRADE = bytaao.SOK1 
      /*AND SUMTIDDAG.GEOMRADE NE bytaao.SOK1*/ AND SUMTIDDAG.DATUM >= bdatum AND
      SUMTIDDAG.DATUM <= avdatum NO-LOCK.     
      GET FIRST dagsumq2  NO-LOCK.
      DO WHILE AVAILABLE(SUMTID) TRANSACTION:                     
         IF SUMTIDDAG.PRISTYP NE "FRÅNVARO." THEN DO:
            CREATE dagtemp.
            ASSIGN 
            dagtemp.AONR = BYTAONR.NAONR 
            dagtemp.DELNR = BYTAONR.NDELNR
            dagtemp.POMRADE = SUMTIDDAG.OMRADE  
            dagtemp.OMRADE = SUMTIDDAG.OMRADE 
            dagtemp.GEOMRADE = BYTAONR.NOMRADE                             
            dagtemp.TIMMAR = SUMTIDDAG.TIMMAR
            dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR
            dagtemp.BELOPP = SUMTIDDAG.BELOPP
            dagtemp.OBELOPP = SUMTIDDAG.OBELOPP
            dagtemp.LONKOST = SUMTIDDAG.LONKOST
            dagtemp.TBELOPP = SUMTIDDAG.TBELOPP.                      
         END.
         GET NEXT dagsumq2  NO-LOCK.                         
      END.   
     
   END.
   ELSE IF vadgora = 6 AND bytaao.RAD_PERIOD = 1 AND bytaao.SOK3 = 2 THEN DO:      
               
   END.
   ELSE IF vadgora = 6 AND bytaao.RAD_PERIOD = 2 AND bytaao.SOK3 = 2 THEN DO:
       
      /* lägg till aonr som tidigare hade fel omrade . Det nya området skall gälla även för det gamla aonrret
      SUMTIDDAG.GEOMRADE behöver ej kollas då projektnummer har hittats */
      OPEN QUERY dagsumq2 FOR EACH BYTAONR WHERE BYTAONR.NOMRADE = bytaao.SOK1 AND BYTAONR.OMRADE NE BYTAONR.NOMRADE NO-LOCK,
      EACH SUMTIDDAG WHERE SUMTIDDAG.AONR = BYTAONR.AONR AND SUMTIDDAG.DELNR = BYTAONR.DELNR AND SUMTIDDAG.OMRADE NE bytaao.SOK1 
      /*AND SUMTIDDAG.GEOMRADE = bytaao.SOK1*/ AND SUMTIDDAG.DATUM >= bdatum AND
      SUMTIDDAG.DATUM <= avdatum NO-LOCK.     
      GET FIRST dagsumq2  NO-LOCK.
      DO WHILE AVAILABLE(SUMTID) TRANSACTION:                     
         IF SUMTIDDAG.PRISTYP NE "FRÅNVARO." THEN DO:
            CREATE dagtemp.
            ASSIGN 
            dagtemp.AONR = BYTAONR.NAONR 
            dagtemp.DELNR = BYTAONR.NDELNR
            dagtemp.POMRADE = SUMTIDDAG.OMRADE
            dagtemp.OMRADE = SUMTIDDAG.OMRADE 
            dagtemp.GEOMRADE = BYTAONR.NOMRADE                             
            dagtemp.TIMMAR = SUMTIDDAG.TIMMAR
            dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR
            dagtemp.BELOPP = SUMTIDDAG.BELOPP
            dagtemp.OBELOPP = SUMTIDDAG.OBELOPP
            dagtemp.LONKOST = SUMTIDDAG.LONKOST
            dagtemp.TBELOPP = SUMTIDDAG.TBELOPP.                      
         END.
         GET NEXT dagsumq2  NO-LOCK.                         
      END.   
   END.
   ELSE IF vadgora = 7 AND bytaao.SOK1 = "ALLA"  THEN DO:
      OPEN QUERY arsumq3 FOR EACH BYTAONR WHERE BYTAONR.OMRADE NE BYTAONR.NOMRADE NO-LOCK,
      EACH dagtemp WHERE dagtemp.AONR = BYTAONR.AONR AND dagtemp.DELNR = BYTAONR.DELNR.
      GET FIRST arsumq3 NO-LOCK.         
      DO WHILE AVAILABLE(dagtemp):
         DO TRANSACTION:          
            FIND CURRENT dagtemp EXCLUSIVE-LOCK.
            ASSIGN  
            dagtemp.AONR = BYTAONR.NAONR
            dagtemp.DELNR = BYTAONR.NDELNR            
            dagtemp.OMRADE = BYTAONR.NOMRADE.                     
         END.
         GET NEXT arsumq3 NO-LOCK. 
      END.
      OPEN QUERY arsumq4 FOR EACH BYTAONR WHERE BYTAONR.OMRADE NE BYTAONR.NOMRADE NO-LOCK,
      EACH restid WHERE restid.AONR = BYTAONR.AONR AND restid.DELNR = BYTAONR.DELNR.
      GET FIRST arsumq4 NO-LOCK.         
      DO WHILE AVAILABLE(restid) TRANSACTION:                                                         
         ASSIGN  
         restid.AONR = BYTAONR.NAONR
         restid.DELNR = BYTAONR.NDELNR            
         restid.OMRADE = BYTAONR.NOMRADE.                     
         
         GET NEXT arsumq4 NO-LOCK. 
      END.
   END.   
   ELSE IF vadgora = 7 AND bytaao.RAD_PERIOD = 1  THEN DO:
      
      
   END.
   ELSE IF vadgora = 7 AND bytaao.RAD_PERIOD = 2 THEN DO:
      OPEN QUERY dagsumq2 FOR EACH BYTAONR WHERE BYTAONR.NOMRADE = bytaao.SOK1 AND BYTAONR.OMRADE NE BYTAONR.NOMRADE NO-LOCK,
      EACH SUMTIDDAG WHERE SUMTIDDAG.AONR = BYTAONR.AONR AND SUMTIDDAG.DELNR = BYTAONR.DELNR AND
      SUMTIDDAG.DATUM >= bdatum AND SUMTIDDAG.DATUM <= avdatum USE-INDEX GEORGAONR NO-LOCK.
      GET FIRST dagsumq2 NO-LOCK.
      DO WHILE AVAILABLE(SUMTIDDAG) TRANSACTION:            
         IF SUMTIDDAG.PRISTYP = "RESTID..." THEN DO: 
            IF SUMTIDDAG.DATUM >= 01/01/99 THEN berindvar = 1.15.
            ELSE IF SUMTIDDAG.DATUM < 01/01/99 THEN berindvar = 0.70.
            CREATE restid.
            ASSIGN  
            restid.OMRADE = SUMTIDDAG.OMRADE
            restid.AONR = BYTAONR.NAONR
            restid.DELNR = BYTAONR.NDELNR               
            restid.OMRADE = BYTAONR.NOMRADE                               
            restid.TIMMAR = restid.TIMMAR + SUMTIDDAG.TIMMAR.   
            CREATE dagtemp.
            ASSIGN    
            dagtemp.AONR = BYTAONR.NAONR
            dagtemp.DELNR = BYTAONR.NDELNR 
            dagtemp.ORT = SUMTIDDAG.ORT     
            dagtemp.INOMRADE = SUMTIDDAG.OMRADE             
            dagtemp.OMRADE = BYTAONR.NOMRADE 
            dagtemp.POMRADE = SUMTIDDAG.OMRADE
            dagtemp.PRISTYP = SUMTIDDAG.PRISTYP
            dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
            dagtemp.BELOPP = SUMTIDDAG.BELOPP 
            dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
            dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
            dagtemp.LONKOST = SUMTIDDAG.LONKOST
            dagtemp.IKOST = SUMTIDDAG.IKOSTNAD * berindvar.
         END.
         ELSE DO:
            IF SUMTIDDAG.DATUM >= 01/01/99 THEN berindvar = 1.15.
            ELSE IF SUMTIDDAG.DATUM < 01/01/99 THEN berindvar = 0.70.
            CREATE dagtemp.
            ASSIGN    
            dagtemp.AONR = BYTAONR.NAONR
            dagtemp.DELNR = BYTAONR.NDELNR 
            dagtemp.ORT = SUMTIDDAG.ORT     
            dagtemp.INOMRADE = SUMTIDDAG.OMRADE             
            dagtemp.OMRADE = BYTAONR.NOMRADE 
            dagtemp.POMRADE = SUMTIDDAG.OMRADE
            dagtemp.PRISTYP = SUMTIDDAG.PRISTYP
            dagtemp.TIMMAR = SUMTIDDAG.TIMMAR
            dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
            dagtemp.BELOPP = SUMTIDDAG.BELOPP 
            dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
            dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
            dagtemp.LONKOST = SUMTIDDAG.LONKOST
            dagtemp.IKOST = SUMTIDDAG.IKOSTNAD * berindvar.
         END.
         GET NEXT dagsumq2 NO-LOCK. 
      END.   
    
   END.
   
   ELSE IF vadgora = 8 AND bytaao.SOK1 = "ALLA"  THEN DO:
      OPEN QUERY arsumq3 FOR EACH BYTAONR WHERE BYTAONR.OMRADE NE BYTAONR.NOMRADE NO-LOCK,
      EACH dagtemp WHERE dagtemp.AONR = BYTAONR.AONR AND dagtemp.DELNR = BYTAONR.DELNR.
      GET FIRST arsumq3 NO-LOCK.         
      DO WHILE AVAILABLE(dagtemp) :        
         DO TRANSACTION:
            FIND CURRENT dagtemp EXCLUSIVE-LOCK.
            ASSIGN  
            dagtemp.AONR = BYTAONR.NAONR
            dagtemp.DELNR = BYTAONR.NDELNR            
            dagtemp.GEOMRADE = BYTAONR.NOMRADE.                     
         END.
         GET NEXT arsumq3 NO-LOCK. 
      END.      
   END.   
   ELSE IF vadgora = 8 AND bytaao.RAD_PERIOD = 1 THEN DO:
   
   END.
   ELSE IF vadgora = 8 AND bytaao.RAD_PERIOD = 2 THEN DO:
      OPEN QUERY dagsumq3 FOR EACH BYTAONR WHERE BYTAONR.NOMRADE = bytaao.SOK1 AND BYTAONR.OMRADE NE BYTAONR.NOMRADE NO-LOCK,
      EACH SUMTIDDAG WHERE SUMTIDDAG.AONR = BYTAONR.AONR AND SUMTIDDAG.DELNR = BYTAONR.DELNR AND
      SUMTIDDAG.DATUM >= bdatum AND SUMTIDDAG.DATUM <= avdatum NO-LOCK.
      GET FIRST dagsumq3 NO-LOCK.
      DO WHILE AVAILABLE(SUMTIDDAG) TRANSACTION:
         CREATE dagtemp.
         ASSIGN
         dagtemp.POMRADE = SUMTIDDAG.OMRADE
         dagtemp.AONR = BYTAONR.NAONR
         dagtemp.DELNR = BYTAONR.NDELNR
         dagtemp.GEOMRADE = BYTAONR.NOMRADE
         dagtemp.PRISTYP = SUMTIDDAG.PRISTYP                        
         dagtemp.TIMMAR = SUMTIDDAG.TIMMAR
         dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR. 
         GET NEXT dagsumq3 NO-LOCK. 
      END.
   END.
   RELEASE dagtemp NO-ERROR.
   RELEASE SUMTIDDAG NO-ERROR.
END.


