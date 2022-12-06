/*APAOUTF.P*/
{APP.I}
DEFINE VARIABLE berindvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE arrhjsum LIKE TIDREGITAB.BERANTAL NO-UNDO.    
DEFINE VARIABLE arrhjsumtid LIKE TIDREGITAB.BERANTAL NO-UNDO.  
DEFINE VARIABLE arrhjsumotid LIKE TIDREGITAB.BERANTAL NO-UNDO. 
DEFINE VARIABLE arrhjsumove LIKE TIDREGITAB.BERANTAL NO-UNDO.
DEFINE VARIABLE arrhjsumtra LIKE TIDREGITAB.BERANTAL NO-UNDO.   
DEFINE VARIABLE arrhjsumlon LIKE TIDREGITAB.BERANTAL NO-UNDO.   
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO. 
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.


DEFINE TEMP-TABLE slutsum           
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD TIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "TIMMAR"      
   FIELD OTIMMAR LIKE EKRAPPRESULT.EANTAL LABEL "OTIMMAR"         
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "ARBKOSTNAD"           
   FIELD OBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "Ö-KOSTNAD"  
   FIELD OANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP LIKE EKRAPPRESULT.EBELOPP LABEL "T-KOSTNAD"
   FIELD TANTAL LIKE EKRAPPRESULT.EANTAL  LABEL "T-ANTAL"    
   FIELD LONKOST LIKE EKRAPPRESULT.EBELOPP LABEL "L-KOSTNAD"     
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING. 
   

   
DEFINE QUERY dagsumq FOR SUMTIDDAG.   
DEFINE QUERY arsumq FOR SUMTID.    
   
DEFINE VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Visning per år", 1,
"Visning per period", 2
     SIZE 21.5 BY 2.5 BGCOLOR 8 NO-UNDO.
{AOUTFUT.I}
{BYTAO.I}

/* ***************************  Main Block  *************************** */
EMPTY TEMP-TABLE tidut  NO-ERROR. 
EMPTY TEMP-TABLE dagtemp NO-ERROR. 
EMPTY TEMP-TABLE slutsum NO-ERROR. 
FIND FIRST uppfoltemp NO-LOCK NO-ERROR.
ASSIGN

 
RAD_PERIOD = uppfoltemp.PERIOD      
aonummer = uppfoltemp.AONR
delnummer = uppfoltemp.DELNR   
bdatum = uppfoltemp.INDATUM
avdatum = uppfoltemp.UTDATUM.
str=
"=====================================================================================".            
RUN huvud_UI.             
IF musz = FALSE THEN DO:   
   /*/*  byt aonr område osv  lena*/
   DEFINE VARIABLE spaonummer AS CHARACTER NO-UNDO.
   DEFINE VARIABLE spdelnummer AS INTEGER NO-UNDO.
   FIND FIRST BYTAONR WHERE BYTAONR.AONR = aonummer
   AND BYTAONR.DELNR = delnummer  NO-LOCK NO-ERROR.      
   IF AVAILABLE BYTAONR THEN DO:
      ASSIGN
      spaonummer = aonummer.
      spdelnummer = delnummer.
      aonummer = BYTAONR.NAONR. 
      delnummer = BYTAONR.NDELNR.
   END.
   IF NOT AVAILABLE BYTAONR THEN DO:
      FIND FIRST BYTAONR WHERE BYTAONR.NAONR = aonummer
      AND BYTAONR.NDELNR = delnummer  NO-LOCK NO-ERROR.
      IF AVAILABLE BYTAONR THEN DO:
         ASSIGN
         spaonummer = aonummer
         spdelnummer = delnummer
         aonummer = BYTAONR.AONR 
         delnummer = BYTAONR.DELNR.
      END.
   END.
   IF AVAILABLE BYTAONR THEN DO:
      RUN summa_UI (INPUT TRUE).   
      ASSIGN
      aonummer = spaonummer
      delnummer = spdelnummer.
   END.*/
   RUN summa_UI (INPUT FALSE).   /*valt projektnummer*/
   RETURN.       
END.
ELSE DO:
   RETURN.
END.
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/ 
   DO TRANSACTION:   
      IF RAD_PERIOD = 1 THEN DO:
         FIND FIRST SUMTID WHERE SUMTID.DATUM = bdatum AND SUMTID.AONR = aonummer AND
         SUMTID.DELNR = delnummer USE-INDEX AONR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE SUMTID THEN DO:            
           CREATE FELTEXT.
           ASSIGN 
           FELTEXT.ANVANDARE = Guru.Konstanter:globanv                      
           FELTEXT.FELTEXT = "Det finns ingen tid skriven på " + LC(Guru.Konstanter:gaok) + aonummer + " " + STRING(delnummer,Guru.Konstanter:varforetypchar[1])
           FELTEXT.PROGRAM = "APAOUTF" + STRING(TODAY) + Guru.Konstanter:globanv.
            musz = TRUE.    
            RETURN.
         END.
      END. 
      IF RAD_PERIOD = 2 THEN DO:
         FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND SUMTIDDAG.DATUM <= avdatum AND
         SUMTIDDAG.AONR = aonummer AND SUMTIDDAG.DELNR = delnummer USE-INDEX AONR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE SUMTIDDAG THEN DO: 
            musz = TRUE.
            RETURN.
         END.
      END.
      IF RAD_PERIOD = 3 THEN DO:
         FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.AONR = aonummer AND SUMTIDDAG.DELNR = delnummer USE-INDEX AONR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE SUMTIDDAG THEN DO: 
            musz = TRUE.
            RETURN.
         END.
      END.
      IF musz = TRUE THEN musz = musz.
      ELSE DO:   
         inder = FALSE.
                                                                                             
         str = "==========.========.=========.========.=========.========.=========.=========". 
         str2= "-----------------------------------------------------------------------------".         
         CREATE tidut. 
         SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
         CREATE tidut.
         CREATE tidut.
                                  
         tidut.UT = "U10- " + CAPS(Guru.Konstanter:gaol) + "-TID-ÖVERTID-TRAKTAMENTEN PERIOD". 
         IF RAD_PERIOD = 1 THEN DO: 
            SUBSTRING(tidut.UT,52) = STRING(YEAR(bdatum),"9999").
         END.
         IF RAD_PERIOD = 2 THEN DO:
            SUBSTRING(tidut.UT,52) = STRING(bdatum) + " - " + STRING(avdatum).     
         END. 
         IF RAD_PERIOD = 3 THEN DO:
            SUBSTRING(tidut.UT,52) = "All tid på " + LC(Guru.Konstanter:gaok).     
         END.
         CREATE tidut.  
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok) + ":".                             
         IF AVAILABLE SUMTID THEN
         ASSIGN                            
         SUBSTRING(tidut.UT,7) = SUMTID.AONR   
         SUBSTRING(tidut.UT,14) = STRING(SUMTID.DELNR,Guru.Konstanter:varforetypchar[1]) 
         SUBSTRING(tidut.UT,19) = SUMTID.ORT.       
         ELSE
         ASSIGN                            
         SUBSTRING(tidut.UT,7) = SUMTIDDAG.AONR   
         SUBSTRING(tidut.UT,14) = STRING(SUMTIDDAG.DELNR,Guru.Konstanter:varforetypchar[1]) 
         SUBSTRING(tidut.UT,19) = SUMTIDDAG.ORT.                        
         CREATE tidut.
         CREATE tidut.      
         ASSIGN                 
         SUBSTRING(tidut.UT,12) = "ARB."
         SUBSTRING(tidut.UT,21) = "ARBETS" 
         SUBSTRING(tidut.UT,31) = "ÖVER."  
         SUBSTRING(tidut.UT,40) = "ÖVERTID"   
         SUBSTRING(tidut.UT,50) = "RES."
         SUBSTRING(tidut.UT,59) = "TRAKT."                  
         SUBSTRING(tidut.UT,69) = "LÖNETILL.".         
              
         CREATE tidut.      
         ASSIGN                                
         SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)
         SUBSTRING(tidut.UT,12) = "TIMMAR"             
         SUBSTRING(tidut.UT,21) = "KOSTNAD"         
         SUBSTRING(tidut.UT,31) = "TIMMAR"  
         SUBSTRING(tidut.UT,40) = "KOSTNAD"
         SUBSTRING(tidut.UT,50) = "TIMMAR"
         SUBSTRING(tidut.UT,59) = "KOSTNAD"
         SUBSTRING(tidut.UT,69) = "KOSTNAD".  
                                               
         CREATE tidut.       
         SUBSTRING(tidut.UT,1) = str.             
      END.
   END.   
END PROCEDURE.
PROCEDURE summa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   DEFINE INPUT PARAMETER bytao AS LOGICAL NO-UNDO.
   IF musz = TRUE THEN RETURN.
   EMPTY TEMP-TABLE slutsum NO-ERROR.    
   /*IF bytao = TRUE THEN DO:
      CREATE tidut.    
      ASSIGN                   
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok) + ": " + BYTAONR.AONR + " " + STRING(BYTAONR.DELNR,Guru.Konstanter:varforetypchar[1]) + " har bytts till: " + CAPS(Guru.Konstanter:gaok) + BYTAONR.NAONR + " " + STRING(BYTAONR.NDELNR,Guru.Konstanter:varforetypchar[1]).    
   END.*/
   ASSIGN   
   arrhjsum = 0   
   arrhjsumtid = 0  
   arrhjsumotid = 0    
   arrhjsumove = 0    
   arrhjsumtra = 0       
   arrhjsumlon = 0.
   CREATE indertemp.
   IF RAD_PERIOD = 1 THEN DO:                       
      EMPTY TEMP-TABLE bytaao NO-ERROR.       
      FIND FIRST indertemp NO-ERROR.
      OPEN QUERY arsumq FOR EACH SUMTID WHERE SUMTID.DATUM = bdatum AND 
      SUMTID.AONR = aonummer AND
      SUMTID.DELNR = delnummer USE-INDEX AONR NO-LOCK. 
      GET FIRST arsumq NO-LOCK.
      DO WHILE AVAILABLE(SUMTID) TRANSACTION:          
                                              
         IF SUMTID.PRISTYP = "RESTID..." THEN DO: 
            FIND FIRST restid WHERE restid.AONR = SUMTID.AONR AND
            restid.DELNR = SUMTID.DELNR USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE restid THEN CREATE restid.
            ASSIGN          
            restid.AONR = SUMTID.AONR
            restid.DELNR = SUMTID.DELNR 
            restid.TIMMAR = restid.TIMMAR + SUMTID.TIMMAR. 
            CREATE dagtemp.
            ASSIGN          
            dagtemp.AONR = SUMTID.AONR
            dagtemp.DELNR = SUMTID.DELNR 
            dagtemp.PRISTYP = SUMTID.PRISTYP            
            dagtemp.OTIMMAR = SUMTID.OTIMMAR 
            dagtemp.BELOPP = SUMTID.BELOPP 
            dagtemp.OBELOPP = SUMTID.OBELOPP 
            dagtemp.TBELOPP = SUMTID.TBELOPP
            dagtemp.LONKOST = SUMTID.LONKOST.
         END.
         ELSE DO:
            CREATE dagtemp.
            ASSIGN          
            dagtemp.AONR = SUMTID.AONR
            dagtemp.DELNR = SUMTID.DELNR 
            dagtemp.PRISTYP = SUMTID.PRISTYP
            dagtemp.TIMMAR = SUMTID.TIMMAR
            dagtemp.OTIMMAR = SUMTID.OTIMMAR 
            dagtemp.BELOPP = SUMTID.BELOPP 
            dagtemp.OBELOPP = SUMTID.OBELOPP 
            dagtemp.TBELOPP = SUMTID.TBELOPP
            dagtemp.LONKOST = SUMTID.LONKOST.
         END.
         GET NEXT arsumq NO-LOCK. 
      END.
   END.
   
   IF RAD_PERIOD = 2 OR RAD_PERIOD = 3 THEN DO:      
      EMPTY TEMP-TABLE bytaao NO-ERROR.       
      FIND FIRST indertemp NO-ERROR.                 
      IF RAD_PERIOD = 2 THEN DO:      
         OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND
         SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = aonummer AND SUMTIDDAG.DELNR = delnummer 
         USE-INDEX AONR NO-LOCK.
      END.
      ELSE DO:
         OPEN QUERY dagsumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.AONR = aonummer AND SUMTIDDAG.DELNR = delnummer 
         USE-INDEX AONR NO-LOCK.
      END.
      GET FIRST dagsumq NO-LOCK.
      DO WHILE AVAILABLE(SUMTIDDAG) TRANSACTION:                  
         IF SUMTIDDAG.PRISTYP = "RESTID..." THEN DO: 
            FIND FIRST restid WHERE restid.AONR = SUMTIDDAG.AONR AND
            restid.DELNR = SUMTIDDAG.DELNR USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE restid THEN CREATE restid.
            ASSIGN          
            restid.AONR = SUMTIDDAG.AONR
            restid.DELNR = SUMTIDDAG.DELNR 
            restid.TIMMAR = restid.TIMMAR + SUMTIDDAG.TIMMAR.  
            CREATE dagtemp.
            ASSIGN          
            dagtemp.AONR = SUMTIDDAG.AONR
            dagtemp.DELNR = SUMTIDDAG.DELNR 
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
            dagtemp.AONR = SUMTIDDAG.AONR
            dagtemp.DELNR = SUMTIDDAG.DELNR 
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
   FOR EACH dagtemp
   BREAK BY dagtemp.AONR BY dagtemp.DELNR:           
      ACCUMULATE 
      dagtemp.BELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR). 
      ACCUMULATE 
      dagtemp.TIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR). 
      ACCUMULATE 
      dagtemp.OTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR). 
      ACCUMULATE 
      dagtemp.OBELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR).  
      ACCUMULATE 
      dagtemp.TBELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR). 
      ACCUMULATE 
      dagtemp.LONKOST (TOTAL BY dagtemp.AONR BY dagtemp.DELNR).
      IF LAST-OF(dagtemp.DELNR) THEN DO TRANSACTION:
         CREATE slutsum.
         ASSIGN 
         slutsum.AONR = dagtemp.AONR
         slutsum.DELNR = dagtemp.DELNR          
         slutsum.BELOPP = (ACCUM TOTAL dagtemp.BELOPP) - arrhjsum                       
         slutsum.TIMMAR = (ACCUM TOTAL dagtemp.TIMMAR) - arrhjsumtid 
         slutsum.OTIMMAR = (ACCUM TOTAL dagtemp.OTIMMAR) - arrhjsumotid
         slutsum.OBELOPP = (ACCUM TOTAL dagtemp.OBELOPP) - arrhjsumove    
         slutsum.TBELOPP = (ACCUM TOTAL dagtemp.TBELOPP) - arrhjsumtra 
         slutsum.LONKOST = (ACCUM TOTAL dagtemp.LONKOST) - arrhjsumlon.            
         arrhjsum = ACCUM TOTAL dagtemp.BELOPP.  
         arrhjsumtid = ACCUM TOTAL dagtemp.TIMMAR. 
         arrhjsumotid = ACCUM TOTAL dagtemp.OTIMMAR.
         arrhjsumove = ACCUM TOTAL dagtemp.OBELOPP.  
         arrhjsumtra = ACCUM TOTAL dagtemp.TBELOPP.       
         arrhjsumlon = ACCUM TOTAL dagtemp.LONKOST.               
      END.     
   END.                
   IF bytao = FALSE THEN DO:
   
      ASSIGN   
      arrhjsum = 0   
      arrhjsumtid = 0.   
      FOR EACH slutsum USE-INDEX AONR NO-LOCK:  
         FIND FIRST restid WHERE restid.AONR = slutsum.AONR AND
         restid.DELNR = slutsum.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.   
         CREATE tidut.                                 
         ASSIGN   
         SUBSTRING(tidut.UT,1) = slutsum.AONR   
         SUBSTRING(tidut.UT,8) = STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1])                                                       
         SUBSTRING(tidut.UT,12) = STRING(slutsum.TIMMAR,">>>>>>99")   
         SUBSTRING(tidut.UT,21) = STRING(slutsum.BELOPP,">>>>>>>99")  
         SUBSTRING(tidut.UT,31) = STRING(slutsum.OTIMMAR,">>>>>>99")   
         SUBSTRING(tidut.UT,40) = STRING(slutsum.OBELOPP,">>>>>>>99")
         SUBSTRING(tidut.UT,59) = STRING(slutsum.TBELOPP,">>>>>>>99")
         SUBSTRING(tidut.UT,69) = STRING(slutsum.LONKOST,"->>>>>>99").
         IF AVAILABLE restid THEN DO:                                 
            ASSIGN                                         
            SUBSTRING(tidut.UT,50) = STRING(restid.TIMMAR,">>>>>>99")
            arrhjsumtid = arrhjsumtid + restid.TIMMAR.         
         END.
         ASSIGN           
         arrhjsum =                                
         arrhjsum + slutsum.BELOPP + slutsum.OBELOPP + slutsum.TBELOPP + slutsum.LONKOST
         arrhjsumtid = arrhjsumtid + slutsum.TIMMAR + slutsum.OTIMMAR.    
          
      END.   
      CREATE tidut.
      CREATE tidut. 
      CREATE tidut. 
      ASSIGN tidut.UT = str2.
      CREATE tidut. 
      CREATE tidut.   
      ASSIGN tidut.UT = "TOTALT".        
      CREATE tidut.  
      CREATE tidut.    
      ASSIGN                   
      SUBSTRING(tidut.UT,1) = "TOTALT ANTAL TIMMAR :"    
      SUBSTRING(tidut.UT,23) = STRING(arrhjsumtid,">>>>>>99").
      CREATE tidut.
      ASSIGN                   
      SUBSTRING(tidut.UT,1) = "TOTAL KOSTNAD       :"    
      SUBSTRING(tidut.UT,23) = STRING(arrhjsum + indertemp.INDEREKT,">>>>>>99").
      CREATE tidut.   
      CREATE tidut. 
      ASSIGN tidut.UT = str2.      
      IF bytaonrmed NE "" THEN DO:
         CREATE tidut.
         CREATE tidut.      
         ASSIGN       
         SUBSTRING(tidut.UT,1) = Guru.Konstanter:gaok + " " + bytaonrmed .                                         
      END.
   END.
END PROCEDURE.
