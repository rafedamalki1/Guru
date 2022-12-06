
/*------------------------------------------------------------------------
    File        : DIRAPP.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Jun 27 11:23:31 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/
{BOLAGSEKSTART.I}
DEFINE QUERY dagsumq FOR SUMTIDDAG. 
 
DEFINE QUERY ejlonq FOR SUMEJLON.
PROCEDURE anvglob_UI :
   DEFINE INPUT PARAMETER anvglob AS CHARACTER NO-UNDO.
   Guru.Konstanter:globanv = anvglob.
END PROCEDURE.
PROCEDURE aophuvud_UI :
   DEFINE INPUT PARAMETE aonummer AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnummer AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER valdelnrlog AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER inder AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER gforetag AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   EMPTY TEMP-TABLE konttemp NO-ERROR. 
   EMPTY TEMP-TABLE ejlontemp NO-ERROR. 
   EMPTY TEMP-TABLE eko NO-ERROR. 
   EMPTY TEMP-TABLE eko23 NO-ERROR. 
   EMPTY TEMP-TABLE dagtemp NO-ERROR. 
   EMPTY TEMP-TABLE indertemp NO-ERROR. 
   EMPTY TEMP-TABLE slutsum NO-ERROR. 
   EMPTY TEMP-TABLE slutsum1 NO-ERROR. 
   EMPTY TEMP-TABLE slutsum2 NO-ERROR. 
   EMPTY TEMP-TABLE restid NO-ERROR. 
   EMPTY TEMP-TABLE restid2 NO-ERROR. 
   EMPTY TEMP-TABLE restid3 NO-ERROR. 
   EMPTY TEMP-TABLE tidvnrtemp NO-ERROR. 
   
   DO TRANSACTION:   
      IF RAD_PERIOD = 1 THEN DO:          
        
         
      END. 
      IF RAD_PERIOD = 2 THEN DO:
         IF valdelnrlog = FALSE THEN DO: 
            FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND 
            SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = aonummer AND SUMTIDDAG.DELNR = delnummer 
            USE-INDEX AONR NO-LOCK NO-ERROR.
         END.
         ELSE DO: 
            FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND 
            SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = aonummer  
            USE-INDEX AONR NO-LOCK NO-ERROR.
         END.   
         IF NOT AVAILABLE SUMTIDDAG THEN DO:
            /*IF skrivut = FALSE THEN*/
            CREATE felmeddtemp.
            ASSIGN felmeddtemp.FELMEDD = "Det finns ingen tid skriven på " + LC(Guru.Konstanter:gaok) + aonummer + STRING(delnummer,Guru.Konstanter:varforetypchar[1]).
            RETURN.
         END.
      END.
      IF RAD_PERIOD = 3 THEN DO:
         IF valdelnrlog = FALSE THEN DO: 
            FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.AONR = aonummer AND SUMTIDDAG.DELNR = delnummer 
            USE-INDEX AONR NO-LOCK NO-ERROR.
         END.
         ELSE DO: 
            FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.AONR = aonummer  
            USE-INDEX AONR NO-LOCK NO-ERROR.
         END.   
         IF NOT AVAILABLE SUMTIDDAG THEN DO:
            /*IF skrivut = FALSE THEN*/
            CREATE felmeddtemp.
            ASSIGN felmeddtemp.FELMEDD = "Det finns ingen tid skriven på " + LC(Guru.Konstanter:gaok) + aonummer + STRING(delnummer,Guru.Konstanter:varforetypchar[1]).
            RETURN.      
         END.
      END.
      IF musz = TRUE THEN musz = musz.   
      ELSE DO:                          
         IF Guru.Konstanter:globforetag = "NORD" THEN DO:
            FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR = aonummer  AND 
            AONRKONTKOD.DELNR = delnummer USE-INDEX AONRKONT NO-LOCK NO-ERROR. 
            IF AVAILABLE AONRKONTKOD THEN DO:
               IF TRIM(AONRKONTKOD.K3) = "0" THEN inder = FALSE.              
               ELSE inder = TRUE.              
            END.   
         END.               
         IF inder = TRUE THEN DO:
            str =                                                                    
"==========.======.===========.======.=======.======.=======.====.=======.=======.=======".      
            str2 =
"----------------------------------------------------------------------------------------".              
         END.
         ELSE DO: 
            str =                                                                   
"==========.======.===========.======.=======.======.=======.====.=======.=======".
            str2 = 
"---------------------------------------------------------------------------------".             
         END.
         CREATE tidut. 
         SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").           
         CREATE tidut.
         CREATE tidut.                                       
         tidut.UT = CAPS(Guru.Konstanter:gaol)+ "-PERSONAL     PERIOD".
         IF RAD_PERIOD = 1 THEN DO: 
            SUBSTRING(tidut.UT,45) = STRING(YEAR(bdatum),"9999").
         END.
         IF RAD_PERIOD = 2 THEN DO:
            SUBSTRING(tidut.UT,45) = STRING(bdatum) + " - " + STRING(avdatum).     
         END.                                                                   
         IF valdelnrlog = TRUE THEN DO:           
            CREATE tidut.  
            ASSIGN SUBSTRING(tidut.UT,1) = "SUMMERAT PÅ HUVUDNUMMRET".
         END.
         
         CREATE tidut.  
         CREATE tidut.
         ASSIGN                                  
         SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrk) + ":".
                          
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = SUMTIDDAG.GEOMRADE
         USE-INDEX OMR NO-LOCK NO-ERROR.
         IF AVAILABLE OMRADETAB THEN DO:            
            ASSIGN                            
            SUBSTRING(tidut.UT,9) = OMRADETAB.NAMN.  
         END.
         ELSE SUBSTRING(tidut.UT,9) = CAPS(Guru.Konstanter:gaok) + " UTAN ORGANISATIONSTILLHÖRIGHET".    
                            
         CREATE tidut.
         CREATE tidut.            
         ASSIGN                                  
         SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok) +  ":".
         
         ASSIGN                             
         SUBSTRING(tidut.UT,9) = SUMTIDDAG.AONR 
         SUBSTRING(tidut.UT,21) = SUMTIDDAG.ORT.      
         IF valdelnrlog = FALSE THEN 
         SUBSTRING(tidut.UT,16) = STRING(SUMTIDDAG.DELNR,Guru.Konstanter:varforetypchar[1]). 
         IF valdelnrlog = TRUE THEN DO: 
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = aonummer NO-LOCK NO-ERROR. 
            IF AVAILABLE AONRTAB THEN SUBSTRING(tidut.UT,21) = AONRTAB.ORT. 
         END.                    
                          
         CREATE tidut.
         CREATE tidut.                                 
         ASSIGN       
         SUBSTRING(tidut.UT,12) = "PERS."             
         SUBSTRING(tidut.UT,19) = "ENHET/ "   
         SUBSTRING(tidut.UT,31) = "ARB."    
         SUBSTRING(tidut.UT,38) = "ARBETS"                                    
         SUBSTRING(tidut.UT,46) = "ÖVER."
         SUBSTRING(tidut.UT,53) = "ÖVERTID"
         SUBSTRING(tidut.UT,61) = "RES."
         SUBSTRING(tidut.UT,66) = "TRAKT."                          
         SUBSTRING(tidut.UT,74) = "L-TILL".
         IF inder = TRUE THEN             
         ASSIGN SUBSTRING(tidut.UT,82) = "INDIR.".            
         CREATE tidut.                                 
         ASSIGN                                
         SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)           
         SUBSTRING(tidut.UT,12) = CAPS(Guru.Konstanter:gomrk)                                         
         SUBSTRING(tidut.UT,19) = "SIGN NAMN"                                    
         SUBSTRING(tidut.UT,31) = "TIMMAR"    
         SUBSTRING(tidut.UT,38) = "KOSTNAD"                                    
         SUBSTRING(tidut.UT,46) = "TIMMAR"
         SUBSTRING(tidut.UT,53) = "KOSTNAD"
         SUBSTRING(tidut.UT,61) = "TIM."
         SUBSTRING(tidut.UT,66) = "KOSTNAD"  
         SUBSTRING(tidut.UT,74) = "KOSTNAD".
         IF inder = TRUE THEN             
         ASSIGN SUBSTRING(tidut.UT,82) = "KOSTNAD".                                 
         CREATE tidut.       
         SUBSTRING(tidut.UT,1) = str.             
      END.
   END.
END PROCEDURE.


PROCEDURE aopkonthuvud_UI :
   DEFINE INPUT PARAMETER TABLE FOR aoval.
   DEFINE INPUT PARAMETER sokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER valdelnrlog AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER inder AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER gforetag AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   ASSIGN Guru.Konstanter:globforetag = gforetag.
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   EMPTY TEMP-TABLE konttemp NO-ERROR. 
   EMPTY TEMP-TABLE ejlontemp NO-ERROR. 
   EMPTY TEMP-TABLE eko NO-ERROR. 
   EMPTY TEMP-TABLE eko23 NO-ERROR. 
   EMPTY TEMP-TABLE dagtemp NO-ERROR. 
   EMPTY TEMP-TABLE indertemp NO-ERROR. 
   EMPTY TEMP-TABLE slutsum NO-ERROR. 
   EMPTY TEMP-TABLE slutsum1 NO-ERROR. 
   EMPTY TEMP-TABLE slutsum2 NO-ERROR. 
   EMPTY TEMP-TABLE restid NO-ERROR. 
   EMPTY TEMP-TABLE restid2 NO-ERROR. 
   EMPTY TEMP-TABLE restid3 NO-ERROR. 
   EMPTY TEMP-TABLE tidvnrtemp NO-ERROR. 
   
   DO TRANSACTION:         
      musz = TRUE.
      IF RAD_PERIOD = 2 THEN DO:         
         FOR EACH aoval:
            IF valdelnrlog = FALSE THEN DO: 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND 
               SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = aoval.AONR AND SUMTIDDAG.DELNR = aoval.DELNR 
               USE-INDEX AONR NO-LOCK NO-ERROR.
            END.
            ELSE DO: 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND 
               SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = aoval.AONR  
               USE-INDEX AONR NO-LOCK NO-ERROR.
            END.   
            IF AVAILABLE SUMTIDDAG THEN musz = FALSE.
         END.
      END.
      IF RAD_PERIOD = 3 THEN DO:
         FOR EACH aoval:
            IF valdelnrlog = FALSE THEN DO: 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.AONR = aoval.AONR AND SUMTIDDAG.DELNR = aoval.DELNR 
               USE-INDEX AONR NO-LOCK NO-ERROR.
            END.
            ELSE DO: 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.AONR = aoval.AONR  
               USE-INDEX AONR NO-LOCK NO-ERROR.
            END.   
            IF AVAILABLE SUMTIDDAG THEN musz = FALSE.
            
         END.
      END.
      
      IF musz = TRUE THEN DO:
         musz = FALSE.                     
         CREATE felmeddtemp.
         ASSIGN felmeddtemp.FELMEDD = "Det finns ingen tid skriven på sökt konto" .
         RETURN.               
      END.
      IF musz = TRUE THEN musz = musz.   
      ELSE DO:                          
         str =                                                                   
"==========.======.===========.======.=======.======.=======.====.=======.=======".
         str2 = 
"---------------------------------------------------------------------------------".                      
         CREATE tidut. 
         SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").           
         CREATE tidut.
         
         CREATE tidut.                                       
         tidut.UT = CAPS(Guru.Konstanter:gaol)+ "-PERSONAL     PERIOD".
         IF RAD_PERIOD = 1 THEN DO: 
            SUBSTRING(tidut.UT,45) = STRING(YEAR(bdatum),"9999").
         END.
         IF RAD_PERIOD = 2 THEN DO:
            SUBSTRING(tidut.UT,45) = STRING(bdatum) + " - " + STRING(avdatum).     
         END.           
         CREATE tidut.  
         CREATE tidut.  
         FIND FIRST KBENAMNING USE-INDEX KBEN NO-LOCK NO-ERROR.
         ASSIGN SUBSTRING(tidut.UT,1) = "Utsökt på " + CAPS(SUBSTRING(KBENAMNING.K2,1,1)) + LC(SUBSTRING(KBENAMNING.K2,2)). .
         SUBSTRING(tidut.UT,18) = sokkto.
         CREATE tidut.  
         CREATE tidut.  
         ASSIGN SUBSTRING(tidut.UT,1) = "Ingående " + CAPS(Guru.Konstanter:gaok) + " :".
         FOR EACH aoval:
            CREATE tidut.  
            ASSIGN                                 
            SUBSTRING(tidut.UT,1) = aoval.AONR 
            SUBSTRING(tidut.UT,8) = STRING(aoval.DELNR).
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = aoval.AONR AND AONRTAB.DELNR = aoval.DELNR  NO-LOCK NO-ERROR.
            SUBSTRING(tidut.UT,12) = AONRTAB.OMRADE.
            SUBSTRING(tidut.UT,18) = AONRTAB.ORT.            
            IF INDEX(SUBSTRING(sokkto,1,10),"*") = 0 THEN DO:
               fkto = 1.
               FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR =  aoval.AONR AND
               AONRKONTKOD.DELNR =  aoval.DELNR AND AONRKONTKOD.K2 = sokkto NO-LOCK:               
                  IF fkto > 1 THEN CREATE tidut.  
                  SUBSTRING(tidut.UT,48) = AONRKONTKOD.K2.
                  SUBSTRING(tidut.UT,55) = STRING(AONRKONTKOD.SATS%).                  
                  fkto = fkto + 1.
               END.
            END.
            ELSE DO:    
               fkto = 1.
               utkonto = SUBSTRING(sokkto,1,INDEX(SUBSTRING(sokkto,1,10),"*") - 1).
               FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR =  aoval.AONR AND
               AONRKONTKOD.DELNR =  aoval.DELNR AND AONRKONTKOD.K2 BEGINS utkonto NO-LOCK:
                  IF fkto > 1 THEN CREATE tidut.  
                  SUBSTRING(tidut.UT,48) = AONRKONTKOD.K2.
                  SUBSTRING(tidut.UT,55) = STRING(AONRKONTKOD.SATS%).                  
                  fkto = fkto + 1.
               END.
            END.
            

         END.
         
         CREATE tidut.
         CREATE tidut.                                 
         ASSIGN       
         SUBSTRING(tidut.UT,12) = "PERS."             
         SUBSTRING(tidut.UT,19) = "ENHET/ "   
         SUBSTRING(tidut.UT,31) = "ARB."    
         SUBSTRING(tidut.UT,38) = "ARBETS"                                    
         SUBSTRING(tidut.UT,46) = "ÖVER."
         SUBSTRING(tidut.UT,53) = "ÖVERTID"
         SUBSTRING(tidut.UT,61) = "RES."
         SUBSTRING(tidut.UT,66) = "TRAKT."                          
         SUBSTRING(tidut.UT,74) = "L-TILL".
         IF inder = TRUE THEN             
         ASSIGN SUBSTRING(tidut.UT,82) = "INDIR.".   
         
         CREATE tidut.                                 
         ASSIGN                                
         /*SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)           */
         SUBSTRING(tidut.UT,12) = CAPS(Guru.Konstanter:gomrk)                                         
         SUBSTRING(tidut.UT,19) = "SIGN NAMN"                                    
         SUBSTRING(tidut.UT,31) = "TIMMAR"    
         SUBSTRING(tidut.UT,38) = "KOSTNAD"                                    
         SUBSTRING(tidut.UT,46) = "TIMMAR"
         SUBSTRING(tidut.UT,53) = "KOSTNAD"
         SUBSTRING(tidut.UT,61) = "TIM."
         SUBSTRING(tidut.UT,66) = "KOSTNAD"  
         SUBSTRING(tidut.UT,74) = "KOSTNAD".
         IF inder = TRUE THEN             
         ASSIGN SUBSTRING(tidut.UT,82) = "KOSTNAD".                                 
         CREATE tidut.       
         SUBSTRING(tidut.UT,1) = str.             
      END.
   END.
END PROCEDURE.

PROCEDURE aopkonthuvudoverst_UI :   
   DEFINE INPUT PARAMETER sokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut. 
   
   CREATE tidut. 
   SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").           
   CREATE tidut.
   CREATE tidut.
                                 
   tidut.UT = CAPS(Guru.Konstanter:gaol)+ "-PERSONAL     PERIOD".
   IF RAD_PERIOD = 1 THEN DO: 
      SUBSTRING(tidut.UT,45) = STRING(YEAR(bdatum),"9999").
   END.
   IF RAD_PERIOD = 2 THEN DO:
      SUBSTRING(tidut.UT,45) = STRING(bdatum) + " - " + STRING(avdatum).     
   END.           
   CREATE tidut.  
   CREATE tidut.  
   FIND FIRST KBENAMNING USE-INDEX KBEN NO-LOCK NO-ERROR.
   ASSIGN SUBSTRING(tidut.UT,1) = "Utsökt på " + CAPS(SUBSTRING(KBENAMNING.K2,1,1)) + LC(SUBSTRING(KBENAMNING.K2,2)). .
   SUBSTRING(tidut.UT,18) = sokkto.
   CREATE tidut.  

END PROCEDURE.

PROCEDURE aopkonthuvudoverst2_UI :   
   DEFINE INPUT PARAMETER sokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER lista AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER TABLE FOR tidut. 
   
   CREATE tidut. 
   SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").           
   CREATE tidut.
   CREATE tidut.                              
   tidut.UT = lista.
   IF RAD_PERIOD = 1 THEN DO: 
      SUBSTRING(tidut.UT,45) = STRING(YEAR(bdatum),"9999").
   END.
   IF RAD_PERIOD = 2 THEN DO:
      SUBSTRING(tidut.UT,45) = STRING(bdatum) + " - " + STRING(avdatum).     
   END.           
   CREATE tidut.  
   CREATE tidut.  
   FIND FIRST KBENAMNING USE-INDEX KBEN NO-LOCK NO-ERROR.
   ASSIGN SUBSTRING(tidut.UT,1) = "Utsökt på " + CAPS(SUBSTRING(KBENAMNING.K2,1,1)) + LC(SUBSTRING(KBENAMNING.K2,2)). .
   SUBSTRING(tidut.UT,18) = sokkto.
   CREATE tidut.  

END PROCEDURE.



PROCEDURE aopkonthuvud2_UI :
   DEFINE INPUT PARAMETER TABLE FOR aoval.
   DEFINE INPUT PARAMETER sokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER hsokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER valdelnrlog AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER inder AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER gforetag AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut. 
   ASSIGN Guru.Konstanter:globforetag = gforetag.
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   /*EMPTY TEMP-TABLE tidut NO-ERROR. */
   EMPTY TEMP-TABLE konttemp NO-ERROR. 
   EMPTY TEMP-TABLE ejlontemp NO-ERROR. 
   EMPTY TEMP-TABLE eko NO-ERROR. 
   EMPTY TEMP-TABLE eko23 NO-ERROR. 
   EMPTY TEMP-TABLE dagtemp NO-ERROR. 
   EMPTY TEMP-TABLE indertemp NO-ERROR. 
   EMPTY TEMP-TABLE slutsum NO-ERROR. 
   EMPTY TEMP-TABLE slutsum1 NO-ERROR. 
   EMPTY TEMP-TABLE slutsum2 NO-ERROR. 
   EMPTY TEMP-TABLE restid NO-ERROR. 
   EMPTY TEMP-TABLE restid2 NO-ERROR. 
   EMPTY TEMP-TABLE restid3 NO-ERROR. 
   EMPTY TEMP-TABLE tidvnrtemp NO-ERROR. 
   
   DO TRANSACTION:         
      musz = FALSE.
      IF RAD_PERIOD = 2 THEN DO:         
         FOR EACH aoval:
            IF valdelnrlog = FALSE THEN DO: 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND 
               SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = aoval.AONR AND SUMTIDDAG.DELNR = aoval.DELNR 
               USE-INDEX AONR NO-LOCK NO-ERROR.
            END.
            ELSE DO: 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND 
               SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = aoval.AONR  
               USE-INDEX AONR NO-LOCK NO-ERROR.
            END.   
            IF AVAILABLE SUMTIDDAG THEN musz = FALSE.
         END.
      END.
      IF RAD_PERIOD = 3 THEN DO:
         FOR EACH aoval:
            IF valdelnrlog = FALSE THEN DO: 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.AONR = aoval.AONR AND SUMTIDDAG.DELNR = aoval.DELNR 
               USE-INDEX AONR NO-LOCK NO-ERROR.
            END.
            ELSE DO: 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.AONR = aoval.AONR  
               USE-INDEX AONR NO-LOCK NO-ERROR.
            END.   
            IF AVAILABLE SUMTIDDAG THEN musz = FALSE.
            
         END.
      END.
      
      DO:                          
         str =                                                                   
"==========.======.===========.======.=======.======.=======.====.=======.=======".
         str2 = 
"--------------------------------------------------------------------------------".                      
         CREATE tidut.  
         CREATE tidut.  
         FIND FIRST KBENAMNING USE-INDEX KBEN NO-LOCK NO-ERROR.         
         ASSIGN SUBSTRING(tidut.UT,1) = "Ingående " + CAPS(Guru.Konstanter:gaok) + " för " + CAPS(SUBSTRING(KBENAMNING.K2,1,1)) + LC(SUBSTRING(KBENAMNING.K2,2)) + " " + hsokkto + " :".
         FOR EACH aoval:
            CREATE tidut.  
            ASSIGN                                 
            SUBSTRING(tidut.UT,1) = aoval.AONR 
            SUBSTRING(tidut.UT,8) = STRING(aoval.DELNR).
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = aoval.AONR AND AONRTAB.DELNR = aoval.DELNR  NO-LOCK NO-ERROR.
            SUBSTRING(tidut.UT,12) = AONRTAB.OMRADE.
            SUBSTRING(tidut.UT,18) = AONRTAB.ORT.                  
            fkto = 1.
            FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR =  aoval.AONR AND
            AONRKONTKOD.DELNR =  aoval.DELNR AND AONRKONTKOD.K2 = hsokkto NO-LOCK:               
               IF fkto > 1 THEN CREATE tidut.  
               SUBSTRING(tidut.UT,48) = AONRKONTKOD.K2.
               SUBSTRING(tidut.UT,55) = STRING(AONRKONTKOD.SATS%).                  
               fkto = fkto + 1.
            END.            
         END.
         CREATE tidut.
         CREATE tidut.                                 
         ASSIGN       
         SUBSTRING(tidut.UT,12) = "PERS."             
         SUBSTRING(tidut.UT,19) = "ENHET/ "   
         SUBSTRING(tidut.UT,31) = "ARB."    
         SUBSTRING(tidut.UT,38) = "ARBETS"                                    
         SUBSTRING(tidut.UT,46) = "ÖVER."
         SUBSTRING(tidut.UT,53) = "ÖVERTID"
         SUBSTRING(tidut.UT,61) = "RES."
         SUBSTRING(tidut.UT,66) = "TRAKT."                          
         SUBSTRING(tidut.UT,74) = "L-TILL".
         IF inder = TRUE THEN             
         ASSIGN SUBSTRING(tidut.UT,82) = "INDIR.".   
         ELSE IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO: 
            ASSIGN SUBSTRING(tidut.UT,66) = " VARAV".                        
         END.
         CREATE tidut.                                 
         ASSIGN                                
         /*SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)           */
         SUBSTRING(tidut.UT,12) = CAPS(Guru.Konstanter:gomrk)                                         
         SUBSTRING(tidut.UT,19) = "SIGN NAMN"                                    
         SUBSTRING(tidut.UT,31) = "TIMMAR"    
         SUBSTRING(tidut.UT,38) = "KOSTNAD"                                    
         SUBSTRING(tidut.UT,46) = "TIMMAR"
         SUBSTRING(tidut.UT,53) = "KOSTNAD"
         SUBSTRING(tidut.UT,61) = "TIM."
         SUBSTRING(tidut.UT,66) = "KOSTNAD"  
         SUBSTRING(tidut.UT,74) = "KOSTNAD".
         IF inder = TRUE THEN             
         ASSIGN SUBSTRING(tidut.UT,82) = "KOSTNAD".                        
         ELSE IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO: 
            ASSIGN SUBSTRING(tidut.UT,66) = " ÖT.BE.".                        
         END.
         CREATE tidut.       
         SUBSTRING(tidut.UT,1) = str.             
      END.
   END.
END PROCEDURE.

PROCEDURE aopkonthuvud3_UI :
   DEFINE INPUT PARAMETER TABLE FOR aoval.
   DEFINE INPUT PARAMETER sokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER hsokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER valdelnrlog AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER inder AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER gforetag AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut. 
   ASSIGN Guru.Konstanter:globforetag = gforetag.
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   /*EMPTY TEMP-TABLE tidut NO-ERROR. */
   EMPTY TEMP-TABLE konttemp NO-ERROR. 
   EMPTY TEMP-TABLE ejlontemp NO-ERROR. 
   EMPTY TEMP-TABLE eko NO-ERROR. 
   EMPTY TEMP-TABLE eko23 NO-ERROR. 
   EMPTY TEMP-TABLE dagtemp NO-ERROR. 
   EMPTY TEMP-TABLE indertemp NO-ERROR. 
   EMPTY TEMP-TABLE slutsum NO-ERROR. 
   EMPTY TEMP-TABLE slutsum1 NO-ERROR. 
   EMPTY TEMP-TABLE slutsum2 NO-ERROR. 
   EMPTY TEMP-TABLE restid NO-ERROR. 
   EMPTY TEMP-TABLE restid2 NO-ERROR. 
   EMPTY TEMP-TABLE restid3 NO-ERROR. 
   EMPTY TEMP-TABLE tidvnrtemp NO-ERROR. 
   
   DO TRANSACTION:         
      musz = FALSE.
      IF RAD_PERIOD = 2 THEN DO:         
         FOR EACH aoval:
            IF valdelnrlog = FALSE THEN DO: 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND 
               SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = aoval.AONR AND SUMTIDDAG.DELNR = aoval.DELNR 
               USE-INDEX AONR NO-LOCK NO-ERROR.
            END.
            ELSE DO: 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.DATUM >= bdatum AND 
               SUMTIDDAG.DATUM <= avdatum AND SUMTIDDAG.AONR = aoval.AONR  
               USE-INDEX AONR NO-LOCK NO-ERROR.
            END.   
            IF AVAILABLE SUMTIDDAG THEN musz = FALSE.
         END.
      END.
      IF RAD_PERIOD = 3 THEN DO:
         FOR EACH aoval:
            IF valdelnrlog = FALSE THEN DO: 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.AONR = aoval.AONR AND SUMTIDDAG.DELNR = aoval.DELNR 
               USE-INDEX AONR NO-LOCK NO-ERROR.
            END.
            ELSE DO: 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.AONR = aoval.AONR  
               USE-INDEX AONR NO-LOCK NO-ERROR.
            END.   
            IF AVAILABLE SUMTIDDAG THEN musz = FALSE.
            
         END.
      END.
      
      DO:                          
         str =                                                                   
"==========.======.===========.======.=======.======.=======.====.=======.=======".
         str2 = 
"--------------------------------------------------------------------------------".                      
         CREATE tidut.  
         CREATE tidut.  
         
         FIND FIRST KBENAMNING USE-INDEX KBEN NO-LOCK NO-ERROR.         
         FIND FIRST aoval NO-LOCK NO-ERROR.
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = aoval.AONR AND AONRTAB.DELNR = aoval.DELNR  NO-LOCK NO-ERROR.
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
         FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
         FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
         FIND FIRST KONTO WHERE KONTO.JUDID = JURPERS.JUDID AND KONTO.KONTO = "K2" AND
         KONTO.KONTONR = hsokkto NO-LOCK NO-ERROR.
         IF AVAILABLE KONTO THEN DO:
            ASSIGN SUBSTRING(tidut.UT,1) = CAPS(SUBSTRING(KBENAMNING.K2,1,1)) + LC(SUBSTRING(KBENAMNING.K2,2)) + " " + hsokkto + " " + KONTO.BENAMNING.
         END.
         ELSE DO:
            ASSIGN SUBSTRING(tidut.UT,1) = CAPS(SUBSTRING(KBENAMNING.K2,1,1)) + LC(SUBSTRING(KBENAMNING.K2,2)) + " " + hsokkto .
         END.
         
         /*ASSIGN SUBSTRING(tidut.UT,1) = "Ingående " + CAPS(Guru.Konstanter:gaok) + " för " + CAPS(SUBSTRING(KBENAMNING.K2,1,1)) + LC(SUBSTRING(KBENAMNING.K2,2)) + " " + hsokkto + " :".
         FOR EACH aoval:
            CREATE tidut.  
            ASSIGN                                 
            SUBSTRING(tidut.UT,1) = aoval.AONR 
            SUBSTRING(tidut.UT,8) = STRING(aoval.DELNR).
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = aoval.AONR AND AONRTAB.DELNR = aoval.DELNR  NO-LOCK NO-ERROR.
            SUBSTRING(tidut.UT,12) = AONRTAB.OMRADE.
            SUBSTRING(tidut.UT,18) = AONRTAB.ORT.                  
            fkto = 1.
            FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR =  aoval.AONR AND
            AONRKONTKOD.DELNR =  aoval.DELNR AND AONRKONTKOD.K2 = hsokkto NO-LOCK:               
               IF fkto > 1 THEN CREATE tidut.  
               SUBSTRING(tidut.UT,48) = AONRKONTKOD.K2.
               SUBSTRING(tidut.UT,55) = STRING(AONRKONTKOD.SATS%).                  
               fkto = fkto + 1.
            END.            
         END.*/
         CREATE tidut.
         CREATE tidut.                                 
         ASSIGN       
         SUBSTRING(tidut.UT,12) = "PERS."             
         SUBSTRING(tidut.UT,19) = "ENHET/ "   
         SUBSTRING(tidut.UT,31) = "ARB."    
         SUBSTRING(tidut.UT,38) = "ARBETS"                                    
         SUBSTRING(tidut.UT,46) = "ÖVER."
         SUBSTRING(tidut.UT,53) = "ÖVERTID"
         SUBSTRING(tidut.UT,61) = "RES."
         SUBSTRING(tidut.UT,66) = "TRAKT."                          
         SUBSTRING(tidut.UT,74) = "L-TILL".
         IF inder = TRUE THEN             
         ASSIGN SUBSTRING(tidut.UT,82) = "INDIR.".   
         ELSE IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO: 
            ASSIGN SUBSTRING(tidut.UT,66) = " VARAV".                        
         END.
         CREATE tidut.                                 
         ASSIGN                                
         /*SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)           */
         SUBSTRING(tidut.UT,12) = CAPS(Guru.Konstanter:gomrk)                                         
         SUBSTRING(tidut.UT,19) = "SIGN NAMN"                                    
         SUBSTRING(tidut.UT,31) = "TIMMAR"    
         SUBSTRING(tidut.UT,38) = "KOSTNAD"                                    
         SUBSTRING(tidut.UT,46) = "TIMMAR"
         SUBSTRING(tidut.UT,53) = "KOSTNAD"
         SUBSTRING(tidut.UT,61) = "TIM."
         SUBSTRING(tidut.UT,66) = "KOSTNAD"  
         SUBSTRING(tidut.UT,74) = "KOSTNAD".
         IF inder = TRUE THEN             
         ASSIGN SUBSTRING(tidut.UT,82) = "KOSTNAD".                        
         ELSE IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO: 
            ASSIGN SUBSTRING(tidut.UT,66) = " ÖT.BE.".                        
         END.
         CREATE tidut.       
         SUBSTRING(tidut.UT,1) = str.             
      END.
   END.
END PROCEDURE.

PROCEDURE aopkonthuvud4_UI :
   DEFINE INPUT PARAMETER TABLE FOR aoval.
   DEFINE INPUT PARAMETER sokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER hsokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.   
   DEFINE INPUT PARAMETER gforetag AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut. 
   ASSIGN Guru.Konstanter:globforetag = gforetag.
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   /*EMPTY TEMP-TABLE tidut NO-ERROR. */
   EMPTY TEMP-TABLE konttemp NO-ERROR. 
   EMPTY TEMP-TABLE ejlontemp NO-ERROR. 
   EMPTY TEMP-TABLE eko NO-ERROR. 
   EMPTY TEMP-TABLE eko23 NO-ERROR. 
   EMPTY TEMP-TABLE dagtemp NO-ERROR. 
   EMPTY TEMP-TABLE indertemp NO-ERROR. 
   EMPTY TEMP-TABLE slutsum NO-ERROR. 
   EMPTY TEMP-TABLE slutsum1 NO-ERROR. 
   EMPTY TEMP-TABLE slutsum2 NO-ERROR. 
   EMPTY TEMP-TABLE restid NO-ERROR. 
   EMPTY TEMP-TABLE restid2 NO-ERROR. 
   EMPTY TEMP-TABLE restid3 NO-ERROR. 
   EMPTY TEMP-TABLE tidvnrtemp NO-ERROR. 
   
   DO TRANSACTION:         
      musz = FALSE.
      IF RAD_PERIOD = 2 THEN DO:         
         FOR EACH aoval:            
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND 
            TIDREGITAB.DATUM <= avdatum AND TIDREGITAB.AONR = aoval.AONR AND TIDREGITAB.DELNR = aoval.DELNR 
            AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.RESMAL NE "" AND TIDREGITAB.PRISTYP NE "RESTID.." USE-INDEX AONR NO-LOCK NO-ERROR.         
            IF AVAILABLE TIDREGITAB THEN musz = FALSE.
         END.
      END.
      IF RAD_PERIOD = 3 THEN DO:
         FOR EACH aoval:            
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.AONR = aoval.AONR AND TIDREGITAB.DELNR = aoval.DELNR 
            AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.RESMAL NE "" AND TIDREGITAB.PRISTYP NE "RESTID.." USE-INDEX AONR NO-LOCK NO-ERROR.           
            IF AVAILABLE TIDREGITAB THEN musz = FALSE.            
         END.
      END.
      
      DO:                          
         str =                                                                   
"==========.=====.=====.======.========.=====.=====.=====.============================================================.=====.".
         str2 = 
"--------------------------------------------------------------------------------".                      
         CREATE tidut.  
         CREATE tidut.  
         
         FIND FIRST KBENAMNING USE-INDEX KBEN NO-LOCK NO-ERROR.         
         FIND FIRST aoval NO-LOCK NO-ERROR.
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = aoval.AONR AND AONRTAB.DELNR = aoval.DELNR  NO-LOCK NO-ERROR.
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
         FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
         FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
         FIND FIRST KONTO WHERE KONTO.JUDID = JURPERS.JUDID AND KONTO.KONTO = "K2" AND
         KONTO.KONTONR = hsokkto NO-LOCK NO-ERROR.
         IF AVAILABLE KONTO THEN DO:
            ASSIGN SUBSTRING(tidut.UT,1) = CAPS(SUBSTRING(KBENAMNING.K2,1,1)) + LC(SUBSTRING(KBENAMNING.K2,2)) + " " + hsokkto + " " + KONTO.BENAMNING.
         END.
         ELSE DO:
            ASSIGN SUBSTRING(tidut.UT,1) = CAPS(SUBSTRING(KBENAMNING.K2,1,1)) + LC(SUBSTRING(KBENAMNING.K2,2)) + " " + hsokkto .
         END.                  
         CREATE tidut.
         CREATE tidut.                                 
         ASSIGN                
         SUBSTRING(tidut.UT,12) = "ENHET/ ".                      
         CREATE tidut.                                 
         ASSIGN                                
         SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)           
         SUBSTRING(tidut.UT,12) = "SIGN"                                         
         SUBSTRING(tidut.UT,18) = "NAMN"                                         
         SUBSTRING(tidut.UT,23) = CAPS(Guru.Konstanter:gomrk)                                                  
         SUBSTRING(tidut.UT,31) = "DATUM"    
         SUBSTRING(tidut.UT,40) = "START"                                    
         SUBSTRING(tidut.UT,46) = "SLUT"
         SUBSTRING(tidut.UT,53) = "TIM"
         SUBSTRING(tidut.UT,61) = "KOMMENTAR".
         SUBSTRING(tidut.UT,119) = "BEORDRARE".
         /*SUBSTRING(tidut.UT,89) = "BEORDRARE".*/
                 
         CREATE tidut.       
         SUBSTRING(tidut.UT,1) = str.             
      END.
   END.
END PROCEDURE.


PROCEDURE aopsumma_UI :
   /*körs inte längre lena 20180412*/
   DEFINE INPUT PARAMETER aonummer AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnummer AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER val1 AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER vallista AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER valdelnrlog AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER inder AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR visa.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   DEFINE VARIABLE vlista AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sealltid AS LOGICAL NO-UNDO.
   RUN kollsealltid_UI (INPUT Guru.Konstanter:globanv ,OUTPUT sealltid).
   
   vlista = CAPS(Guru.Konstanter:gaol)+ "-PERSONAL     PERIOD".
   
   FIND FIRST visa WHERE visa.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   FIND FIRST tidut  WHERE tidut.UT BEGINS vlista  NO-LOCK NO-ERROR.
   IF AVAILABLE tidut THEN DO:
      tidut.UT = visa.TYP + "- " + tidut.UT. 
   END.   
   {aopsummau.i}   
   IF bytaonrmed NE "" THEN DO:
      CREATE tidut.
      CREATE tidut.      
      ASSIGN       
      SUBSTRING(tidut.UT,1) = Guru.Konstanter:gaok + " " + bytaonrmed .                                         
   END.
   IF RAD_PERIOD = 2 AND val1 = TRUE THEN DO:    
      GET FIRST dagsumq NO-LOCK.
      IF AVAILABLE SUMTIDDAG THEN DO:
         CREATE tidut.
         CREATE tidut.
         CREATE tidut. 
         ASSIGN   
         SUBSTRING(tidut.UT,1,6) = "BILAGA". 
         CREATE tidut.
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "INGÅENDE TIDSEDLARS VECKONUMMER".
         CREATE tidut. 
         CREATE tidut.
         ASSIGN                        
         SUBSTRING(tidut.UT,1) = "ENHET/ "
         SUBSTRING(tidut.UT,12) = "TIMMAR".  
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "SIGN "  
         SUBSTRING(tidut.UT,8) = "VNR"
         SUBSTRING(tidut.UT,12) = "MÅN"
         SUBSTRING(tidut.UT,16) = "TIS"
         SUBSTRING(tidut.UT,20) = "ONS"
         SUBSTRING(tidut.UT,24) = "TOR"
         SUBSTRING(tidut.UT,28) = "FRE"
         SUBSTRING(tidut.UT,32) = "LÖR"
         SUBSTRING(tidut.UT,36) = "SÖN".
         CREATE tidut.
         ASSIGN                   
         SUBSTRING(tidut.UT,1) = "======.===.===.===.===.===.===.===.===".              
      END.  
      DO WHILE AVAILABLE(SUMTIDDAG):            
         FIND FIRST tidvnrtemp WHERE 
         tidvnrtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD AND
         tidvnrtemp.DATUM = SUMTIDDAG.DATUM 
         USE-INDEX PERSONALKOD NO-ERROR.
         IF NOT AVAILABLE tidvnrtemp THEN CREATE tidvnrtemp.
         ASSIGN
         tidvnrtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD
         tidvnrtemp.DATUM = SUMTIDDAG.DATUM
         tidvnrtemp.NAMN = SUMTIDDAG.FORNAMN + " " + SUMTIDDAG.EFTERNAMN.  
         IF WEEKDAY(SUMTIDDAG.DATUM) = 2 THEN 
         tidvnrtemp.MAN = tidvnrtemp.MAN +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 3 THEN 
         tidvnrtemp.TIS = tidvnrtemp.TIS +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 4 THEN 
         tidvnrtemp.ONS = tidvnrtemp.ONS +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 5 THEN 
         tidvnrtemp.TOR = tidvnrtemp.TOR +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 6 THEN 
         tidvnrtemp.FRE = tidvnrtemp.FRE +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 7 THEN 
         tidvnrtemp.LOR = tidvnrtemp.LOR +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 1 THEN 
         tidvnrtemp.SON = tidvnrtemp.SON +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         GET NEXT dagsumq NO-LOCK.                                               
      END.
      REPEAT:
         FIND FIRST tidvnrtemp WHERE tidvnrtemp.VECKONUMMER = 0
         USE-INDEX VNR NO-ERROR.
         IF AVAILABLE tidvnrtemp THEN DO:
            regdatum = tidvnrtemp.DATUM.
            RUN REGVEC.P.
            regdagnamn = "MÅN".
            RUN VECODAT.P.
            bvnrdatum = regdatum.
            regdagnamn = "SÖN".
            RUN VECODAT.P.
            avnrdatum = regdatum.
            ASSIGN
            tidvnrtemp.VECKONUMMER = regvnr.
         END.
         ELSE LEAVE.   
         FOR EACH tidvnrtemp WHERE tidvnrtemp.DATUM >= bvnrdatum AND
         tidvnrtemp.DATUM <= avnrdatum AND tidvnrtemp.VECKONUMMER = 0
         USE-INDEX DATUM:
            ASSIGN
            tidvnrtemp.VECKONUMMER = regvnr.
         END.   
      END.                              
      FOR EACH tidvnrtemp
      BREAK BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER: 
         ACCUMULATE 
         tidvnrtemp.MAN (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.TIS (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.ONS (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.TOR (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER).  
         ACCUMULATE 
         tidvnrtemp.FRE (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.LOR (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER).   
         ACCUMULATE 
         tidvnrtemp.SON (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER).
         IF LAST-OF(tidvnrtemp.VECKONUMMER) THEN DO TRANSACTION:
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidvnrtemp.PERSONALKOD  
            SUBSTRING(tidut.UT,8) = STRING(tidvnrtemp.VECKONUMMER,"999").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.MAN) > 0 THEN 
            SUBSTRING(tidut.UT,12,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.MAN),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TIS) > 0 THEN                       
            SUBSTRING(tidut.UT,16,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TIS),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.ONS) > 0 THEN 
            SUBSTRING(tidut.UT,20,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.ONS),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TOR) > 0 THEN
            SUBSTRING(tidut.UT,24,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TOR),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.FRE) > 0 THEN    
            SUBSTRING(tidut.UT,28,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.FRE),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.LOR) > 0 THEN 
            SUBSTRING(tidut.UT,32,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.LOR),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.SON) > 0 THEN
            SUBSTRING(tidut.UT,36,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.SON),">>9").                                        
         END.                                                   
      END.
   END.
END PROCEDURE.

PROCEDURE aopsummau_UI :
   DEFINE INPUT PARAMETER aonummer AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnummer AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER val1 AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER vallista AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER valdelnrlog AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER inder AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR visa.
   DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   
   DEFINE VARIABLE sealltid AS LOGICAL NO-UNDO.
   RUN kollsealltid_UI (INPUT Guru.Konstanter:globanv ,OUTPUT sealltid).
   
   DEFINE VARIABLE vlista AS CHARACTER NO-UNDO.
   FIND FIRST visa WHERE visa.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   
   vlista = CAPS(Guru.Konstanter:gaol)+ "-PERSONAL     PERIOD".   
   FIND FIRST tidut  WHERE tidut.UT BEGINS vlista  NO-LOCK NO-ERROR.
   IF AVAILABLE tidut THEN DO:
      tidut.UT = visa.TYP + "- " + tidut.UT. 
   END.   
   
   
   
   FIND FIRST uppvaltemp NO-LOCK NO-ERROR.
   {aopsummau2.i}
   IF bytaonrmed NE "" THEN DO:
      CREATE tidut.
      CREATE tidut.      
      ASSIGN       
      SUBSTRING(tidut.UT,1) = Guru.Konstanter:gaok + " " + bytaonrmed .                                         
   END.
   IF RAD_PERIOD = 2 AND val1 = TRUE THEN DO:    
      GET FIRST dagsumq NO-LOCK.
      IF AVAILABLE SUMTIDDAG THEN DO:
         CREATE tidut.
         CREATE tidut.
         CREATE tidut. 
         ASSIGN   
         SUBSTRING(tidut.UT,1,6) = "BILAGA". 
         CREATE tidut.
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "INGÅENDE TIDSEDLARS VECKONUMMER".
         CREATE tidut. 
         CREATE tidut.
         ASSIGN                        
         SUBSTRING(tidut.UT,1) = "ENHET/ "
         SUBSTRING(tidut.UT,12) = "TIMMAR".  
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "SIGN "  
         SUBSTRING(tidut.UT,8) = "VNR"
         SUBSTRING(tidut.UT,12) = "MÅN"
         SUBSTRING(tidut.UT,16) = "TIS"
         SUBSTRING(tidut.UT,20) = "ONS"
         SUBSTRING(tidut.UT,24) = "TOR"
         SUBSTRING(tidut.UT,28) = "FRE"
         SUBSTRING(tidut.UT,32) = "LÖR"
         SUBSTRING(tidut.UT,36) = "SÖN".
         CREATE tidut.
         ASSIGN                   
         SUBSTRING(tidut.UT,1) = "======.===.===.===.===.===.===.===.===".              
      END.  
      DO WHILE AVAILABLE(SUMTIDDAG):            
         FIND FIRST tidvnrtemp WHERE 
         tidvnrtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD AND
         tidvnrtemp.DATUM = SUMTIDDAG.DATUM 
         USE-INDEX PERSONALKOD NO-ERROR.
         IF NOT AVAILABLE tidvnrtemp THEN CREATE tidvnrtemp.
         ASSIGN
         tidvnrtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD
         tidvnrtemp.DATUM = SUMTIDDAG.DATUM
         tidvnrtemp.NAMN = SUMTIDDAG.FORNAMN + " " + SUMTIDDAG.EFTERNAMN.  
         IF WEEKDAY(SUMTIDDAG.DATUM) = 2 THEN 
         tidvnrtemp.MAN = tidvnrtemp.MAN +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 3 THEN 
         tidvnrtemp.TIS = tidvnrtemp.TIS +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 4 THEN 
         tidvnrtemp.ONS = tidvnrtemp.ONS +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 5 THEN 
         tidvnrtemp.TOR = tidvnrtemp.TOR +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 6 THEN 
         tidvnrtemp.FRE = tidvnrtemp.FRE +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 7 THEN 
         tidvnrtemp.LOR = tidvnrtemp.LOR +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 1 THEN 
         tidvnrtemp.SON = tidvnrtemp.SON +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         GET NEXT dagsumq NO-LOCK.                                               
      END.
      REPEAT:
         FIND FIRST tidvnrtemp WHERE tidvnrtemp.VECKONUMMER = 0
         USE-INDEX VNR NO-ERROR.
         IF AVAILABLE tidvnrtemp THEN DO:
            regdatum = tidvnrtemp.DATUM.
            RUN REGVEC.P.
            regdagnamn = "MÅN".
            RUN VECODAT.P.
            bvnrdatum = regdatum.
            regdagnamn = "SÖN".
            RUN VECODAT.P.
            avnrdatum = regdatum.
            ASSIGN
            tidvnrtemp.VECKONUMMER = regvnr.
         END.
         ELSE LEAVE.   
         FOR EACH tidvnrtemp WHERE tidvnrtemp.DATUM >= bvnrdatum AND
         tidvnrtemp.DATUM <= avnrdatum AND tidvnrtemp.VECKONUMMER = 0
         USE-INDEX DATUM:
            ASSIGN
            tidvnrtemp.VECKONUMMER = regvnr.
         END.   
      END.                              
      FOR EACH tidvnrtemp
      BREAK BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER: 
         ACCUMULATE 
         tidvnrtemp.MAN (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.TIS (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.ONS (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.TOR (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER).  
         ACCUMULATE 
         tidvnrtemp.FRE (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.LOR (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER).   
         ACCUMULATE 
         tidvnrtemp.SON (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER).
         IF LAST-OF(tidvnrtemp.VECKONUMMER) THEN DO TRANSACTION:
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidvnrtemp.PERSONALKOD  
            SUBSTRING(tidut.UT,8) = STRING(tidvnrtemp.VECKONUMMER,"999").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.MAN) > 0 THEN 
            SUBSTRING(tidut.UT,12,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.MAN),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TIS) > 0 THEN                       
            SUBSTRING(tidut.UT,16,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TIS),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.ONS) > 0 THEN 
            SUBSTRING(tidut.UT,20,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.ONS),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TOR) > 0 THEN
            SUBSTRING(tidut.UT,24,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TOR),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.FRE) > 0 THEN    
            SUBSTRING(tidut.UT,28,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.FRE),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.LOR) > 0 THEN 
            SUBSTRING(tidut.UT,32,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.LOR),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.SON) > 0 THEN
            SUBSTRING(tidut.UT,36,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.SON),">>9").                                        
         END.                                                   
      END.
   END.
END PROCEDURE.


PROCEDURE aopkont_UI :
   DEFINE INPUT PARAMETER TABLE FOR aoval.   
   DEFINE INPUT PARAMETER sokkto AS CHARACTER NO-UNDO.   
   DEFINE OUTPUT PARAMETER TABLE FOR aokto.      
   DEFINE OUTPUT PARAMETER TABLE FOR kto2.      
   FOR EACH aoval,
   EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = aoval.AONR AND AONRKONTKOD.DELNR = aoval.DELNR NO-LOCK.
      IF INDEX(SUBSTRING(sokkto,1,10),"*") = 0 THEN DO:      
         IF AONRKONTKOD.K2 = sokkto THEN DO:
            CREATE aokto.
            ASSIGN aokto.AONR = aoval.AONR 
            aokto.DELNR = aoval.DELNR 
            aokto.AONRREC = aoval.AONRREC
            aokto.K1 = AONRKONTKOD.K1
            aokto.K2 = AONRKONTKOD.K2 
            aokto.K3 = AONRKONTKOD.K3
            aokto.K4 = AONRKONTKOD.K4
            aokto.K5 = AONRKONTKOD.K5
            aokto.SATS% = AONRKONTKOD.SATS%.   .
            FIND FIRST kto2 WHERE kto2.K2 = AONRKONTKOD.K2 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kto2 THEN DO:
               CREATE kto2.
               ASSIGN kto2.K2 = AONRKONTKOD.K2.
            END.
         END.
      END.
      ELSE DO:
         utkonto = SUBSTRING(sokkto,1,INDEX(SUBSTRING(sokkto,1,10),"*") - 1).
         IF AONRKONTKOD.K2 BEGINS utkonto THEN DO:
            CREATE aokto.
            ASSIGN aokto.AONR = aoval.AONR 
            aokto.DELNR = aoval.DELNR 
            aokto.AONRREC = aoval.AONRREC
            aokto.K1 = AONRKONTKOD.K1
            aokto.K2 = AONRKONTKOD.K2 
            aokto.K3 = AONRKONTKOD.K3
            aokto.K4 = AONRKONTKOD.K4
            aokto.K5 = AONRKONTKOD.K5
            aokto.SATS% = AONRKONTKOD.SATS%.   .
            FIND FIRST kto2 WHERE kto2.K2 = AONRKONTKOD.K2 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kto2 THEN DO:
               CREATE kto2.
               ASSIGN kto2.K2 = AONRKONTKOD.K2.
            END.
         END.
      END.

      
   END.
END PROCEDURE.


PROCEDURE aopkontsumma_UI :
   /*körs inte klängre Lena 20180412*/
   DEFINE INPUT PARAMETER TABLE FOR aoval.
   DEFINE INPUT PARAMETER sokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER val1 AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER vallista AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER valdelnrlog AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER inder AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR visa.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   DEFINE VARIABLE sealltid AS LOGICAL NO-UNDO.
   RUN kollsealltid_UI (INPUT Guru.Konstanter:globanv ,OUTPUT sealltid).
   FIND FIRST visa WHERE visa.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   {aopsummauk.i}
   IF bytaonrmed NE "" THEN DO:
      CREATE tidut.
      CREATE tidut.      
      ASSIGN       
      SUBSTRING(tidut.UT,1) = Guru.Konstanter:gaok + " " + bytaonrmed .                                         
   END.
   IF RAD_PERIOD = 2 AND val1 = TRUE THEN DO:    
      GET FIRST dagsumkq NO-LOCK.
      IF AVAILABLE SUMTIDDAG THEN DO:
         CREATE tidut.
         CREATE tidut.
         CREATE tidut. 
         ASSIGN   
         SUBSTRING(tidut.UT,1,6) = "BILAGA". 
         CREATE tidut.
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "INGÅENDE TIDSEDLARS VECKONUMMER".
         CREATE tidut. 
         CREATE tidut.
         ASSIGN                        
         SUBSTRING(tidut.UT,1) = "ENHET/ "
         SUBSTRING(tidut.UT,12) = "TIMMAR".  
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "SIGN "  
         SUBSTRING(tidut.UT,8) = "VNR"
         SUBSTRING(tidut.UT,12) = "MÅN"
         SUBSTRING(tidut.UT,16) = "TIS"
         SUBSTRING(tidut.UT,20) = "ONS"
         SUBSTRING(tidut.UT,24) = "TOR"
         SUBSTRING(tidut.UT,28) = "FRE"
         SUBSTRING(tidut.UT,32) = "LÖR"
         SUBSTRING(tidut.UT,36) = "SÖN".
         CREATE tidut.
         ASSIGN                   
         SUBSTRING(tidut.UT,1) = "======.===.===.===.===.===.===.===.===".              
      END.  
      DO WHILE AVAILABLE(SUMTIDDAG):            
         FIND FIRST tidvnrtemp WHERE 
         tidvnrtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD AND
         tidvnrtemp.DATUM = SUMTIDDAG.DATUM 
         USE-INDEX PERSONALKOD NO-ERROR.
         IF NOT AVAILABLE tidvnrtemp THEN CREATE tidvnrtemp.
         ASSIGN
         tidvnrtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD
         tidvnrtemp.DATUM = SUMTIDDAG.DATUM
         tidvnrtemp.NAMN = SUMTIDDAG.FORNAMN + " " + SUMTIDDAG.EFTERNAMN.  
         IF WEEKDAY(SUMTIDDAG.DATUM) = 2 THEN 
         tidvnrtemp.MAN = tidvnrtemp.MAN +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 3 THEN 
         tidvnrtemp.TIS = tidvnrtemp.TIS +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 4 THEN 
         tidvnrtemp.ONS = tidvnrtemp.ONS +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 5 THEN 
         tidvnrtemp.TOR = tidvnrtemp.TOR +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 6 THEN 
         tidvnrtemp.FRE = tidvnrtemp.FRE +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 7 THEN 
         tidvnrtemp.LOR = tidvnrtemp.LOR +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 1 THEN 
         tidvnrtemp.SON = tidvnrtemp.SON +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         GET NEXT dagsumkq NO-LOCK.                                               
      END.
      REPEAT:
         FIND FIRST tidvnrtemp WHERE tidvnrtemp.VECKONUMMER = 0
         USE-INDEX VNR NO-ERROR.
         IF AVAILABLE tidvnrtemp THEN DO:
            regdatum = tidvnrtemp.DATUM.
            RUN REGVEC.P.
            regdagnamn = "MÅN".
            RUN VECODAT.P.
            bvnrdatum = regdatum.
            regdagnamn = "SÖN".
            RUN VECODAT.P.
            avnrdatum = regdatum.
            ASSIGN
            tidvnrtemp.VECKONUMMER = regvnr.
         END.
         ELSE LEAVE.   
         FOR EACH tidvnrtemp WHERE tidvnrtemp.DATUM >= bvnrdatum AND
         tidvnrtemp.DATUM <= avnrdatum AND tidvnrtemp.VECKONUMMER = 0
         USE-INDEX DATUM:
            ASSIGN
            tidvnrtemp.VECKONUMMER = regvnr.
         END.   
      END.                              
      FOR EACH tidvnrtemp
      BREAK BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER: 
         ACCUMULATE 
         tidvnrtemp.MAN (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.TIS (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.ONS (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.TOR (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER).  
         ACCUMULATE 
         tidvnrtemp.FRE (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.LOR (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER).   
         ACCUMULATE 
         tidvnrtemp.SON (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER).
         IF LAST-OF(tidvnrtemp.VECKONUMMER) THEN DO TRANSACTION:
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidvnrtemp.PERSONALKOD  
            SUBSTRING(tidut.UT,8) = STRING(tidvnrtemp.VECKONUMMER,"999").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.MAN) > 0 THEN 
            SUBSTRING(tidut.UT,12,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.MAN),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TIS) > 0 THEN                       
            SUBSTRING(tidut.UT,16,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TIS),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.ONS) > 0 THEN 
            SUBSTRING(tidut.UT,20,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.ONS),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TOR) > 0 THEN
            SUBSTRING(tidut.UT,24,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TOR),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.FRE) > 0 THEN    
            SUBSTRING(tidut.UT,28,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.FRE),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.LOR) > 0 THEN 
            SUBSTRING(tidut.UT,32,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.LOR),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.SON) > 0 THEN
            SUBSTRING(tidut.UT,36,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.SON),">>9").                                        
         END.                                                   
      END.
   END.
END PROCEDURE.

PROCEDURE aopkontsumma2_UI :
   DEFINE INPUT PARAMETER TABLE FOR aoval.
   DEFINE INPUT PARAMETER sokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER hsokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER val1 AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER vallista AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER valdelnrlog AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER inder AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR visa.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   DEFINE VARIABLE sealltid AS LOGICAL NO-UNDO.
   RUN kollsealltid_UI (INPUT Guru.Konstanter:globanv ,OUTPUT sealltid).
   FIND FIRST visa WHERE visa.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   {aopsummauk2.i}
   IF bytaonrmed NE "" THEN DO:
      CREATE tidut.
      CREATE tidut.      
      ASSIGN       
      SUBSTRING(tidut.UT,1) = Guru.Konstanter:gaok + " " + bytaonrmed .                                         
   END.
   IF RAD_PERIOD = 2 AND val1 = TRUE THEN DO:    
      GET FIRST dagsumkq NO-LOCK.
      IF AVAILABLE SUMTIDDAG THEN DO:
         CREATE tidut.
         CREATE tidut.
         CREATE tidut. 
         ASSIGN   
         SUBSTRING(tidut.UT,1,6) = "BILAGA". 
         CREATE tidut.
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "INGÅENDE TIDSEDLARS VECKONUMMER".
         CREATE tidut. 
         CREATE tidut.
         ASSIGN                        
         SUBSTRING(tidut.UT,1) = "ENHET/ "
         SUBSTRING(tidut.UT,12) = "TIMMAR".  
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,1) = "SIGN "  
         SUBSTRING(tidut.UT,8) = "VNR"
         SUBSTRING(tidut.UT,12) = "MÅN"
         SUBSTRING(tidut.UT,16) = "TIS"
         SUBSTRING(tidut.UT,20) = "ONS"
         SUBSTRING(tidut.UT,24) = "TOR"
         SUBSTRING(tidut.UT,28) = "FRE"
         SUBSTRING(tidut.UT,32) = "LÖR"
         SUBSTRING(tidut.UT,36) = "SÖN".
         CREATE tidut.
         ASSIGN                   
         SUBSTRING(tidut.UT,1) = "======.===.===.===.===.===.===.===.===".              
      END.  
      DO WHILE AVAILABLE(SUMTIDDAG):            
         FIND FIRST tidvnrtemp WHERE 
         tidvnrtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD AND
         tidvnrtemp.DATUM = SUMTIDDAG.DATUM 
         USE-INDEX PERSONALKOD NO-ERROR.
         IF NOT AVAILABLE tidvnrtemp THEN CREATE tidvnrtemp.
         ASSIGN
         tidvnrtemp.PERSONALKOD = SUMTIDDAG.PERSONALKOD
         tidvnrtemp.DATUM = SUMTIDDAG.DATUM
         tidvnrtemp.NAMN = SUMTIDDAG.FORNAMN + " " + SUMTIDDAG.EFTERNAMN.  
         IF WEEKDAY(SUMTIDDAG.DATUM) = 2 THEN 
         tidvnrtemp.MAN = tidvnrtemp.MAN +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 3 THEN 
         tidvnrtemp.TIS = tidvnrtemp.TIS +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 4 THEN 
         tidvnrtemp.ONS = tidvnrtemp.ONS +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 5 THEN 
         tidvnrtemp.TOR = tidvnrtemp.TOR +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 6 THEN 
         tidvnrtemp.FRE = tidvnrtemp.FRE +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 7 THEN 
         tidvnrtemp.LOR = tidvnrtemp.LOR +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         ELSE IF WEEKDAY(SUMTIDDAG.DATUM) = 1 THEN 
         tidvnrtemp.SON = tidvnrtemp.SON +
         SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         GET NEXT dagsumkq NO-LOCK.                                               
      END.
      REPEAT:
         FIND FIRST tidvnrtemp WHERE tidvnrtemp.VECKONUMMER = 0
         USE-INDEX VNR NO-ERROR.
         IF AVAILABLE tidvnrtemp THEN DO:
            regdatum = tidvnrtemp.DATUM.
            RUN REGVEC.P.
            regdagnamn = "MÅN".
            RUN VECODAT.P.
            bvnrdatum = regdatum.
            regdagnamn = "SÖN".
            RUN VECODAT.P.
            avnrdatum = regdatum.
            ASSIGN
            tidvnrtemp.VECKONUMMER = regvnr.
         END.
         ELSE LEAVE.   
         FOR EACH tidvnrtemp WHERE tidvnrtemp.DATUM >= bvnrdatum AND
         tidvnrtemp.DATUM <= avnrdatum AND tidvnrtemp.VECKONUMMER = 0
         USE-INDEX DATUM:
            ASSIGN
            tidvnrtemp.VECKONUMMER = regvnr.
         END.   
      END.                              
      FOR EACH tidvnrtemp
      BREAK BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER: 
         ACCUMULATE 
         tidvnrtemp.MAN (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.TIS (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.ONS (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.TOR (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER).  
         ACCUMULATE 
         tidvnrtemp.FRE (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER). 
         ACCUMULATE 
         tidvnrtemp.LOR (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER).   
         ACCUMULATE 
         tidvnrtemp.SON (TOTAL BY tidvnrtemp.PERSONALKOD BY tidvnrtemp.VECKONUMMER).
         IF LAST-OF(tidvnrtemp.VECKONUMMER) THEN DO TRANSACTION:
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidvnrtemp.PERSONALKOD  
            SUBSTRING(tidut.UT,8) = STRING(tidvnrtemp.VECKONUMMER,"999").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.MAN) > 0 THEN 
            SUBSTRING(tidut.UT,12,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.MAN),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TIS) > 0 THEN                       
            SUBSTRING(tidut.UT,16,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TIS),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.ONS) > 0 THEN 
            SUBSTRING(tidut.UT,20,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.ONS),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TOR) > 0 THEN
            SUBSTRING(tidut.UT,24,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.TOR),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.FRE) > 0 THEN    
            SUBSTRING(tidut.UT,28,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.FRE),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.LOR) > 0 THEN 
            SUBSTRING(tidut.UT,32,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.LOR),">>9").
            IF (ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.SON) > 0 THEN
            SUBSTRING(tidut.UT,36,3) = 
            STRING((ACCUM TOTAL BY tidvnrtemp.VECKONUMMER tidvnrtemp.SON),">>9").                                        
         END.                                                   
      END.
   END.
END PROCEDURE.

PROCEDURE aopkontsumma3_UI :
   DEFINE INPUT PARAMETER TABLE FOR aoval.
   DEFINE INPUT PARAMETER sokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER hsokkto AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER val1 AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER vallista AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.   
   DEFINE INPUT PARAMETER TABLE FOR visa.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   DEFINE VARIABLE sealltid AS LOGICAL NO-UNDO.
   RUN kollsealltid_UI (INPUT Guru.Konstanter:globanv ,OUTPUT sealltid).
   
   
   FIND FIRST visa WHERE visa.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   
   {aopsummauk3.i}
   IF bytaonrmed NE "" THEN DO:
      CREATE tidut.
      CREATE tidut.      
      ASSIGN       
      SUBSTRING(tidut.UT,1) = Guru.Konstanter:gaok + " " + bytaonrmed .                                         
   END.
   {GDPRLOGGCLIENT.I}
END PROCEDURE.


PROCEDURE aodhuvud_UI :
   DEFINE INPUT PARAMETE aonummer AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnummer AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER valdelnrlog AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   IF RAD_PERIOD = 1 THEN avdatum = DATE(12,31,YEAR(bdatum)).   
   DO TRANSACTION:   
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND TIDREGITAB.DATUM <= avdatum AND
      TIDREGITAB.AONR = aonummer AND TIDREGITAB.DELNR = delnummer AND TIDREGITAB.TIDLOG = TRUE         
      USE-INDEX AONR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         FIND FIRST TIDFEL WHERE TIDFEL.DATUM >= bdatum AND TIDFEL.DATUM <= avdatum AND
         TIDFEL.AONR = aonummer AND TIDFEL.DELNR = delnummer AND TIDFEL.TIDLOG = TRUE         
         USE-INDEX AONR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE TIDFEL THEN DO:
            CREATE felmeddtemp.
            ASSIGN felmeddtemp.FELMEDD = "Det finns ingen tid skriven på " + LC(Guru.Konstanter:gaok) + aonummer + STRING(delnummer,Guru.Konstanter:varforetypchar[1]).
            RETURN.         
         END.
      END.      
         
      CREATE tidut. 
      SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
      CREATE tidut.
      CREATE tidut.                           
      ASSIGN
      tidut.UT = "U50- " + CAPS(Guru.Konstanter:gaol) + "-TID-DETALJERAT PERIOD"               
      SUBSTRING(tidut.UT,45) = STRING(bdatum) + " - " + STRING(avdatum).     
      CREATE tidut.
      CREATE tidut.      
      ASSIGN
      SUBSTRING(tidut.UT,71) = "EKO-LÖN"
      SUBSTRING(tidut.UT,82) = "SENAST"
      SUBSTRING(tidut.UT,99) = "RÄTTAD".
      IF Guru.Konstanter:varforetypval[47] = 1 THEN   SUBSTRING(tidut.UT,110) = "ANNAT". 
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "ENHET/SIGN"         
      SUBSTRING(tidut.UT,15) = CAPS(Guru.Konstanter:gaok)                    
      SUBSTRING(tidut.UT,26) = "DATUM" 
      SUBSTRING(tidut.UT,35) = "START" 
      SUBSTRING(tidut.UT,41) = "SLUT"
      SUBSTRING(tidut.UT,47) = "TIMMAR"
      SUBSTRING(tidut.UT,54) = CAPS(Guru.Konstanter:gdebk)
      SUBSTRING(tidut.UT,65) = "GODK."
      SUBSTRING(tidut.UT,71) = "SAMMAN"
      SUBSTRING(tidut.UT,82) = "ÄNDRAD"
      SUBSTRING(tidut.UT,99) = "SAMMAN".
      IF Guru.Konstanter:varforetypval[47] = 1 THEN SUBSTRING(tidut.UT,110) = "BOLAG". 
      CREATE tidut.
      IF Guru.Konstanter:varforetypval[47] = 1 THEN                                       
      ASSIGN 
      tidut.UT =                                                           
      "=============.==========.========.=====.=====.======.==========.=====.=========.=================.=========.======.".
      ELSE DO:
         ASSIGN 
      tidut.UT =                                                           
      "=============.==========.========.=====.=====.======.==========.=====.=========.=================.=========.".
      END.                             
   END.
END PROCEDURE.
PROCEDURE aodlhuvud_UI :
   DEFINE INPUT PARAMETE aonummer AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnummer AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER valdelnrlog AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   IF RAD_PERIOD = 1 THEN avdatum = DATE(12,31,YEAR(bdatum)).

   DO TRANSACTION:   
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND TIDREGITAB.DATUM <= avdatum AND
      TIDREGITAB.AONR = aonummer AND TIDREGITAB.DELNR = delnummer AND TIDREGITAB.TIDLOG = TRUE         
      USE-INDEX AONR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         FIND FIRST TIDFEL WHERE TIDFEL.DATUM >= bdatum AND TIDFEL.DATUM <= avdatum AND
         TIDFEL.AONR = aonummer AND TIDFEL.DELNR = delnummer AND TIDFEL.TIDLOG = TRUE         
         USE-INDEX AONR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE TIDFEL THEN DO:
            CREATE felmeddtemp.
            ASSIGN felmeddtemp.FELMEDD = "Det finns ingen tid skriven på " + LC(Guru.Konstanter:gaok) + aonummer + STRING(delnummer,Guru.Konstanter:varforetypchar[1]).
            RETURN.       
         END.
      END.      
      CREATE tidut. 
      SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
      CREATE tidut.
      CREATE tidut.                           
      ASSIGN
      tidut.UT = "U51- " + CAPS(Guru.Konstanter:gaol) + "-TID-DETALJERAT-KOMMENTAR PERIOD"               
      SUBSTRING(tidut.UT,50) = STRING(bdatum) + " - " + STRING(avdatum).     
      CREATE tidut.
      CREATE tidut.      
      ASSIGN
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)
      SUBSTRING(tidut.UT,15) = aonummer
      SUBSTRING(tidut.UT,22) = STRING(delnummer,Guru.Konstanter:varforetypchar[1]).      
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD NO-LOCK NO-ERROR.        
      FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.
      CREATE tidut.
      FOR EACH OVERKOD WHERE OVERKOD.KOD =  ANSTFORMTAB.KOD NO-LOCK:
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = OVERKOD.VILART + " = ".
         SUBSTRING(tidut.UT,7) = OVERKOD.LONKODTEXT. 
      END.
            
      CREATE tidut.          
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "ENHET".
      SUBSTRING(tidut.UT,22) = "ÖVERT".         
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "/SIGN"                  
      SUBSTRING(tidut.UT,7) = "DATUM"          
      SUBSTRING(tidut.UT,16) = "TIM"
      SUBSTRING(tidut.UT,22) = "TIM"
      SUBSTRING(tidut.UT,28) = "RESMÅL".         
      CREATE tidut.                                       
      ASSIGN 
      tidut.UT =                                                           
      "=====.========.=====.=====.=========================================================================".                                  
   END.
END PROCEDURE.

PROCEDURE aodkohuvud_UI :
   DEFINE INPUT PARAMETE aonummer AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnummer AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER valdelnrlog AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   IF RAD_PERIOD = 1 THEN avdatum = DATE(12,31,YEAR(bdatum)).

   DO TRANSACTION:   
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND TIDREGITAB.DATUM <= avdatum AND
      TIDREGITAB.AONR = aonummer AND TIDREGITAB.DELNR = delnummer AND TIDREGITAB.TIDLOG = TRUE         
      AND TIDREGITAB.RESMAL NE "" USE-INDEX AONR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         FIND FIRST TIDFEL WHERE TIDFEL.DATUM >= bdatum AND TIDFEL.DATUM <= avdatum AND
         TIDFEL.AONR = aonummer AND TIDFEL.DELNR = delnummer AND TIDFEL.TIDLOG = TRUE         
         USE-INDEX AONR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE TIDFEL THEN DO:
            CREATE felmeddtemp.
            ASSIGN felmeddtemp.FELMEDD = "Det finns ingen tid med kommentar skriven på " + LC(Guru.Konstanter:gaok) + aonummer + STRING(delnummer,Guru.Konstanter:varforetypchar[1]).
            RETURN.       
         END.
      END.      
      CREATE tidut. 
      SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
      CREATE tidut.
      CREATE tidut.                           
      ASSIGN
      tidut.UT = "U52- " + CAPS(Guru.Konstanter:gaol) + "-TID-KOMMENTAR PERIOD"               
      SUBSTRING(tidut.UT,50) = STRING(bdatum) + " - " + STRING(avdatum).     
      CREATE tidut.
      CREATE tidut.      
      ASSIGN
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)
      SUBSTRING(tidut.UT,15) = aonummer
      SUBSTRING(tidut.UT,22) = STRING(delnummer,Guru.Konstanter:varforetypchar[1]).            
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "ENHET".         
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "/SIGN"                  
      SUBSTRING(tidut.UT,7) = "NAMN"                  
      SUBSTRING(tidut.UT,18) = "DATUM"          
      SUBSTRING(tidut.UT,27) = "TIM"
      SUBSTRING(tidut.UT,33) = "Kommentar".         
      CREATE tidut.                                       
      ASSIGN 
      tidut.UT =                                                           
      "=====.==========.========.=====.=============================================================================".                                  
   END.
END PROCEDURE.


PROCEDURE aodsumma_UI :
   DEFINE INPUT PARAMETER aonummer AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnummer AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER inder AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   DEFINE VARIABLE sealltid  AS LOGICAL NO-UNDO.
   DEBUGGER:SET-BREAK().
   EMPTY TEMP-TABLE bytaao NO-ERROR.
   EMPTY TEMP-TABLE tiddettemp  NO-ERROR.
   DEBUGGER:SET-BREAK().
   RUN kollsealltid_UI (INPUT Guru.Konstanter:globanv ,OUTPUT sealltid).
   CREATE bytaao.
   ASSIGN
   bytaao.VADGORA = 2
   bytaao.STDATUM = bdatum
   bytaao.SLDATUM = avdatum
   bytaao.ARTAL = YEAR(bdatum)
   bytaao.RAD_PERIOD = RAD_PERIOD
   bytaao.AONR = aonummer
   bytaao.DELNR = delnummer
   bytaao.GLOBFORETAG = Guru.Konstanter:globforetag
   bytaao.INDER = inder.      
   RUN SKAPAOBY.P (INPUT-OUTPUT TABLE bytaao, INPUT-OUTPUT TABLE dagtemp, INPUT-OUTPUT TABLE indertemp , INPUT-OUTPUT TABLE restid, OUTPUT bytaonrmed, INPUT-OUTPUT TABLE tidut). 
   {DAGTEMPBOLAG2.I}
   OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND 
   TIDREGITAB.DATUM <= avdatum AND
   TIDREGITAB.AONR = aonummer AND TIDREGITAB.DELNR = delnummer AND TIDREGITAB.TIDLOG = TRUE         
   USE-INDEX AONR NO-LOCK.
   GET FIRST tq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB): 
      {DAGTEMPBOLAG3.I}
      {ANVTIDSEKKOLL.I}
      IF sealltid = TRUE THEN tisek = TRUE.
      IF bsek = FALSE AND tisek = TRUE THEN DO:                  
         CREATE tiddettemp.                  
         ASSIGN
         tiddettemp.PERSONALKOD = TIDREGITAB.PERSONALKOD                
         tiddettemp.AONR = TIDREGITAB.AONR 
         tiddettemp.DELNR = TIDREGITAB.DELNR 
         tiddettemp.DATUM = TIDREGITAB.DATUM
         tiddettemp.DAG = TIDREGITAB.DAG
         tiddettemp.START = TIDREGITAB.START 
         tiddettemp.SLUT = TIDREGITAB.SLUT
         tiddettemp.TOTALT = klock100(TIDREGITAB.TOTALT)
         tiddettemp.PRISTYP = TIDREGITAB.PRISTYP
         tiddettemp.PRIS = TIDREGITAB.PRIS                  
         tiddettemp.GODKAND = SUBSTRING(TIDREGITAB.GODKAND,1,4)
         tiddettemp.VECKOKORD = TIDREGITAB.VECKOKORD.
         tiddettemp.RESMAL = SUBSTRING(TIDREGITAB.RESMAL,1,50).      
         IF TIDREGITAB.PROGRAM NE "" THEN DO:   
            anddat = INDEX(TIDREGITAB.PROGRAM,"/",1) .
            IF anddat > 0 THEN DO:         
               tiddettemp.PROGRAM = SUBSTRING(TIDREGITAB.PROGRAM,anddat - 2,LENGTH(TIDREGITAB.PROGRAM)).
            END.
         END.
         IF Guru.Konstanter:varforetypval[47] = 1 THEN DO:
            FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD NO-LOCK NO-ERROR.
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND AONRTAB.DELNR = TIDREGITAB.DELNR NO-LOCK NO-ERROR.
            IF AVAILABLE AONRTAB AND AONRTAB.OMRADE NE "" THEN DO:
               FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
               FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
               FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
               FIND FIRST oradebuff WHERE oradebuff.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
               FIND FIRST avdbuff WHERE avdbuff.AVDELNINGNR = oradebuff.AVDELNINGNR NO-LOCK NO-ERROR.
               FIND FIRST jurbuff WHERE jurbuff.JUDID = avdbuff.POSTANST NO-LOCK NO-ERROR.
               IF JURPERS.JUDID NE jurbuff.JUDID AND oradebuff.OMRADE NE ""  THEN DO:
                  tiddettemp.ANNATBOL = PERSONALTAB.OMRADE.
               END.
            END.
         END.   
                   
      END.        
      GET NEXT tq NO-LOCK.
   END.            
   OPEN QUERY ftq FOR EACH TIDFEL WHERE TIDFEL.DATUM >= bdatum AND 
   TIDFEL.DATUM <= avdatum AND
   TIDFEL.AONR = aonummer AND TIDFEL.DELNR = delnummer AND TIDFEL.TIDLOG = TRUE         
   USE-INDEX AONR NO-LOCK.
   GET FIRST ftq NO-LOCK.
   DO WHILE AVAILABLE(TIDFEL): 
      {DAGTEMPBOLAG4.I}
      {ANVTIDFSEKKOLL.I} 
      IF sealltid = TRUE THEN tisek = TRUE.
      IF bsek = FALSE AND tisek = TRUE THEN DO:
         CREATE tiddettemp.         
         ASSIGN
         tiddettemp.PERSONALKOD = TIDFEL.PERSONALKOD         
         tiddettemp.AONR = TIDFEL.AONR 
         tiddettemp.DELNR = TIDFEL.DELNR 
         tiddettemp.DATUM = TIDFEL.DATUM 
         tiddettemp.START = TIDFEL.START 
         tiddettemp.SLUT = TIDFEL.SLUT.      
         tiddettemp.RESMAL = SUBSTRING(TIDFEL.RESMAL,1,50).
         IF TIDFEL.DEBET = FALSE THEN DO:      
            tidftot = ( -1 ) * klock100(TIDFEL.TOTALT).
            ASSIGN
            tiddettemp.TOTALT = tidftot.                                         
         END.
         ELSE DO:
            ASSIGN
            tiddettemp.TOTALT = klock100(TIDFEL.TOTALT).                           
         END.
         ASSIGN
         tiddettemp.RATT = "R"
         tiddettemp.PRISTYP = TIDFEL.PRISTYP
         tiddettemp.PRIS = TIDFEL.PRIS  
         tiddettemp.GODKAND = SUBSTRING(TIDFEL.GODKAND,1,4)
         tiddettemp.VECKOKORD = TIDFEL.VECKOKORD.      
         IF TIDFEL.PROGRAM NE "" THEN DO:   
            anddat = INDEX(TIDFEL.PROGRAM,"/",1) .
            IF anddat > 0 THEN DO:         
               tiddettemp.PROGRAM = SUBSTRING(TIDFEL.PROGRAM,anddat - 2,LENGTH(TIDFEL.PROGRAM)).
            END.
         END.    
         tiddettemp.FELKORD = TIDFEL.FELKORD.
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = TIDFEL.PERSONALKOD NO-LOCK NO-ERROR.
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDFEL.AONR AND AONRTAB.DELNR = TIDFEL.DELNR NO-LOCK NO-ERROR.
         IF AVAILABLE AONRTAB AND AONRTAB.OMRADE NE "" THEN DO:
            FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
            FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
            FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
            FIND FIRST oradebuff WHERE oradebuff.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
            FIND FIRST avdbuff WHERE avdbuff.AVDELNINGNR = oradebuff.AVDELNINGNR NO-LOCK NO-ERROR.
            FIND FIRST jurbuff WHERE jurbuff.JUDID = avdbuff.POSTANST NO-LOCK NO-ERROR.
            IF JURPERS.JUDID NE jurbuff.JUDID AND oradebuff.OMRADE NE ""  THEN DO:
               tiddettemp.ANNATBOL = PERSONALTAB.OMRADE.
            END.
         END.              
      END.         
      GET NEXT ftq NO-LOCK.
   END.            
   IF bytaonrmed NE "" THEN DO:
      CREATE tidut.
      CREATE tidut.      
      ASSIGN       
      SUBSTRING(tidut.UT,1) = Guru.Konstanter:gaok + " " + bytaonrmed . 
   END.
   FOR EACH tiddettemp USE-INDEX DATUM NO-LOCK:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = tiddettemp.PERSONALKOD 
      SUBSTRING(tidut.UT,15) = tiddettemp.AONR
      SUBSTRING(tidut.UT,22) = STRING(tiddettemp.DELNR,Guru.Konstanter:varforetypchar[1]) 
      SUBSTRING(tidut.UT,26) = STRING(tiddettemp.DATUM)
      SUBSTRING(tidut.UT,35) = STRING(tiddettemp.START,"99.99") 
      SUBSTRING(tidut.UT,41) = STRING(tiddettemp.SLUT,"99.99")
      SUBSTRING(tidut.UT,47) = STRING(tiddettemp.TOTALT,"99.99")               
      /*SUBSTRING(STRING(tiddettemp.TOTALT,"99.99"),1,2) +                 
      STRING(DECIMAL(SUBSTRING(STRING(tiddettemp.TOTALT,"99.99"),4,2)) / 60,".99")*/
      SUBSTRING(tidut.UT,54) = tiddettemp.PRISTYP
      SUBSTRING(tidut.UT,65) = SUBSTRING(tiddettemp.GODKAND,1,4)
      SUBSTRING(tidut.UT,71) = tiddettemp.VECKOKORD.      
      IF tiddettemp.PROGRAM NE "" THEN DO:   
         anddat = INDEX(tiddettemp.PROGRAM,"/",1) .
         IF anddat > 0 THEN DO:         
            SUBSTRING(tidut.UT,81) = SUBSTRING(tiddettemp.PROGRAM,anddat - 2,LENGTH(tiddettemp.PROGRAM)).
         END.
      END.      
      SUBSTRING(tidut.UT,99) = tiddettemp.FELKORD.
      
      SUBSTRING(tidut.UT,110) = tiddettemp.ANNATBOL.      
   END.

END PROCEDURE.
PROCEDURE aodlsumma_UI :
   DEFINE INPUT PARAMETER aonummer AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnummer AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER inder AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   DEFINE VARIABLE sealltid AS LOGICAL NO-UNDO.
   EMPTY TEMP-TABLE bytaao NO-ERROR.
   DEBUGGER:SET-BREAK().
   RUN kollsealltid_UI (INPUT Guru.Konstanter:globanv ,OUTPUT sealltid).
    
   CREATE bytaao.
   ASSIGN
   bytaao.VADGORA = 2
   bytaao.STDATUM = bdatum
   bytaao.SLDATUM = avdatum
   bytaao.ARTAL = YEAR(bdatum)
   bytaao.RAD_PERIOD = RAD_PERIOD
   bytaao.AONR = aonummer
   bytaao.DELNR = delnummer
   bytaao.GLOBFORETAG = Guru.Konstanter:globforetag
   bytaao.INDER = inder.      
   viseg = TRUE.
   IF Guru.Konstanter:globforetag = "LULE" AND ( aonummer = "141" OR aonummer = "143") THEN DO:
      FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      IF AVAILABLE ANVANDARE THEN DO: 
         IF ANVANDARE.AV-LEVEL = 0 THEN.         
         ELSE IF ANVANDARE.AV-LEVEL NE 10 THEN viseg = FALSE.         
      END.
   END.
   RUN SKAPAOBY.P(INPUT-OUTPUT TABLE bytaao, INPUT-OUTPUT TABLE dagtemp, INPUT-OUTPUT TABLE indertemp , INPUT-OUTPUT TABLE restid, OUTPUT bytaonrmed, INPUT-OUTPUT TABLE tidut). 
   {DAGTEMPBOLAG2.I}
   OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND 
   TIDREGITAB.DATUM <= avdatum AND
   TIDREGITAB.AONR = aonummer AND TIDREGITAB.DELNR = delnummer AND TIDREGITAB.TIDLOG = TRUE         
   USE-INDEX AONR NO-LOCK.
   GET FIRST tq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      {DAGTEMPBOLAG3.I}
      {ANVTIDSEKKOLL.I}
      IF sealltid = TRUE THEN tisek = TRUE.
      IF bsek = FALSE AND tisek = TRUE THEN DO:       
         IF viseg = TRUE THEN DO:
            IF TIDREGITAB.OKOD1 = "" AND TIDREGITAB.OKOD2 = "" AND TIDREGITAB.OKOD3 = "" THEN DO:         
               CREATE tidut.         
               ASSIGN
               SUBSTRING(tidut.UT,1) = TIDREGITAB.PERSONALKOD          
               SUBSTRING(tidut.UT,7) = STRING(TIDREGITAB.DATUM)                   
               SUBSTRING(tidut.UT,16) =                
               SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),1,2) +                 
               STRING(DECIMAL(SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),4,2)) / 60,".99")
               SUBSTRING(tidut.UT,28) = TIDREGITAB.RESMAL.
            END.
            ELSE DO:
               IF TIDREGITAB.OKOD1 NE "" THEN DO:                 
                  FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD1 NO-LOCK NO-ERROR.
                  IF AVAILABLE OVERKOD THEN DO:
                      CREATE tidut.         
                      ASSIGN
                      SUBSTRING(tidut.UT,1) = TIDREGITAB.PERSONALKOD          
                      SUBSTRING(tidut.UT,7) = STRING(TIDREGITAB.DATUM)                   
                      SUBSTRING(tidut.UT,16) =                
                      SUBSTRING(STRING(TIDREGITAB.OANT1,"99.99"),1,2) +                 
                      STRING(DECIMAL(SUBSTRING(STRING(TIDREGITAB.OANT1,"99.99"),4,2)) / 60,".99")
                      SUBSTRING(tidut.UT,22) = OVERKOD.VILART.
                      SUBSTRING(tidut.UT,28) = TIDREGITAB.RESMAL.
                  END.   
               END.
               IF TIDREGITAB.OKOD2 NE "" THEN DO:
                   FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD2 NO-LOCK NO-ERROR.
                   IF AVAILABLE OVERKOD THEN DO:
                      CREATE tidut.         
                      ASSIGN
                      SUBSTRING(tidut.UT,1) = TIDREGITAB.PERSONALKOD          
                      SUBSTRING(tidut.UT,7) = STRING(TIDREGITAB.DATUM)                   
                      SUBSTRING(tidut.UT,16) =                
                      SUBSTRING(STRING(TIDREGITAB.OANT2,"99.99"),1,2) +                 
                      STRING(DECIMAL(SUBSTRING(STRING(TIDREGITAB.OANT2,"99.99"),4,2)) / 60,".99")
                      SUBSTRING(tidut.UT,22) = OVERKOD.VILART.
                      SUBSTRING(tidut.UT,28) = TIDREGITAB.RESMAL.
                   END.   
               END.
               IF TIDREGITAB.OKOD3 NE "" THEN DO:
                   FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD3 NO-LOCK NO-ERROR.
                   IF AVAILABLE OVERKOD THEN DO:
                      CREATE tidut.         
                      ASSIGN
                      SUBSTRING(tidut.UT,1) = TIDREGITAB.PERSONALKOD          
                      SUBSTRING(tidut.UT,7) = STRING(TIDREGITAB.DATUM)                   
                      SUBSTRING(tidut.UT,16) =                
                      SUBSTRING(STRING(TIDREGITAB.OANT3,"99.99"),1,2) +                 
                      STRING(DECIMAL(SUBSTRING(STRING(TIDREGITAB.OANT3,"99.99"),4,2)) / 60,".99")
                      SUBSTRING(tidut.UT,22) = OVERKOD.VILART.
                      SUBSTRING(tidut.UT,28) = TIDREGITAB.RESMAL.
                   END.   
               END.   
            END.                     
         END.
         
      END.
      GET NEXT tq NO-LOCK.
   END.         
   viseg = TRUE.
   IF Guru.Konstanter:globforetag = "LULE" AND ( aonummer = "141" OR aonummer = "143") THEN DO:
      FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      IF AVAILABLE ANVANDARE THEN DO: 
         IF ANVANDARE.AV-LEVEL = 0 THEN.         
         ELSE IF ANVANDARE.AV-LEVEL NE 10 THEN viseg = FALSE.
         
      END.
   END.
   OPEN QUERY ftq FOR EACH TIDFEL WHERE TIDFEL.DATUM >= bdatum AND 
   TIDFEL.DATUM <= avdatum AND
   TIDFEL.AONR = aonummer AND TIDFEL.DELNR = delnummer AND TIDFEL.TIDLOG = TRUE         
   USE-INDEX AONR NO-LOCK.
   GET FIRST ftq NO-LOCK.
   DO WHILE AVAILABLE(TIDFEL):
      {DAGTEMPBOLAG4.I}
      {ANVTIDFSEKKOLL.I}
      IF sealltid = TRUE THEN tisek = TRUE.
      IF bsek = FALSE AND tisek = TRUE THEN DO:       
         IF viseg = TRUE THEN DO:         
            IF TIDFEL.OKOD1 = "" AND TIDFEL.OKOD2 = "" AND TIDFEL.OKOD3 = "" THEN DO:
               CREATE tidut.         
               ASSIGN
               SUBSTRING(tidut.UT,1) = TIDFEL.PERSONALKOD       
               SUBSTRING(tidut.UT,7) = STRING(TIDFEL.DATUM).       
               IF TIDFEL.DEBET = FALSE THEN DO:      
                  tidftot = ( -1 ) * TIDFEL.TOTALT.
                  ASSIGN
                  SUBSTRING(tidut.UT,15) = SUBSTRING(STRING(tidftot,"-99.99"),1,3) +                 
                  STRING(DECIMAL(SUBSTRING(STRING(tidftot,"-99.99"),5,2)) / 60,".99").         
               END.
               ELSE DO:
                  ASSIGN
                  SUBSTRING(tidut.UT,16) =                
                  SUBSTRING(STRING(TIDFEL.TOTALT,"99.99"),1,2) +                 
                  STRING(DECIMAL(SUBSTRING(STRING(TIDFEL.TOTALT,"99.99"),4,2)) / 60,".99").
               END.
               SUBSTRING(tidut.UT,27) = "R".
               ASSIGN
               SUBSTRING(tidut.UT,28) = TIDFEL.RESMAL.
            END.
            ELSE DO:
               IF TIDFEL.OKOD1 NE "" THEN DO:                 
                  FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND OVERKOD.OVERTIDTILL = TIDFEL.OKOD1 NO-LOCK NO-ERROR.
                  IF AVAILABLE OVERKOD THEN DO:
                     CREATE tidut.         
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = TIDFEL.PERSONALKOD       
                     SUBSTRING(tidut.UT,7) = STRING(TIDFEL.DATUM).       
                     IF TIDFEL.DEBET = FALSE THEN DO:      
                        tidftot = ( -1 ) * TIDFEL.OANT1.
                        ASSIGN
                        SUBSTRING(tidut.UT,15) = SUBSTRING(STRING(tidftot,"-99.99"),1,3) +                 
                        STRING(DECIMAL(SUBSTRING(STRING(tidftot,"-99.99"),5,2)) / 60,".99").         
                     END.
                     ELSE DO:
                        ASSIGN
                        SUBSTRING(tidut.UT,16) =                
                        SUBSTRING(STRING(TIDFEL.OANT1,"99.99"),1,2) +                 
                        STRING(DECIMAL(SUBSTRING(STRING(TIDFEL.OANT1,"99.99"),4,2)) / 60,".99").
                     END.
                     SUBSTRING(tidut.UT,22) = OVERKOD.VILART.
                     SUBSTRING(tidut.UT,27) = "R".
                     ASSIGN
                     SUBSTRING(tidut.UT,28) = TIDFEL.RESMAL.                     
                  END.
               END.
               IF TIDFEL.OKOD2 NE "" THEN DO:                 
                  FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND OVERKOD.OVERTIDTILL = TIDFEL.OKOD2 NO-LOCK NO-ERROR.
                  IF AVAILABLE OVERKOD THEN DO:
                     CREATE tidut.         
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = TIDFEL.PERSONALKOD       
                     SUBSTRING(tidut.UT,7) = STRING(TIDFEL.DATUM).       
                     IF TIDFEL.DEBET = FALSE THEN DO:      
                        tidftot = ( -1 ) * TIDFEL.OANT2.
                        ASSIGN
                        SUBSTRING(tidut.UT,15) = SUBSTRING(STRING(tidftot,"-99.99"),1,3) +                 
                        STRING(DECIMAL(SUBSTRING(STRING(tidftot,"-99.99"),5,2)) / 60,".99").         
                     END.
                     ELSE DO:
                        ASSIGN
                        SUBSTRING(tidut.UT,16) =                
                        SUBSTRING(STRING(TIDFEL.OANT2,"99.99"),1,2) +                 
                        STRING(DECIMAL(SUBSTRING(STRING(TIDFEL.OANT2,"99.99"),4,2)) / 60,".99").
                     END.
                     SUBSTRING(tidut.UT,22) = OVERKOD.VILART.
                     SUBSTRING(tidut.UT,27) = "R".
                     ASSIGN
                     SUBSTRING(tidut.UT,28) = TIDFEL.RESMAL.                     
                  END.
               END.
               IF TIDFEL.OKOD3 NE "" THEN DO:                 
                  FIND FIRST OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND OVERKOD.OVERTIDTILL = TIDFEL.OKOD3 NO-LOCK NO-ERROR.
                  IF AVAILABLE OVERKOD THEN DO:
                     CREATE tidut.         
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = TIDFEL.PERSONALKOD       
                     SUBSTRING(tidut.UT,7) = STRING(TIDFEL.DATUM).       
                     IF TIDFEL.DEBET = FALSE THEN DO:      
                        tidftot = ( -1 ) * TIDFEL.OANT3.
                        ASSIGN
                        SUBSTRING(tidut.UT,15) = SUBSTRING(STRING(tidftot,"-99.99"),1,3) +                 
                        STRING(DECIMAL(SUBSTRING(STRING(tidftot,"-99.99"),5,2)) / 60,".99").         
                     END.
                     ELSE DO:
                        ASSIGN
                        SUBSTRING(tidut.UT,16) =                
                        SUBSTRING(STRING(TIDFEL.OANT3,"99.99"),1,2) +                 
                        STRING(DECIMAL(SUBSTRING(STRING(TIDFEL.OANT3,"99.99"),4,2)) / 60,".99").
                     END.
                     SUBSTRING(tidut.UT,22) = OVERKOD.VILART.
                     SUBSTRING(tidut.UT,27) = "R".
                     ASSIGN
                     SUBSTRING(tidut.UT,28) = TIDFEL.RESMAL.                     
                  END.
               END.      
            END.      
         END.
      END.         
      GET NEXT ftq NO-LOCK.
   END.         
   viseg = TRUE.
   IF bytaonrmed NE "" THEN DO:
      CREATE tidut.
      CREATE tidut.      
      ASSIGN       
      SUBSTRING(tidut.UT,1) = Guru.Konstanter:gaok + " " + bytaonrmed . 
   END.

END PROCEDURE.
PROCEDURE aodkosumma_UI :
   DEFINE INPUT PARAMETER aonummer AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnummer AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER inder AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.   
   DEFINE VARIABLE sealltid AS LOGICAL NO-UNDO.
   EMPTY TEMP-TABLE bytaao NO-ERROR.
   DEBUGGER:SET-BREAK().
   RUN kollsealltid_UI (INPUT Guru.Konstanter:globanv ,OUTPUT sealltid).
   CREATE bytaao.
   ASSIGN
   bytaao.VADGORA = 2
   bytaao.STDATUM = bdatum
   bytaao.SLDATUM = avdatum
   bytaao.ARTAL = YEAR(bdatum)
   bytaao.RAD_PERIOD = RAD_PERIOD
   bytaao.AONR = aonummer
   bytaao.DELNR = delnummer
   bytaao.GLOBFORETAG = Guru.Konstanter:globforetag
   bytaao.INDER = inder.      
   RUN SKAPAOBY.P(INPUT-OUTPUT TABLE bytaao, INPUT-OUTPUT TABLE dagtemp, INPUT-OUTPUT TABLE indertemp , INPUT-OUTPUT TABLE restid, OUTPUT bytaonrmed, INPUT-OUTPUT TABLE tidut). 
   {DAGTEMPBOLAG2.I}
   OPEN QUERY tq FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= bdatum AND 
   TIDREGITAB.DATUM <= avdatum AND
   TIDREGITAB.AONR = aonummer AND TIDREGITAB.DELNR = delnummer AND TIDREGITAB.TIDLOG = TRUE         
   AND TIDREGITAB.RESMAL NE "" USE-INDEX PKOD NO-LOCK.
   GET FIRST tq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      {DAGTEMPBOLAG3.I}
      {ANVTIDSEKKOLL.I}
      IF sealltid = TRUE THEN tisek = TRUE.
      IF bsek = FALSE AND tisek = TRUE THEN DO:       
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD NO-LOCK NO-ERROR.        
         FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.
         CREATE tidut.         
         ASSIGN
         SUBSTRING(tidut.UT,1) = TIDREGITAB.PERSONALKOD.
         IF AVAILABLE PERSONALTAB THEN SUBSTRING(tidut.UT,7) = CAPS(SUBSTRING(PERSONALTAB.FORNAMN,1,1)) + "." + CAPS(SUBSTRING(PERSONALTAB.EFTERNAMN,1,7)) + ".".
         ASSIGN
         SUBSTRING(tidut.UT,18) = STRING(TIDREGITAB.DATUM)                   
         SUBSTRING(tidut.UT,27) =                
         SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),1,2) +                 
         STRING(DECIMAL(SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),4,2)) / 60,".99").            
         
         SUBSTRING(tidut.UT,33) = TIDREGITAB.RESMAL.             
      END.
      GET NEXT tq NO-LOCK.
   END.            
   OPEN QUERY ftq FOR EACH TIDFEL WHERE TIDFEL.DATUM >= bdatum AND 
   TIDFEL.DATUM <= avdatum AND
   TIDFEL.AONR = aonummer AND TIDFEL.DELNR = delnummer AND TIDFEL.TIDLOG = TRUE         
   AND TIDFEL.RESMAL NE "" USE-INDEX PKOD NO-LOCK.
   GET FIRST ftq NO-LOCK.
   DO WHILE AVAILABLE(TIDFEL):
      {DAGTEMPBOLAG4.I}
      {ANVTIDFSEKKOLL.I}
      IF sealltid = TRUE THEN tisek = TRUE.
      IF bsek = FALSE AND tisek = TRUE  THEN DO:       
         CREATE tidut.         
         ASSIGN
         SUBSTRING(tidut.UT,1) = TIDFEL.PERSONALKOD.
         IF AVAILABLE PERSONALTAB THEN SUBSTRING(tidut.UT,7) = CAPS(SUBSTRING(PERSONALTAB.FORNAMN,1,1)) + "." + CAPS(substring(PERSONALTAB.EFTERNAMN,1,7)) + ".".
         SUBSTRING(tidut.UT,18) = STRING(TIDFEL.DATUM).       
         IF TIDFEL.DEBET = FALSE THEN DO:      
            tidftot = ( -1 ) * TIDFEL.TOTALT.
            ASSIGN
            SUBSTRING(tidut.UT,26) = SUBSTRING(STRING(tidftot,"-99.99"),1,3) +                 
            STRING(DECIMAL(SUBSTRING(STRING(tidftot,"-99.99"),5,2)) / 60,".99").         
         END.
         ELSE DO:
            ASSIGN
            SUBSTRING(tidut.UT,27) =                
            SUBSTRING(STRING(TIDFEL.TOTALT,"99.99"),1,2) +                 
            STRING(DECIMAL(SUBSTRING(STRING(TIDFEL.TOTALT,"99.99"),4,2)) / 60,".99").
         END.
         
         
         SUBSTRING(tidut.UT,32) = "R".
         ASSIGN
         SUBSTRING(tidut.UT,33) = TIDFEL.RESMAL.
      END.         
      GET NEXT ftq NO-LOCK.
   END.            
   IF bytaonrmed NE "" THEN DO:
      CREATE tidut.
      CREATE tidut.      
      ASSIGN       
      SUBSTRING(tidut.UT,1) = Guru.Konstanter:gaok + " " + bytaonrmed . 
   END.

END PROCEDURE.
PROCEDURE omrade_UI :
   ASSIGN
   ototea = 0 ototber = 0 ototarb = 0 ototmask = 0 ototmtrl = 0 ototovr = 0
   ototalt = 0 obertim = 0 oarbtim = 0 omsktim = 0 oarbtim1 = 0 oarbtim2 = 0
   oarbtim3 = 0 oarbtim4 = 0 oarbtim5 = 0 ototalt1 = 0 ototalt2 = 0
   ototalt3 = 0 ototalt4 = 0 ototalt5 = 0 ototutr = 0 ototutrtim = 0.   
   CREATE tidut.
   CREATE tidut.
   EMPTY TEMP-TABLE kkod NO-ERROR. 
   RUN rubrik_UI.

   OPEN QUERY aoq FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = aoval.AONR AND
   AONRKONTKOD.DELNR = aoval.DELNR USE-INDEX AONR NO-LOCK.
   GET FIRST aoq NO-LOCK.
   DO WHILE AVAILABLE(AONRKONTKOD):
      IF AONRKONTKOD.K3 NE "" THEN DO:
         FIND FIRST KONTO WHERE KONTO.KONTO = "K3" AND 
         KONTO.KONTONR = AONRKONTKOD.K3 NO-LOCK NO-ERROR.
         CREATE kkod.
         ASSIGN
         kkod.K1 = AONRKONTKOD.K1
         kkod.K2 = AONRKONTKOD.K2
         kkod.K3 = AONRKONTKOD.K3
         kkod.K4 = AONRKONTKOD.K4
         kkod.K5 = AONRKONTKOD.K5
         kkod.SATS% = AONRKONTKOD.SATS%.
         IF AVAILABLE KONTO THEN kkod.BENAMNING = KONTO.BENAMNING.
      END.
      GET NEXT aoq NO-LOCK.
   END.
   CLOSE QUERY aoq.      
   FOR EACH kkod: 
      ASSIGN
      ktotea = 0 ktotber = 0 ktotarb = 0 ktotmask = 0 ktotmtrl = 0
      ktotovr = 0 ktotalt = 0 kbertim = 0 karbtim = 0 kmsktim = 0
      karbtim1 = 0 karbtim2 = 0 karbtim3 = 0 karbtim4 = 0
      karbtim5 = 0 ktotalt1 = 0 ktotalt2 = 0 ktotalt3 = 0
      ktotalt4 = 0 ktotalt5 = 0
      ktotutr = 0 ktotutrtim = 0 planvar = "". 
      CREATE tidut.
      FIND FIRST KBENAMNING USE-INDEX KBEN NO-LOCK NO-ERROR.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = KBENAMNING.K1 
      SUBSTRING(tidut.UT,10) = KBENAMNING.K2
      SUBSTRING(tidut.UT,20) = KBENAMNING.K4
      SUBSTRING(tidut.UT,30) = KBENAMNING.K5.     
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = kkod.K1
      SUBSTRING(tidut.UT,10) = kkod.K2
      SUBSTRING(tidut.UT,20) = kkod.K4
      SUBSTRING(tidut.UT,30) = kkod.K5.            
      RUN plannr_UI.
      RUN slutkonto_UI.
      CREATE tidut.
   END.   
   RUN slutomrade_UI.
   CREATE tidut.
END PROCEDURE.


PROCEDURE rubrik_UI :
      ASSIGN
      str=                                                                    
"=========================================.==.=====.=====.=====.=====.=====.========.========.========.=======.=======.========".         
      SUBSTRING(tidut.UT,1) = str.                     
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,46) = "BER."
      SUBSTRING(tidut.UT,52) = "ARB."
      SUBSTRING(tidut.UT,58) = "MASK"
      SUBSTRING(tidut.UT,64) = "UTR." 
      SUBSTRING(tidut.UT,76) = "ARBETS"
      SUBSTRING(tidut.UT,85) = "MATERIEL"
      SUBSTRING(tidut.UT,94) = "MASKIN"
      SUBSTRING(tidut.UT,103) = "UTRUST"
      SUBSTRING(tidut.UT,111) = "ÖVRIG".        
      CREATE tidut.            
      ASSIGN  
      SUBSTRING(tidut.UT,43) = "%"
      SUBSTRING(tidut.UT,46) = "TIM."
      SUBSTRING(tidut.UT,52) = "TIM."
      SUBSTRING(tidut.UT,58) = "TIM." 
      SUBSTRING(tidut.UT,64) = "TIM."                                                                                  
      SUBSTRING(tidut.UT,70) = "EA"
      SUBSTRING(tidut.UT,76) = "KOSTNAD"
      SUBSTRING(tidut.UT,85) = "KOSTNAD"
      SUBSTRING(tidut.UT,94) = "KOSTNAD"
      SUBSTRING(tidut.UT,103) = "KOSTNAD"
      SUBSTRING(tidut.UT,111) = "KOSTNAD"
      SUBSTRING(tidut.UT,119) = "TOTALT".                        
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.           
END PROCEDURE.

PROCEDURE sidfot_UI : 
   /*ANTAL MINUS*/
   
   IF bertim >= 0 THEN SUBSTRING(tidut.UT,46) = STRING(bertim,">>>>9").
   ELSE SUBSTRING(tidut.UT,46) = STRING(bertim,"->>>9").
   IF arbtim >= 0 THEN SUBSTRING(tidut.UT,52) = STRING(arbtim,">>>>9").
   ELSE SUBSTRING(tidut.UT,52) = STRING(arbtim,"->>>9").
   IF msktim >= 0 THEN SUBSTRING(tidut.UT,58) = STRING(msktim,">>>>9").
   ELSE SUBSTRING(tidut.UT,58) = STRING(msktim,"->>>9").
   IF totutrtim >= 0 THEN SUBSTRING(tidut.UT,64) = STRING(totutrtim,">>>>9").
   ELSE SUBSTRING(tidut.UT,64) = STRING(totutrtim,"->>>9").
   IF totea >= 0 THEN SUBSTRING(tidut.UT,70) = STRING(totea,">>>>9").
   ELSE SUBSTRING(tidut.UT,70) = STRING(totea,"->>>9").
   IF totarb >= 0 THEN SUBSTRING(tidut.UT,76) = STRING(totarb,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,76) = STRING(totarb,"->>>>>>9").
                                            
   SUBSTRING(tidut.UT,85) = STRING(totmtrl,"->>>>>>9").
   IF totmask >= 0 THEN SUBSTRING(tidut.UT,94) = STRING(totmask,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,94) = STRING(totmask,"->>>>>>9").
   IF totutr >= 0 THEN SUBSTRING(tidut.UT,103) = STRING(totutr,">>>>>>9").
   ELSE SUBSTRING(tidut.UT,103) = STRING(totutr,"->>>>>9").

   IF totovr < 0 THEN SUBSTRING(tidut.UT,111) = STRING(totovr,"->>>>>9").
   ELSE SUBSTRING(tidut.UT,111) = STRING(totovr,">>>>>>9").
   SUBSTRING(tidut.UT,119) = STRING(totalt,"->>>>>>>9").                                     
   
   ASSIGN
   ktotea = ktotea + totea
   ktotarb = ktotarb + totarb
   ktotmask = ktotmask + totmask
   ktotmtrl = ktotmtrl + totmtrl
   ktotovr = ktotovr + totovr
   ktotalt = ktotalt + totalt
   karbtim = karbtim + arbtim
   kbertim = kbertim + bertim
   kmsktim = kmsktim + msktim
   ktotutr = ktotutr + totutr
   ktotutrtim = ktotutrtim + totutrtim.
END PROCEDURE.
PROCEDURE slutkonto_UI :                                    
   ASSIGN
   ototea = ototea + ktotea
   ototarb = ototarb + ktotarb
   ototmask = ototmask + ktotmask
   ototmtrl = ototmtrl + ktotmtrl
   ototovr = ototovr + ktotovr
   ototalt = ototalt + ktotalt
   oarbtim = oarbtim + karbtim
   obertim = obertim + kbertim
   omsktim = omsktim + kmsktim
   ototutr = ototutr + ktotutr
   ototutrtim = ototutrtim + ktotutrtim.
END PROCEDURE.
PROCEDURE slutomrade_UI :
   /*ANTAL MINUS*/
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "Summa kalkyl:".   
   IF obertim >= 0 THEN SUBSTRING(tidut.UT,46) = STRING(obertim,">>>>9").
   ELSE SUBSTRING(tidut.UT,46) = STRING(obertim,"->>>9").
   IF oarbtim >= 0 THEN SUBSTRING(tidut.UT,52) = STRING(oarbtim,">>>>9").
   ELSE SUBSTRING(tidut.UT,52) = STRING(oarbtim,"->>>9").
   IF omsktim >= 0 THEN SUBSTRING(tidut.UT,58) = STRING(omsktim,">>>>9").
   ELSE SUBSTRING(tidut.UT,58) = STRING(omsktim,"->>>9").
   IF ototutrtim >= 0 THEN SUBSTRING(tidut.UT,64) = STRING(ototutrtim,">>>>9").
   ELSE SUBSTRING(tidut.UT,64) = STRING(ototutrtim,"->>>9").
   IF ototea >= 0 THEN SUBSTRING(tidut.UT,70) = STRING(ototea,">>>>9").
   ELSE SUBSTRING(tidut.UT,70) = STRING(ototea,"->>>9").
   IF ototarb >= 0 THEN SUBSTRING(tidut.UT,76) = STRING(ototarb,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,76) = STRING(ototarb,"->>>>>>9").
   SUBSTRING(tidut.UT,85) = STRING(ototmtrl,"->>>>>>9").
   IF ototmask >= 0 THEN SUBSTRING(tidut.UT,94) = STRING(ototmask,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,94) = STRING(ototmask,"->>>>>>9").
   IF ototutr >= 0 THEN SUBSTRING(tidut.UT,103) = STRING(ototutr,">>>>>>9").
   ELSE SUBSTRING(tidut.UT,103) = STRING(ototutr,"->>>>>9").
   IF ototovr < 0 THEN SUBSTRING(tidut.UT,111) = STRING(ototovr,"->>>>>9").
   ELSE SUBSTRING(tidut.UT,111) = STRING(ototovr,">>>>>>9").
   SUBSTRING(tidut.UT,119) = STRING(ototalt,"->>>>>>>9").                                     
END PROCEDURE.

PROCEDURE procset_UI:  
  IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRADATAHMT.P PERSISTENT SET edataapph.            
END PROCEDURE .

PROCEDURE tiddet_UI :
   
   DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
   DEFINE INPUT PARAMETER TABLE FOR valperstemp.                                     
   /*DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.   
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.*/
   DEFINE OUTPUT PARAMETER TABLE FOR tiddettemp.
   DEFINE VARIABLE sealltid AS LOGICAL NO-UNDO.  
   DEBUGGER:SET-BREAK(). 
   RUN kollsealltid_UI (INPUT Guru.Konstanter:globanv ,OUTPUT sealltid).
   
   FIND FIRST uppvaltemp NO-ERROR. 
   EMPTY TEMP-TABLE tiddettemp NO-ERROR.     
   OPEN QUERY ttq FOR EACH valperstemp,
   EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
   TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM AND
   TIDREGITAB.PERSONALKOD = valperstemp.PERSONALKOD AND TIDREGITAB.TIDLOG = TRUE         
   USE-INDEX AONR NO-LOCK.
   GET FIRST ttq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB): 
      {DAGTEMPBOLAG3.I}
      {ANVTIDSEKKOLL.I}
      IF sealltid = TRUE THEN tisek = TRUE.
      IF bsek = FALSE AND tisek = TRUE THEN DO:
         CREATE tiddettemp.                  
         ASSIGN
         tiddettemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
         tiddettemp.OMRADE = valperstemp.OMRADE
         tiddettemp.FORNAMN = valperstemp.FORNAMN
         tiddettemp.EFTERNAMN = valperstemp.EFTERNAMN 
         tiddettemp.AONR = TIDREGITAB.AONR 
         tiddettemp.DELNR = TIDREGITAB.DELNR 
         tiddettemp.DATUM = TIDREGITAB.DATUM
         tiddettemp.DAG = TIDREGITAB.DAG
         tiddettemp.START = TIDREGITAB.START 
         tiddettemp.SLUT = TIDREGITAB.SLUT
         tiddettemp.TOTALT = klock100(TIDREGITAB.TOTALT)
         tiddettemp.PRISTYP = TIDREGITAB.PRISTYP
         tiddettemp.PRIS = TIDREGITAB.PRIS
         tiddettemp.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG
         tiddettemp.OKOD1 = TIDREGITAB.OKOD1
         tiddettemp.OKOD2 = TIDREGITAB.OKOD2
         tiddettemp.OKOD3 = TIDREGITAB.OKOD3
         tiddettemp.OANT1 = klock100(TIDREGITAB.OANT1)
         tiddettemp.OANT2 = klock100(TIDREGITAB.OANT2)
         tiddettemp.OANT3 = klock100(TIDREGITAB.OANT3)
         tiddettemp.GODKAND = SUBSTRING(TIDREGITAB.GODKAND,1,4)
         tiddettemp.VECKOKORD = TIDREGITAB.VECKOKORD.
         tiddettemp.RESMAL = SUBSTRING(TIDREGITAB.RESMAL,1,50).      
         IF TIDREGITAB.PROGRAM NE "" THEN DO:   
            anddat = INDEX(TIDREGITAB.PROGRAM,"/",1) .
            IF anddat > 0 THEN DO:         
               tiddettemp.PROGRAM = SUBSTRING(TIDREGITAB.PROGRAM,anddat - 2,LENGTH(TIDREGITAB.PROGRAM)).
            END.
         END.       
         IF tiddettemp.OKOD1 NE "" THEN DO:
            FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = tiddettemp.OKOD1 USE-INDEX OVER NO-LOCK NO-ERROR.
            IF AVAILABLE OVERKOD  THEN DO:               
               tiddettemp.OKOD1 = OVERKOD.VILART.
               IF OVERKOD.ENKEL = "KVAL" THEN DO:
                  IF tiddettemp.OKOD2 = "" THEN DO:
                     /*FLYTTA SÅ ATT KVAL I SAMMA KOLUMN*/
                     ASSIGN
                     tiddettemp.OKOD2 = tiddettemp.OKOD1
                     tiddettemp.OANT2 = tiddettemp.OANT1
                     tiddettemp.OKOD1 = ""
                     tiddettemp.OANT1 = 0.
                  END.
                  ELSE IF tiddettemp.OKOD2 = tiddettemp.OKOD1 THEN DO:
                     ASSIGN                     
                     tiddettemp.OANT2 = tiddettemp.OANT2 + tiddettemp.OANT1
                     tiddettemp.OKOD1 = ""
                     tiddettemp.OANT1 = 0.    
                  END.   
               END.   
            END.
         END.
         IF tiddettemp.OKOD2 NE "" THEN DO:
            FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = tiddettemp.OKOD2 USE-INDEX OVER NO-LOCK NO-ERROR.
            IF AVAILABLE OVERKOD  THEN DO:
               tiddettemp.OKOD2 = OVERKOD.VILART.
            END.
         END.
         IF tiddettemp.OKOD3 NE "" THEN DO:
            FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = tiddettemp.OKOD3 USE-INDEX OVER NO-LOCK NO-ERROR.
            IF AVAILABLE OVERKOD  THEN DO:
               tiddettemp.OKOD3 = OVERKOD.VILART.
            END.
         END.      
      END.        
      GET NEXT ttq NO-LOCK.
   END.            
   OPEN QUERY fftq FOR EACH valperstemp,
   EACH TIDFEL WHERE TIDFEL.DATUM >= uppvaltemp.STARTDATUM AND 
   TIDFEL.DATUM <= uppvaltemp.SLUTDATUM AND
   TIDFEL.PERSONALKOD = valperstemp.PERSONALKOD AND TIDFEL.TIDLOG = TRUE         
   USE-INDEX AONR NO-LOCK.
   GET FIRST fftq NO-LOCK.
   DO WHILE AVAILABLE(TIDFEL): 
      {DAGTEMPBOLAG4.I} 
      {ANVTIDFSEKKOLL.I}
      
      IF bsek = FALSE AND tisek = TRUE THEN DO:     
         CREATE tiddettemp.         
         ASSIGN
         tiddettemp.PERSONALKOD = TIDFEL.PERSONALKOD
         tiddettemp.OMRADE = valperstemp.OMRADE
         tiddettemp.FORNAMN = valperstemp.FORNAMN
         tiddettemp.EFTERNAMN = valperstemp.EFTERNAMN 
         tiddettemp.AONR = TIDFEL.AONR 
         tiddettemp.DELNR = TIDFEL.DELNR 
         tiddettemp.DATUM = TIDFEL.DATUM 
         tiddettemp.START = TIDFEL.START 
         tiddettemp.SLUT = TIDFEL.SLUT.      
         tiddettemp.RESMAL = SUBSTRING(TIDFEL.RESMAL,1,50).
         IF TIDFEL.DEBET = FALSE THEN DO:      
            tidftot = ( -1 ) * klock100(TIDFEL.TOTALT).
            ASSIGN
            tiddettemp.TOTALT = tidftot.                                         
         END.
         ELSE DO:
            ASSIGN
            tiddettemp.TOTALT = klock100(TIDFEL.TOTALT).                           
         END.
         ASSIGN
         tiddettemp.RATT = "R"
         tiddettemp.PRISTYP = TIDFEL.PRISTYP
         tiddettemp.PRIS = TIDFEL.PRIS
         tiddettemp.OVERTIDUTTAG = TIDFEL.OVERTIDUTTAG
         tiddettemp.OKOD1 = TIDFEL.OKOD1
         tiddettemp.OKOD2 = TIDFEL.OKOD2
         tiddettemp.OKOD3 = TIDFEL.OKOD3
         tiddettemp.OANT1 = klock100(TIDFEL.OANT1)
         tiddettemp.OANT2 = klock100(TIDFEL.OANT2)
         tiddettemp.OANT3 = klock100(TIDFEL.OANT3)
         tiddettemp.GODKAND = SUBSTRING(TIDFEL.GODKAND,1,4)
         tiddettemp.VECKOKORD = TIDFEL.VECKOKORD.      
         IF TIDFEL.PROGRAM NE "" THEN DO:   
            anddat = INDEX(TIDFEL.PROGRAM,"/",1) .
            IF anddat > 0 THEN DO:         
               tiddettemp.PROGRAM = SUBSTRING(TIDFEL.PROGRAM,anddat - 2,LENGTH(TIDFEL.PROGRAM)).
            END.
         END.    
         tiddettemp.FELKORD = TIDFEL.FELKORD.
         IF tiddettemp.OKOD1 NE "" THEN DO:
            FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = tiddettemp.OKOD1 USE-INDEX OVER NO-LOCK NO-ERROR.
            IF AVAILABLE OVERKOD  THEN DO:
               tiddettemp.OKOD1 = OVERKOD.VILART.
               IF OVERKOD.ENKEL = "KVAL" THEN DO:
                  IF tiddettemp.OKOD2 = "" THEN DO:
                     /*FLYTTA SÅ ATT KVAL I SAMMA KOLUMN*/
                     ASSIGN
                     tiddettemp.OKOD2 = tiddettemp.OKOD1
                     tiddettemp.OANT2 = tiddettemp.OANT1
                     tiddettemp.OKOD1 = ""
                     tiddettemp.OANT1 = 0.
                  END.
                  ELSE IF tiddettemp.OKOD2 = tiddettemp.OKOD1 THEN DO:
                     ASSIGN                     
                     tiddettemp.OANT2 = tiddettemp.OANT2 + tiddettemp.OANT1
                     tiddettemp.OKOD1 = ""
                     tiddettemp.OANT1 = 0.    
                  END.   
               END.
            END.
         END.
         IF tiddettemp.OKOD2 NE "" THEN DO:
            FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = tiddettemp.OKOD2 USE-INDEX OVER NO-LOCK NO-ERROR.
            IF AVAILABLE OVERKOD  THEN DO:
               tiddettemp.OKOD2 = OVERKOD.VILART.
            END.
         END.
         IF tiddettemp.OKOD3 NE "" THEN DO:
            FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = tiddettemp.OKOD3 USE-INDEX OVER NO-LOCK NO-ERROR.
            IF AVAILABLE OVERKOD  THEN DO:
               tiddettemp.OKOD3 = OVERKOD.VILART.
            END.
         END.
      END.         
      GET NEXT fftq NO-LOCK.
   END.            
   FOR EACH tiddettemp WHERE tiddettemp.OKOD1 NE "" OR tiddettemp.OKOD2 NE "" OR tiddettemp.OKOD3 NE "" :
      EMPTY TEMP-TABLE etiddettemp NO-ERROR.  
      CREATE etiddettemp.
      BUFFER-COPY tiddettemp TO etiddettemp.
      ASSIGN
      tiddettemp.OKOD1 = ""
      tiddettemp.OKOD2 = ""
      tiddettemp.OKOD3 = ""
      tiddettemp.OANT1 = 0
      tiddettemp.OANT2 = 0
      tiddettemp.OANT3 = 0.
      IF etiddettemp.OKOD1 NE "" THEN DO: 
         FIND FIRST OVERKOD WHERE OVERKOD.VILART = etiddettemp.OKOD1 USE-INDEX OVER NO-LOCK NO-ERROR.            
         IF OVERKOD.ENKEL = "ENKE" THEN DO:
            assign
            tiddettemp.OKOD1 = etiddettemp.OKOD1
            tiddettemp.OANT1 = tiddettemp.OANT1 + etiddettemp.OANT1.                           
         END.           
         IF OVERKOD.ENKEL = "KVAL" THEN DO:            
            ASSIGN
            tiddettemp.OKOD2 = etiddettemp.OKOD1
            tiddettemp.OANT2 = tiddettemp.OANT2 + etiddettemp.OANT1.           
         END.           
      END.
      IF etiddettemp.OKOD2 NE "" THEN DO: 
         FIND FIRST OVERKOD WHERE OVERKOD.VILART = etiddettemp.OKOD2 USE-INDEX OVER NO-LOCK NO-ERROR.            
         IF OVERKOD.ENKEL = "ENKE" THEN DO:
            ASSIGN 
            tiddettemp.OKOD1 = etiddettemp.OKOD2
            tiddettemp.OANT1 = tiddettemp.OANT1 + etiddettemp.OANT2.                           
         END.           
         IF OVERKOD.ENKEL = "KVAL" THEN DO:            
            ASSIGN
            tiddettemp.OKOD2 = etiddettemp.OKOD2
            tiddettemp.OANT2 =  tiddettemp.OANT2 + etiddettemp.OANT2.           
         END.           
      END.
      IF etiddettemp.OKOD3 NE "" THEN DO: 
         FIND FIRST OVERKOD WHERE OVERKOD.VILART = etiddettemp.OKOD3 USE-INDEX OVER NO-LOCK NO-ERROR.            
         IF OVERKOD.ENKEL = "ENKE" THEN DO:
            assign
            tiddettemp.OKOD1 = etiddettemp.OKOD3
            tiddettemp.OANT1 = tiddettemp.OANT1 + etiddettemp.OANT3.                           
         END.           
         IF OVERKOD.ENKEL = "KVAL" THEN DO:            
            ASSIGN
            tiddettemp.OKOD2 = etiddettemp.OKOD3
            tiddettemp.OANT2 =  tiddettemp.OANT2 + etiddettemp.OANT3.           
         END.           
      END.
   END.

END PROCEDURE.

/*
"Kompledig 1.5"
*/
DEFINE TEMP-TABLE aonrhelp NO-UNDO
FIELD AONR AS CHARACTER
FIELD DELNR AS INTEGER
INDEX AONR AONR DELNR.
PROCEDURE tiddsum_UI :
   DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
   DEFINE INPUT PARAMETER TABLE FOR valperstemp.                                     
   DEFINE OUTPUT PARAMETER TABLE FOR tiddettemp.
   
   DEFINE VARIABLE sealltid AS LOGICAL NO-UNDO.
   DEBUGGER:SET-BREAK().
   RUN kollsealltid_UI (INPUT Guru.Konstanter:globanv ,OUTPUT sealltid).
   FIND FIRST uppvaltemp NO-ERROR.    
   OPEN QUERY ttq FOR EACH valperstemp,
   EACH TIDREGITAB WHERE TIDREGITAB.DATUM >= uppvaltemp.STARTDATUM AND 
   TIDREGITAB.DATUM <= uppvaltemp.SLUTDATUM AND
   TIDREGITAB.PERSONALKOD = valperstemp.PERSONALKOD AND TIDREGITAB.TIDLOG = TRUE         
   USE-INDEX AONR NO-LOCK.
   GET FIRST ttq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB): 
      {DAGTEMPBOLAG3.I}
      {ANVTIDSEKKOLL.I}
      
      IF bsek = FALSE AND tisek = TRUE THEN DO:
         FIND FIRST aonrhelp WHERE aonrhelp.AONR = TIDREGITAB.AONR AND aonrhelp.DELNR = TIDREGITAB.DELNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE aonrhelp THEN DO:
            CREATE aonrhelp.
            ASSIGN
            aonrhelp.AONR = TIDREGITAB.AONR
            aonrhelp.DELNR = TIDREGITAB.DELNR.
         END.
         CREATE tiddettemp.                  
         ASSIGN
         tiddettemp.PERSONALKOD = TIDREGITAB.PERSONALKOD
         tiddettemp.OMRADE = valperstemp.OMRADE
         tiddettemp.FORNAMN = valperstemp.FORNAMN
         tiddettemp.EFTERNAMN = valperstemp.EFTERNAMN 
         tiddettemp.AONR = TIDREGITAB.AONR 
         tiddettemp.DELNR = TIDREGITAB.DELNR 
         tiddettemp.TOTALT = klock100(TIDREGITAB.TOTALT)
         tiddettemp.PRISTYP = TIDREGITAB.PRISTYP
         tiddettemp.OKOD1 = TIDREGITAB.OKOD1
         tiddettemp.OKOD2 = TIDREGITAB.OKOD2
         tiddettemp.OKOD3 = TIDREGITAB.OKOD3
         tiddettemp.OANT1 = klock100(TIDREGITAB.OANT1)
         tiddettemp.OANT2 = klock100(TIDREGITAB.OANT2)
         tiddettemp.OANT3 = klock100(TIDREGITAB.OANT3).
         IF tiddettemp.OKOD1 NE "" THEN DO:
            FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = tiddettemp.OKOD1 USE-INDEX OVER NO-LOCK NO-ERROR.
            IF AVAILABLE OVERKOD  THEN DO:
               tiddettemp.OKOD1 = OVERKOD.LONKODTEXT.
            END.
         END.
         IF tiddettemp.OKOD2 NE "" THEN DO:
            FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = tiddettemp.OKOD2 USE-INDEX OVER NO-LOCK NO-ERROR.
            IF AVAILABLE OVERKOD  THEN DO:
               tiddettemp.OKOD2 = OVERKOD.LONKODTEXT.
            END.
         END.
         IF tiddettemp.OKOD3 NE "" THEN DO:
            FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = tiddettemp.OKOD3 USE-INDEX OVER NO-LOCK NO-ERROR.
            IF AVAILABLE OVERKOD  THEN DO:
               tiddettemp.OKOD3 = OVERKOD.LONKODTEXT.
            END.
         END.      
      END.        
      GET NEXT ttq NO-LOCK.
   END.
   FOR EACH tiddettemp WHERE tiddettemp.PRISTYP = "RESTID..." NO-LOCK:
      DELETE tiddettemp.
   END.
   FOR EACH aonrhelp NO-LOCK:
      FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = aonrhelp.AONR AND AONRKONTKOD.DELNR = aonrhelp.DELNR NO-LOCK:
          FOR EACH tiddettemp WHERE tiddettemp.AONR = AONRKONTKOD.AONR AND tiddettemp.DELNR = AONRKONTKOD.DELNR NO-LOCK:
             tiddettemp.K1 = AONRKONTKOD.K1.
          END.
      END.
   END.            
   FOR EACH tiddettemp WHERE tiddettemp.PRISTYP = "FRÅNVARO." NO-LOCK:
      tiddettemp.FRANVAROT = tiddettemp.TOTALT.
      tiddettemp.TOTALT = 0.
   END.   
   FOR EACH tiddettemp WHERE tiddettemp.OKOD1 NE ""  BREAK 
   BY tiddettemp.PERSONALKOD BY tiddettemp.AONR BY tiddettemp.DELNR BY tiddettemp.OKOD1:         
      ACCUMULATE 
      tiddettemp.OANT1 (TOTAL BY tiddettemp.PERSONALKOD BY tiddettemp.AONR BY tiddettemp.DELNR BY tiddettemp.OKOD1).
      IF LAST-OF(tiddettemp.OKOD1) THEN DO:
         IF (ACCUM TOTAL BY tiddettemp.OKOD1 tiddettemp.OANT1) NE 0 THEN DO:
            CREATE etiddettemp.
            BUFFER-COPY tiddettemp TO etiddettemp.
            etiddettemp.OKOD1 = tiddettemp.OKOD1.
            etiddettemp.OKOD2 = "".
            etiddettemp.OKOD3 = "".
            etiddettemp.OANT2 = 0.
            etiddettemp.OANT3 = 0.
            etiddettemp.OANT1  = (ACCUM TOTAL BY tiddettemp.OKOD1 tiddettemp.OANT1).
                                          
         END.
      END.
   END. 
   FOR EACH tiddettemp WHERE tiddettemp.OKOD2 NE ""  BREAK 
   BY tiddettemp.PERSONALKOD BY tiddettemp.AONR BY tiddettemp.DELNR BY tiddettemp.OKOD2:         
      ACCUMULATE 
      tiddettemp.OANT2 (TOTAL BY tiddettemp.PERSONALKOD BY tiddettemp.AONR BY tiddettemp.DELNR BY tiddettemp.OKOD2).
      IF LAST-OF(tiddettemp.OKOD2) THEN DO:
         IF (ACCUM TOTAL BY tiddettemp.OKOD2 tiddettemp.OANT2) NE 0 THEN DO:
            CREATE etiddettemp.
            BUFFER-COPY tiddettemp TO etiddettemp.
            etiddettemp.OKOD1 = tiddettemp.OKOD2.
            etiddettemp.OKOD2 = "".
            etiddettemp.OKOD3 = "".
            etiddettemp.OANT2 = 0.
            etiddettemp.OANT3 = 0.
            etiddettemp.OANT1  = (ACCUM TOTAL BY tiddettemp.OKOD2 tiddettemp.OANT2).
                                          
         END.
      END.
   END.  
   FOR EACH tiddettemp WHERE tiddettemp.OKOD3 NE ""  BREAK 
   BY tiddettemp.PERSONALKOD BY tiddettemp.AONR BY tiddettemp.DELNR BY tiddettemp.OKOD3:         
      ACCUMULATE 
      tiddettemp.OANT3 (TOTAL BY tiddettemp.PERSONALKOD BY tiddettemp.AONR BY tiddettemp.DELNR BY tiddettemp.OKOD3).
      IF LAST-OF(tiddettemp.OKOD3) THEN DO:
         IF (ACCUM TOTAL BY tiddettemp.OKOD3 tiddettemp.OANT3) NE 0 THEN DO:
            CREATE etiddettemp.
            BUFFER-COPY tiddettemp TO etiddettemp.
            etiddettemp.OKOD1 = tiddettemp.OKOD3.
            etiddettemp.OKOD2 = "".
            etiddettemp.OKOD3 = "".
            etiddettemp.OANT2 = 0.
            etiddettemp.OANT3 = 0.
            etiddettemp.OANT1  = (ACCUM TOTAL BY tiddettemp.OKOD3 tiddettemp.OANT3).
         END.
      END.
   END.  
   
   
   FOR EACH tiddettemp WHERE BREAK 
   BY tiddettemp.PERSONALKOD BY tiddettemp.AONR BY tiddettemp.DELNR :         
      ACCUMULATE 
      tiddettemp.TOTALT (TOTAL BY tiddettemp.PERSONALKOD BY tiddettemp.AONR BY tiddettemp.DELNR).
      ACCUMULATE 
      tiddettemp.FRANVAROT (TOTAL BY tiddettemp.PERSONALKOD BY tiddettemp.AONR BY tiddettemp.DELNR).
      IF LAST-OF(tiddettemp.DELNR) THEN DO:
         IF (ACCUM TOTAL BY tiddettemp.DELNR tiddettemp.TOTALT) = 0 AND (ACCUM TOTAL BY tiddettemp.DELNR tiddettemp.FRANVAROT) = 0 THEN.
         ELSE DO: 
            CREATE etiddettemp.
            ASSIGN 
            etiddettemp.K1 = tiddettemp.K1
            etiddettemp.PERSONALKOD = tiddettemp.PERSONALKOD
            etiddettemp.OMRADE = tiddettemp.OMRADE
            etiddettemp.FORNAMN = tiddettemp.FORNAMN
            etiddettemp.EFTERNAMN = tiddettemp.EFTERNAMN 
            etiddettemp.AONR = tiddettemp.AONR 
            etiddettemp.DELNR = tiddettemp.DELNR
            etiddettemp.TOTALT  = (ACCUM TOTAL BY tiddettemp.DELNR tiddettemp.TOTALT).
            etiddettemp.FRANVAROT  = (ACCUM TOTAL BY tiddettemp.DELNR tiddettemp.FRANVAROT).
         END.
      END.
   END.
   EMPTY TEMP-TABLE tiddettemp NO-ERROR.
   FOR EACH etiddettemp WHERE etiddettemp.OKOD1 = "" BREAK 
   BY etiddettemp.PERSONALKOD BY etiddettemp.AONR BY etiddettemp.DELNR :         
      ACCUMULATE 
      etiddettemp.TOTALT (TOTAL BY etiddettemp.PERSONALKOD BY etiddettemp.AONR BY etiddettemp.DELNR).
      ACCUMULATE 
      etiddettemp.FRANVAROT (TOTAL BY etiddettemp.PERSONALKOD BY etiddettemp.AONR BY etiddettemp.DELNR).
      IF LAST-OF(etiddettemp.DELNR) THEN DO:
         CREATE tiddettemp.
         BUFFER-COPY etiddettemp TO tiddettemp.
         ASSIGN 
         tiddettemp.TOTALT  = (ACCUM TOTAL BY etiddettemp.DELNR etiddettemp.TOTALT).
         tiddettemp.FRANVAROT  = (ACCUM TOTAL BY etiddettemp.DELNR etiddettemp.FRANVAROT).         
      END.
   END. 
   FOR EACH etiddettemp WHERE etiddettemp.OKOD1 NE "" BREAK 
   BY etiddettemp.PERSONALKOD BY etiddettemp.AONR BY etiddettemp.DELNR BY etiddettemp.OKOD1:         
      ACCUMULATE 
      etiddettemp.OANT1 (TOTAL BY etiddettemp.PERSONALKOD BY etiddettemp.AONR BY etiddettemp.DELNR BY etiddettemp.OKOD1).
      
      IF LAST-OF(etiddettemp.OKOD1) THEN DO:
         FIND FIRST tiddettemp WHERE tiddettemp.PERSONALKOD = etiddettemp.PERSONALKOD AND 
         tiddettemp.AONR = etiddettemp.AONR AND 
         tiddettemp.DELNR = etiddettemp.DELNR
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tiddettemp THEN DO:
            CREATE tiddettemp.
            ASSIGN 
            tiddettemp.K1 = etiddettemp.K1
            tiddettemp.PERSONALKOD = etiddettemp.PERSONALKOD
            tiddettemp.OMRADE = etiddettemp.OMRADE
            tiddettemp.FORNAMN = etiddettemp.FORNAMN
            tiddettemp.EFTERNAMN = etiddettemp.EFTERNAMN 
            tiddettemp.AONR = etiddettemp.AONR 
            tiddettemp.DELNR = etiddettemp.DELNR
            tiddettemp.OKOD1 = etiddettemp.OKOD1.
         END.
        
         IF etiddettemp.OKOD1 MATCHES "*ENKEL*" THEN tiddettemp.OANT1  =  tiddettemp.OANT1 + (ACCUM TOTAL BY etiddettemp.OKOD1 etiddettemp.OANT1).
         ELSE IF etiddettemp.OKOD1 = "Kompledig 1.5" THEN tiddettemp.OANT1  = tiddettemp.OANT1 + (ACCUM TOTAL BY etiddettemp.OKOD1 etiddettemp.OANT1). 
         ELSE tiddettemp.OANT2  = tiddettemp.OANT2 + (ACCUM TOTAL BY etiddettemp.OKOD1 etiddettemp.OANT1).         
      END.
   END. 
   
END PROCEDURE.

PROCEDURE kollsealltid_UI :
   DEFINE INPUT  PARAMETER ganv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER setidall AS LOGICAL NO-UNDO.
   IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   setidall = FALSE.   
   FIND FIRST ANVANDARE  WHERE ANVANDARE.ANVANDARE = ganv NO-LOCK NO-ERROR.
   IF AVAILABLE ANVANDARE THEN DO:        
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "SEALLTID"                   
      inextradatatemp.HUVUDCH = ANVANDARE.PERSONALKOD.  
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN   setidall = extradatatemp.SOKLOG[1].                        
      END.   
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   END.
END PROCEDURE.

   
