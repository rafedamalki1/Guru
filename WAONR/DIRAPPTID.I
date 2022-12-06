
/*------------------------------------------------------------------------
    File        : DIRAPPTID.I
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

PROCEDURE aodhuvud_UI :
   DEFINE INPUT PARAMETE aonummer AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnummer AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER valdelnrlog AS LOGICAL NO-UNDO.
   IF RAD_PERIOD = 1 THEN avdatum = DATE(12,31,YEAR(bdatum)).   
   DO TRANSACTION:   
      CREATE tidut.                           
      ASSIGN
      tidut.UT = CAPS(Guru.Konstanter:gaol) + "-TID-DETALJERAT PERIOD"               
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


PROCEDURE aodsumma_UI :
   DEFINE INPUT PARAMETER aonummer AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnummer AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER RAD_PERIOD AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER inder AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
   /*DEFINE OUTPUT PARAMETER TABLE FOR tidut  .*/
   EMPTY TEMP-TABLE bytaao NO-ERROR.
   EMPTY TEMP-TABLE tiddettemp  NO-ERROR.  
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
      IF bsek = FALSE AND tisek = TRUE  THEN DO:         
         
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
      IF bsek = FALSE AND tisek = TRUE  THEN DO:
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
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + TIDFEL.PERSONALKOD.
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
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + tiddettemp.PERSONALKOD.    
   END.

END PROCEDURE.
