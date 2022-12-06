 /*OMRPLUTFAPPC.P*/
 {KALKYLUPP.I} 
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{REGVAR.I}

{EXTRADATA.I}
DEFINE NEW SHARED TEMP-TABLE omrkonto    
   FIELD OMRADE AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD OMREC AS RECID  
   INDEX OMRADE IS PRIMARY OMRADE ASCENDING.
   
DEFINE NEW SHARED TEMP-TABLE okod
   FIELD KONTO AS CHARACTER
   FIELD KONTONR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD INDEL AS INTEGER
   FIELD KREC AS RECID
   INDEX KNR IS PRIMARY KONTONR ASCENDING.    
   
DEFINE NEW SHARED TEMP-TABLE kkod
   FIELD KONTO AS CHARACTER
   FIELD KONTONR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD INDEL AS INTEGER
   FIELD KREC AS RECID
   INDEX KNR IS PRIMARY KONTONR ASCENDING.

DEFINE NEW SHARED TEMP-TABLE mtrl_temp2   
   {MTRLTEMP2TT.I}

DEFINE NEW SHARED TEMP-TABLE tempupp   
    FIELD totea      AS DECIMAL    
    FIELD totarb     AS DECIMAL    
    FIELD totmask    AS DECIMAL    
    FIELD totmtrl    AS DECIMAL    
    FIELD totovr     AS DECIMAL    
    FIELD totalt     AS DECIMAL    
    FIELD arbtim     AS DECIMAL    
    FIELD bertim     AS DECIMAL    
    FIELD msktim     AS DECIMAL    
    FIELD totutr     AS DECIMAL    
    FIELD totutrtim  AS DECIMAL        
    FIELD ototea     AS DECIMAL 
    FIELD ototber    AS DECIMAL
    FIELD ototarb    AS DECIMAL         
    FIELD ototmask   AS DECIMAL    
    FIELD ototmtrl   AS DECIMAL    
    FIELD ototovr    AS DECIMAL    
    FIELD ototalt    AS DECIMAL    
    FIELD oarbtim    AS DECIMAL    
    FIELD obertim    AS DECIMAL    
    FIELD omsktim    AS DECIMAL    
    FIELD ototutr    AS DECIMAL    
    FIELD ototutrtim AS DECIMAL.    
     

{KALKTEMP.I} 
{LISTDEF.I} 
DEFINE INPUT PARAMETER planvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER franar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER monpris AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER xtypmtrl AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tempupp.
DEFINE INPUT PARAMETER TABLE FOR omrkonto.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
DEFINE INPUT PARAMETER forevar AS CHARACTER NO-UNDO.

DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO. 
DEFINE VARIABLE offert AS LOGICAL NO-UNDO.
DEFINE VARIABLE emask3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE berpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE region AS LOGICAL NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.

FIND FIRST omrkonto NO-LOCK NO-ERROR.
FIND FIRST tempupp NO-LOCK NO-ERROR.
EMPTY TEMP-TABLE tidut NO-ERROR. 


RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).

/*HUVUD*/                                             
CREATE tidut.
SUBSTRING(tidut.UT,1) = "RAPPORT: " + CAPS(Guru.Konstanter:gomrk) + " - " + CAPS(Guru.Konstanter:gplk).        
ASSIGN         
SUBSTRING(tidut.UT,40) = STRING(TODAY)
SUBSTRING(tidut.UT,50) = STRING(TIME,"HH:MM:SS").
CREATE tidut.
SUBSTRING(tidut.UT,1) = "PERIOD :".
SUBSTRING(tidut.UT,10) = STRING(franar).
CREATE tidut.
CREATE tidut.
SUBSTRING(tidut.UT,1) = "==================================================================================================================================".
       
RUN omrade_UI.
{KALKYLUPPSUMMAPLAN.I} 
{TEMPUPPTT.I}
PROCEDURE omrade_UI.

   FOR EACH omrkonto:
      ASSIGN
      tempupp.ototea = 0 tempupp.ototber = 0 tempupp.ototarb = 0 tempupp.ototmask = 0 tempupp.ototmtrl = 0 tempupp.ototovr = 0
      tempupp.ototalt = 0 tempupp.obertim = 0 tempupp.oarbtim = 0 tempupp.omsktim = 0 tempupp.ototutr = 0 tempupp.ototutrtim = 0.
      CREATE tidut.                                
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrk) + "  : " + omrkonto.NAMN.
      CREATE tidut.
      CREATE tidut.
      RUN rubrik_UI. 
      planvar = "". 
      OPEN QUERY kq FOR EACH PLANNRTAB WHERE PLANNRTAB.OMRADE = omrkonto.OMRADE
      AND PLANNRTAB.ARTAL = franar NO-LOCK BY PLANNRTAB.PLANNR.
      GET FIRST kq NO-LOCK.
      DO WHILE AVAILABLE(PLANNRTAB):  
         IF AVAILABLE FASTSPEC THEN RELEASE FASTSPEC.
         IF planvar NE PLANNRTAB.PLANNR THEN DO:
            CREATE tidut. 
            planvar = PLANNRTAB.PLANNR.               
         END.
         ASSIGN
         SUBSTRING(tidut.UT,15) = PLANNRTAB.PLANNR
         SUBSTRING(tidut.UT,22) = SUBSTRING(PLANNRTAB.ORT,1,20).
         IF PLANNRTAB.PLANNRAVDATUM NE 01/01/91 THEN SUBSTRING(tidut.UT,43) = "J".
         ELSE SUBSTRING(tidut.UT,43) = "N".
         EMPTY TEMP-TABLE kalk_temp NO-ERROR.          
         IF PLANNRTAB.UPP = TRUE AND PLANNRTAB.UPPNR = FALSE THEN DO:
            FIND FIRST KALKAONR WHERE KALKAONR.PLANNR = PLANNRTAB.PLANNR AND
            KALKAONR.ARTAL = PLANNRTAB.ARTAL - 1 AND KALKAONR.TYP = 2 AND 
            KALKAONR.STATUSNIV = "UF"
            USE-INDEX PLANR NO-LOCK NO-ERROR.
            IF AVAILABLE KALKAONR THEN DO:
               RUN kalkupp_UI (INPUT KALKAONR.KALKNR,INPUT KALKAONR.OMRADE).
            END.
         END.
         ELSE DO: 
            FIND FIRST KALKAONR WHERE KALKAONR.PLANNR = PLANNRTAB.PLANNR AND
            KALKAONR.ARTAL = PLANNRTAB.ARTAL AND KALKAONR.TYP = 2 AND 
            KALKAONR.STATUSNIV = "UF"
            USE-INDEX PLANR NO-LOCK NO-ERROR.
            IF AVAILABLE KALKAONR THEN DO:
               RUN kalkupp_UI (INPUT KALKAONR.KALKNR,INPUT KALKAONR.OMRADE).
            END.
         END. 
         IF AVAILABLE KALKAONR THEN DO:
            RUN summeringkalkplan_UI. 
            RUN tempuppsum_UI.
            SUBSTRING(tidut.UT,47) = "2".  
            RUN sidfot_UI.
         END.  
         
         ELSE DO:
            IF PLANNRTAB.UPP = TRUE AND PLANNRTAB.UPPNR = FALSE THEN DO:
               FIND FIRST KALKAONR WHERE KALKAONR.PLANNR = PLANNRTAB.PLANNR AND
               KALKAONR.ARTAL = PLANNRTAB.ARTAL - 1 AND KALKAONR.TYP = 1 AND 
               KALKAONR.STATUSNIV = "UF"
               USE-INDEX PLANR NO-LOCK NO-ERROR.
               IF AVAILABLE KALKAONR THEN DO:
                  RUN kalkupp_UI (INPUT KALKAONR.KALKNR,INPUT KALKAONR.OMRADE).
               END.
            END.
            ELSE DO:                  
               FIND FIRST KALKAONR WHERE KALKAONR.PLANNR = PLANNRTAB.PLANNR AND
               KALKAONR.ARTAL = PLANNRTAB.ARTAL AND KALKAONR.TYP = 1 AND 
               KALKAONR.STATUSNIV = "UF"
               USE-INDEX PLANR NO-LOCK NO-ERROR.
               IF AVAILABLE KALKAONR THEN DO:                     
                  RUN kalkupp_UI (INPUT KALKAONR.KALKNR,INPUT KALKAONR.OMRADE).
               END.
            END.   
            IF AVAILABLE KALKAONR THEN DO:
               RUN summeringkalkplan_UI.
               RUN tempuppsum_UI.   
               SUBSTRING(tidut.UT,47) = "1".
               RUN sidfot_UI.
            END.                 
         END.
         GET NEXT kq NO-LOCK.
      END.   
      CLOSE QUERY kq.      
      RUN slutomrade_UI.
      CREATE tidut.
   END.   
END PROCEDURE.

PROCEDURE slutomrade_UI. 
   /*ANTAL MINUS*/
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "SUMMA " + CAPS(Guru.Konstanter:gomrk) + ":".   
   IF tempupp.obertim >= 0 THEN SUBSTRING(tidut.UT,50) = STRING(tempupp.obertim,">>>>9").
   ELSE SUBSTRING(tidut.UT,50) = STRING(tempupp.obertim,"->>>9").
   IF tempupp.oarbtim >= 0 THEN SUBSTRING(tidut.UT,56) = STRING(tempupp.oarbtim,">>>>9").
   ELSE SUBSTRING(tidut.UT,56) = STRING(tempupp.oarbtim,"->>>9").
   IF tempupp.omsktim >= 0 THEN SUBSTRING(tidut.UT,62) = STRING(tempupp.omsktim,">>>>9").
   ELSE SUBSTRING(tidut.UT,62) = STRING(tempupp.omsktim,"->>>9").
   IF tempupp.ototutrtim >= 0 THEN SUBSTRING(tidut.UT,68) = STRING(tempupp.ototutrtim,">>>>9").
   ELSE SUBSTRING(tidut.UT,68) = STRING(tempupp.ototutrtim,"->>>9").
   IF tempupp.ototea >= 0 THEN SUBSTRING(tidut.UT,74) = STRING(tempupp.ototea,">>>>9").
   ELSE SUBSTRING(tidut.UT,74) = STRING(tempupp.ototea,"->>>9").
   IF tempupp.ototarb >= 0 THEN SUBSTRING(tidut.UT,80) = STRING(tempupp.ototarb,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,80) = STRING(tempupp.ototarb,"->>>>>>9").
   IF tempupp.ototmtrl >= 0 THEN SUBSTRING(tidut.UT,89) = STRING(tempupp.ototmtrl,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,89) = STRING(tempupp.ototmtrl,"->>>>>>9").
   IF tempupp.ototmask >= 0 THEN SUBSTRING(tidut.UT,98) = STRING(tempupp.ototmask,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,98) = STRING(tempupp.ototmask,"->>>>>>9").
   IF tempupp.ototutr >= 0 THEN SUBSTRING(tidut.UT,107) = STRING(tempupp.ototutr,">>>>>>9").
   ELSE SUBSTRING(tidut.UT,107) = STRING(tempupp.ototutr,"->>>>>9").
   IF tempupp.ototovr >= 0 THEN SUBSTRING(tidut.UT,115) = STRING(tempupp.ototovr,">>>>>>9").
   ELSE SUBSTRING(tidut.UT,115) = STRING(tempupp.ototovr,"->>>>>9").
   IF tempupp.ototalt >= 0 THEN SUBSTRING(tidut.UT,123) = STRING(tempupp.ototalt,">>>>>>>9").                                     
   ELSE SUBSTRING(tidut.UT,123) = STRING(tempupp.ototalt,"->>>>>>9").                                        
END PROCEDURE.

PROCEDURE sidfot_UI.   
   /*ANTAL MINUS*/
   IF tempupp.bertim >= 0 THEN SUBSTRING(tidut.UT,50) = STRING(tempupp.bertim,">>>>9").
   ELSE SUBSTRING(tidut.UT,50) = STRING(tempupp.bertim,"->>>9").
   IF tempupp.arbtim >= 0 THEN SUBSTRING(tidut.UT,56) = STRING(tempupp.arbtim,">>>>9").
   ELSE SUBSTRING(tidut.UT,56) = STRING(tempupp.arbtim,"->>>9").
   IF tempupp.msktim >= 0 THEN SUBSTRING(tidut.UT,62) = STRING(tempupp.msktim,">>>>9").
   ELSE SUBSTRING(tidut.UT,62) = STRING(tempupp.msktim,"->>>9").
   IF tempupp.totutrtim >= 0 THEN SUBSTRING(tidut.UT,68) = STRING(tempupp.totutrtim,">>>>9").
   ELSE SUBSTRING(tidut.UT,68) = STRING(tempupp.totutrtim,"->>>9").
   IF tempupp.totea >= 0 THEN SUBSTRING(tidut.UT,74) = STRING(tempupp.totea,">>>>9").
   ELSE SUBSTRING(tidut.UT,74) = STRING(tempupp.totea,"->>>9").
   IF tempupp.totarb >= 0 THEN SUBSTRING(tidut.UT,80) = STRING(tempupp.totarb,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,80) = STRING(tempupp.totarb,"->>>>>>9").
   IF tempupp.totmtrl >= 0 THEN SUBSTRING(tidut.UT,89) = STRING(tempupp.totmtrl,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,89) = STRING(tempupp.totmtrl,"->>>>>>9").
   IF tempupp.totmask >= 0 THEN SUBSTRING(tidut.UT,98) = STRING(tempupp.totmask,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,98) = STRING(tempupp.totmask,"->>>>>>9").
   IF tempupp.totutr >= 0 THEN SUBSTRING(tidut.UT,107) = STRING(tempupp.totutr,">>>>>>9").
   ELSE SUBSTRING(tidut.UT,107) = STRING(tempupp.totutr,"->>>>>9").
   IF tempupp.totovr >= 0 THEN SUBSTRING(tidut.UT,115) = STRING(tempupp.totovr,">>>>>>9").
   ELSE SUBSTRING(tidut.UT,115) = STRING(tempupp.totovr,"->>>>>9").
   IF tempupp.totalt >= 0 THEN SUBSTRING(tidut.UT,123) = STRING(tempupp.totalt,">>>>>>>9").                                     
   ELSE SUBSTRING(tidut.UT,123) = STRING(tempupp.totalt,"->>>>>>9").                                     
   
   
   ASSIGN
   tempupp.ototea = tempupp.ototea + tempupp.totea
   tempupp.ototarb = tempupp.ototarb + tempupp.totarb
   tempupp.ototmask = tempupp.ototmask + tempupp.totmask
   tempupp.ototmtrl = tempupp.ototmtrl + tempupp.totmtrl
   tempupp.ototovr = tempupp.ototovr + tempupp.totovr
   tempupp.ototalt = tempupp.ototalt + tempupp.totalt
   tempupp.oarbtim = tempupp.oarbtim + tempupp.arbtim
   tempupp.obertim = tempupp.obertim + tempupp.bertim
   tempupp.omsktim = tempupp.omsktim + tempupp.msktim
   tempupp.ototutr = tempupp.ototutr + tempupp.totutr
   tempupp.ototutrtim = tempupp.ototutrtim + tempupp.totutrtim.            
END PROCEDURE.


PROCEDURE rubrik_UI.    
      ASSIGN
      str=                                                                    
"=============.======.====================.==.===.=====.=====.=====.=====.=====.========.========.========.=======.=======.========".         
      SUBSTRING(tidut.UT,1) = str.                     
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,50) = "BER."
      SUBSTRING(tidut.UT,56) = "ARB."
      SUBSTRING(tidut.UT,62) = "MASK"
      SUBSTRING(tidut.UT,68) = "UTR." 
      SUBSTRING(tidut.UT,80) = "ARBETS"
      SUBSTRING(tidut.UT,89) = "MATERIEL"
      SUBSTRING(tidut.UT,98) = "MASKIN"
      SUBSTRING(tidut.UT,107) = "UTRUST"
      SUBSTRING(tidut.UT,115) = "ÖVRIG".        
      CREATE tidut.            
      ASSIGN  
      SUBSTRING(tidut.UT,15) = CAPS(Guru.Konstanter:gplk)                                                                                   
      SUBSTRING(tidut.UT,22) = "BENÄMNING"
      SUBSTRING(tidut.UT,43) = "AV"
      SUBSTRING(tidut.UT,46) = "TYP"
      SUBSTRING(tidut.UT,50) = "TIM."
      SUBSTRING(tidut.UT,56) = "TIM."
      SUBSTRING(tidut.UT,62) = "TIM." 
      SUBSTRING(tidut.UT,68) = "TIM."                                                                                  
      SUBSTRING(tidut.UT,74) = "EA"
      SUBSTRING(tidut.UT,80) = "KOSTNAD"
      SUBSTRING(tidut.UT,89) = "KOSTNAD"
      SUBSTRING(tidut.UT,98) = "KOSTNAD"
      SUBSTRING(tidut.UT,107) = "KOSTNAD"
      SUBSTRING(tidut.UT,115) = "KOSTNAD"
      SUBSTRING(tidut.UT,123) = "TOTALT".                        
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.           
END PROCEDURE.



PROCEDURE berkalk_UI.
   totmtrl = 0.   
   IF KALKAONR.AONR NE ? THEN DO:
      FIND FIRST BEREDNING WHERE BEREDNING.AONR = KALKAONR.AONR AND 
      BEREDNING.DELNR = KALKAONR.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE BEREDNING THEN DO:                  
         RUN LISTPROG.P (INPUT BEREDNING.BERAONR,INPUT BEREDNING.OMRADE,
         OUTPUT TABLE mtrl_temp, OUTPUT TABLE lin_upp,OUTPUT TABLE lin_temp).
      END.
      EMPTY TEMP-TABLE mtrl_temp2 NO-ERROR.       
      FOR EACH mtrl_temp BREAK BY mtrl_temp.ENR: 
         ACCUMULATE mtrl_temp.TOTPRIS (TOTAL BY mtrl_temp.ENR). 
         ACCUMULATE mtrl_temp.ANTAL (TOTAL BY mtrl_temp.ENR).       
         IF LAST-OF(mtrl_temp.ENR) THEN DO:
            CREATE mtrl_temp2.
            ASSIGN 
            mtrl_temp2.ENR = mtrl_temp.ENR
            mtrl_temp2.BENAMNING = mtrl_temp.BENAMNING 
            mtrl_temp2.ENHET = mtrl_temp.ENHET 
            mtrl_temp2.PRIS = mtrl_temp.PRIS
            mtrl_temp2.TOTPRIS = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.TOTPRIS)                       
            mtrl_temp2.ANTAL = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.ANTAL).                                                        
         END.     
      END.    
      FOR EACH lin_upp:
         FIND FIRST mtrl_temp2 WHERE mtrl_temp2.ENR = lin_upp.ENR NO-LOCK NO-ERROR.
         IF AVAILABLE mtrl_temp2 THEN DO:                      
            ASSIGN
            mtrl_temp2.ANTAL = mtrl_temp2.ANTAL + lin_upp.TOTMETER
            mtrl_temp2.TOTPRIS = mtrl_temp2.TOTPRIS + lin_upp.TOTPRIS.
         END.
         ELSE DO:                    
            CREATE mtrl_temp2.
            ASSIGN 
            mtrl_temp2.ENR = lin_upp.ENR
            mtrl_temp2.BENAMNING = lin_upp.BENAMNING 
            mtrl_temp2.ENHET = lin_upp.ENHET 
            mtrl_temp2.PRIS = lin_upp.PRIS
            mtrl_temp2.TOTPRIS = lin_upp.TOTPRIS                       
            mtrl_temp2.ANTAL = lin_upp.TOTMETER.
         END.
      END.
      FOR EACH mtrl_temp2:
         totmtrl = totmtrl + mtrl_temp2.TOTPRIS.
      END.  
   END.
END PROCEDURE.


PROCEDURE procset_UI:  
  IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRADATAHMT.P PERSISTENT SET edataapph.            
END PROCEDURE .
