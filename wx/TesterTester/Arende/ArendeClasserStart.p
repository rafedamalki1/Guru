/*ArendeClasserStart.p*/
{ARENDEKAT.I}
{ARENDEALLTEMPC.I}
{ARENDEUPPTT.I}
{HAMTAVDJUDEF.I}
{GATILL.I}
{PTEMP.I}
{LOPTEMP.I}
{PTEMPLOPTEMP.I}
{KBETEMP.I}
DEFINE TEMP-TABLE extravaldfasttemp NO-UNDO LIKE valdfasttemp.
{ARENDEPRODATA.i}
DEFINE VARIABLE startArenderoot AS Guru.Root NO-UNDO.
DEFINE VARIABLE startArendedb   AS Modules.Arende.Arendedb NO-UNDO.
DEFINE VARIABLE Root          AS Guru.Root NO-UNDO.
DEFINE VARIABLE LocalAppServerHandle  AS HANDLE NO-UNDO.
DEFINE VARIABLE LocalAppServerExtraHandle AS HANDLE NO-UNDO. 
DEFINE VARIABLE AppservControll       AS HANDLE  NO-UNDO.
DEFINE VARIABLE ArendeimportTTh AS HANDLE NO-UNDO.
DEFINE VARIABLE extravaldfastth AS HANDLE NO-UNDO.
ArendeimportTTh = TEMP-TABLE ArendeimportTT:HANDLE:DEFAULT-BUFFER-HANDLE.
extravaldfastth = TEMP-TABLE extravaldfasttemp:HANDLE:DEFAULT-BUFFER-HANDLE. 
/*ny kalkyl*/ 

PROCEDURE NyArende_UI:
  DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER TABLE FOR extravaldfasttemp. 
  DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
  
  EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
  RUN StartCon_UI. 
  FIND FIRST extravaldfasttemp WHERE NO-LOCK NO-ERROR.
  IF NOT AVAILABLE extravaldfasttemp THEN DO:
     startArenderoot:andraArende(OUTPUT kalknr,OUTPUT omrvar,OUTPUT AppservControll).
  END.
  ELSE DO:
     
     startArenderoot:andraArende(OUTPUT kalknr,OUTPUT omrvar,INPUT extravaldfastth,OUTPUT AppservControll).
  END.   
  IF VALID-HANDLE(AppservControll) THEN DO:
     RUN AnvKalkylHmt_UI IN AppservControll (OUTPUT TABLE anvkalkyltt).
     RUN AnvkalKylBort_UI IN AppservControll.
  END.   
  RUN Avsluta_UI.  
  IF kalknr NE 0 THEN DO:
      /*hämta den nya kalkylen*/
      RUN ConArendedb_UI.
      /*hämta de ändrade kalkylerna*/
      FOR EACH anvkalkyltt WHERE NO-LOCK:
         RUN SokKalkylOmr_UI IN LocalAppServerHandle (INPUT anvkalkyltt.ARENDENR,INPUT anvkalkyltt.OMRADE,OUTPUT TABLE eutvaldfasttemp APPEND ).
      END.
      RUN DisConArendedb_UI.     
   END. 
    
END PROCEDURE.
/*ändra kalkyl*/

PROCEDURE AndraArende_UI :
   DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   EMPTY TEMP-TABLE ArendeimportTT NO-ERROR. 
    RUN StartCon_UI. 
  
   startArenderoot:andraArende(INPUT ArendeimportTTh,INPUT-OUTPUT kalknr,INPUT-OUTPUT omrvar,OUTPUT AppservControll).
   IF VALID-HANDLE(AppservControll) THEN DO: 
      RUN AnvKalkylHmt_UI IN AppservControll (OUTPUT TABLE anvkalkyltt).
      RUN AnvkalKylBort_UI IN AppservControll.
   END.   
   RUN Avsluta_UI.
   RUN ConArendedb_UI.
  
   /*hämta de ändrade kalkylerna*/
   FOR EACH anvkalkyltt WHERE NO-LOCK:
      RUN sokkalkylomr_UI IN LocalAppServerHandle (INPUT anvkalkyltt.ARENDENR,INPUT anvkalkyltt.OMRADE ,OUTPUT TABLE eutvaldfasttemp APPEND).
   END.   
   
   RUN DisConArendedb_UI. 
END PROCEDURE.   
/*bort kalkyl*/
PROCEDURE bortArende_UI :
   DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER ejbortkalkvar AS LOGICAL NO-UNDO.
   DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
   RUN ConArendedb_UI.
   RUN KalkylBort_UI IN LocalAppServerHandle (INPUT kalknr,OUTPUT felmedd).
   IF felmedd = "" THEN.
   ELSE DO:
      MESSAGE felmedd
      VIEW-AS ALERT-BOX.
      ejbortkalkvar = TRUE.
   END.       
   RUN DisConArendedb_UI.  
   
END PROCEDURE.  
PROCEDURE VisaArendStatus:
   DEFINE INPUT PARAMETER TABLE FOR extravaldfasttemp.
   FOR EACH extravaldfasttemp WHERE extravaldfasttemp.STATUSNIV NE "ÄRENDE" NO-LOCK:
      DELETE extravaldfasttemp.
   END.
   FIND FIRST extravaldfasttemp WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE extravaldfasttemp THEN RETURN. 
   RUN StartCon_UI.
   startArenderoot:VisaArendStatus(INPUT extravaldfasttemp.KALKNR,INPUT extravaldfasttemp.OMRADE, INPUT extravaldfastth,OUTPUT AppservControll).
   RUN Avsluta_UI.  
  
END PROCEDURE. 
PROCEDURE ArendeSet_UI :
   DEFINE INPUT  PARAMETER arendekalkin AS CHARACTER NO-UNDO.
   Guru.GlobalaVariabler:arendekalk = arendekalkin.
   IF Guru.GlobalaVariabler:arendekalk = "ÄRENDE" THEN Guru.GlobalaVariabler:KalkArendeText = "Ärende".
   IF Guru.GlobalaVariabler:arendekalk = "KAL" THEN Guru.GlobalaVariabler:KalkArendeText =  "Kalkyl".
END PROCEDURE.
PROCEDURE ArendeGet_UI :
   DEFINE INPUT  PARAMETER arendekalkout AS CHARACTER NO-UNDO.
   arendekalkout = Guru.GlobalaVariabler:arendekalk.
END PROCEDURE.
/*hamtar kalkyler till aonret*/
PROCEDURE AonrHmtArende_UI :
   DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER aonrplanin AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR gatill.
   EMPTY TEMP-TABLE gatill NO-ERROR. 
   RUN ConArendedb_UI.
   RUN AonrHmtKalk_UI IN LocalAppServerHandle (INPUT Guru.GlobalaVariabler:arendekalk,INPUT aonrvar, INPUT delnrvar,INPUT aonrplanin,OUTPUT TABLE gatill).    
   RUN DisConArendedb_UI.  
    
END PROCEDURE.
/*sök kalkyler på urval*/
PROCEDURE UrvalArende_UI :
   DEFINE INPUT  PARAMETER TABLE FOR uppkalktemp.
   DEFINE INPUT PARAMETER TABLE FOR uppavdjud.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR.  
   RUN ConArendedb_UI.
   RUN UrvalArende_UI IN LocalAppServerHandle (INPUT Guru.GlobalaVariabler:arendekalk,INPUT TABLE uppkalktemp,INPUT TABLE uppavdjud, OUTPUT TABLE eutvaldfasttemp).
   RUN UrvalBort_UI IN LocalAppServerHandle. 
   RUN DisConArendedb_UI.
END PROCEDURE.  
/*sök på kalknr*/
PROCEDURE SokArende_UI :
   DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   RUN ConArendedb_UI.
   RUN SokKalkyl_UI IN LocalAppServerHandle (INPUT Guru.GlobalaVariabler:arendekalk,INPUT kalknr,OUTPUT TABLE eutvaldfasttemp).
   RUN UrvalBort_UI IN LocalAppServerHandle. 
   RUN DisConArendedb_UI.  
END PROCEDURE.  
/*sök  på aonr*/
PROCEDURE SokAonrArende_UI :
   DEFINE INPUT  PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER allavar AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER skapatom AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   RUN ConArendedb_UI.
   RUN SokAonrKalkyl_UI IN LocalAppServerHandle (INPUT Guru.GlobalaVariabler:arendekalk,INPUT aonrvar, INPUT delnrvar,INPUT allavar,INPUT skapatom,OUTPUT TABLE eutvaldfasttemp).
   RUN UrvalBort_UI IN LocalAppServerHandle. 
    
   RUN DisConArendedb_UI.
END PROCEDURE.
/*fix valista efter aonr*/
PROCEDURE LaddaTypBen_UI :
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valdfasttemp.
   RUN ConArendedb_UI.
   RUN LaddaTypBen_UI IN LocalAppServerHandle (INPUT-OUTPUT TABLE valdfasttemp).
   RUN UrvalBort_UI IN LocalAppServerHandle.    
   RUN DisConArendedb_UI.
END PROCEDURE.
/*visa direkt*/
PROCEDURE ArendeVisa_UI :
   DEFINE INPUT PARAMETER TABLE FOR extravaldfasttemp.
   DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
   RUN ConArendedb_UI.
   RUN Arendevisakoll_UI IN LocalAppServerHandle (INPUT-OUTPUT TABLE extravaldfasttemp,OUTPUT felmedd).
   IF felmedd = "" THEN.
   ELSE DO:
      MESSAGE felmedd
      VIEW-AS ALERT-BOX.
   END.
   FIND FIRST extravaldfasttemp WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE extravaldfasttemp THEN DO:
      MESSAGE "Du kan inte visa någon!"
      VIEW-AS ALERT-BOX.
   END.    
   RUN DisConArendedb_UI.
   RUN StartCon_UI.
   
   startArenderoot:VisaArende(INPUT extravaldfastth).
   RUN Avsluta_UI. 
END PROCEDURE.

/*fix favo*/
PROCEDURE ArendeFavo_UI :
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valdfasttemp.
   RUN ConArendedb_UI.
   RUN Kalkfavo_UI IN LocalAppServerHandle (INPUT-OUTPUT TABLE valdfasttemp).
   RUN UrvalBort_UI IN LocalAppServerHandle.    
   RUN DisConArendedb_UI.
END PROCEDURE.
/*vilket pris till uppf*/
PROCEDURE GetArendeUpp_UI :
   DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER xtypmtrl AS INTEGER NO-UNDO.
   RUN ConArendedb_UI.
   RUN GetKalkUpp_UI IN LocalAppServerHandle (INPUT kalknr, OUTPUT xtypmtrl).
   RUN DisConArendedb_UI.
END PROCEDURE.
  
    
/*Start kalkyl objectorientering*/
PROCEDURE StartCon_UI :
   startArenderoot = NEW Guru.Root().
   startArenderoot:startArendedb().
END PROCEDURE.
PROCEDURE Avsluta_UI :
   DELETE OBJECT startArenderoot NO-ERROR.
END PROCEDURE. 
/*con av db då ej oo*/
PROCEDURE ConArendedb_UI:
   IF Guru.Konstanter:appcon THEN DO:
      RUN ARENDEBERAPPDS.p PERSISTENT SET LocalAppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
   END.
   ELSE DO:
      RUN ARENDEBERAPPDS.p PERSISTENT SET LocalAppServerHandle (INPUT Guru.Konstanter:globanv).
   END.
END PROCEDURE.
/* discon av db då ej oo*/
PROCEDURE DisConArendedb_UI :
    IF VALID-HANDLE(LocalAppServerHandle) THEN DELETE PROCEDURE LocalAppServerHandle.
END PROCEDURE.  
  

   