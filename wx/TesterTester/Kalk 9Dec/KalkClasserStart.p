/*KalkClasserStart.p*/
{KALKYLKAT.I}
{KALKALLTEMPC.I}
{KALKUPPTT.I}
{HAMTAVDJUDEF.I}
{GATILL.I}
{PTEMP.I}
{LOPTEMP.I}
{PTEMPLOPTEMP.I}
{KBETEMP.I}
DEFINE TEMP-TABLE extravaldfasttemp NO-UNDO LIKE valdfasttemp.
{KALKYLPRODATA.i}
DEFINE VARIABLE startkalkroot AS Guru.Root NO-UNDO.
DEFINE VARIABLE startkalkdb   AS Modules.Kalkyl.Kalkyldb NO-UNDO.
DEFINE VARIABLE Root          AS Guru.Root NO-UNDO.
DEFINE VARIABLE LocalAppServerHandle  AS HANDLE NO-UNDO.
DEFINE VARIABLE LocalAppServerExtraHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE AppservControll       AS HANDLE  NO-UNDO.
DEFINE VARIABLE KalkylimportTTh AS HANDLE NO-UNDO.
DEFINE VARIABLE extravaldfastth AS HANDLE NO-UNDO.
KalkylimportTTh = TEMP-TABLE KalkylimportTT:HANDLE:DEFAULT-BUFFER-HANDLE.
extravaldfastth = TEMP-TABLE extravaldfasttemp:HANDLE:DEFAULT-BUFFER-HANDLE. 
/*adm kalkyl*/
PROCEDURE Kalkyladm_UI:
   DEBUGGER:SET-BREAK().
   RUN StartConAdm_UI.
   startkalkroot:KalkylAdm(OUTPUT AppservControll).
   RUN Avsluta_UI.  
   
END PROCEDURE.  
/*ny kalkyl*/ 


PROCEDURE Ny_UI:
  DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER TABLE FOR extravaldfasttemp. 
  DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
  EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
  RUN StartCon_UI.
  FIND FIRST extravaldfasttemp WHERE NO-LOCK NO-ERROR.
  IF NOT AVAILABLE extravaldfasttemp THEN DO:
     startkalkroot:andrakalkyl(OUTPUT kalknr,OUTPUT omrvar,OUTPUT AppservControll).
    
  END.
  ELSE DO:
     startkalkroot:andrakalkyl(OUTPUT kalknr,OUTPUT omrvar,INPUT extravaldfastth,OUTPUT AppservControll).
  
  END.   
  IF VALID-HANDLE(AppservControll) THEN DO:
     RUN AnvKalkylHmt_UI IN AppservControll (OUTPUT TABLE anvkalkyltt).
     RUN AnvkalKylBort_UI IN AppservControll.
  END.   
  RUN Avsluta_UI.  
  IF kalknr NE 0 THEN DO:
      /*hämta den nya kalkylen*/
      RUN ConKalkyldb_UI.
      /*hämta de ändrade kalkylerna*/
      FOR EACH anvkalkyltt WHERE NO-LOCK:
         RUN SokKalkylOmr_UI IN LocalAppServerHandle (INPUT anvkalkyltt.KALKNR,INPUT anvkalkyltt.OMRADE,OUTPUT TABLE eutvaldfasttemp APPEND ).
      END.
      RUN DisConKalkyldb_UI.     
   END. 
    
END PROCEDURE.
/*ändra kalkyl*/

PROCEDURE Andra_UI :
   DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   EMPTY TEMP-TABLE KalkylimportTT NO-ERROR. 
   
   RUN StartCon_UI. 
  
   startkalkroot:andrakalkyl(INPUT KalkylimportTTh,INPUT-OUTPUT kalknr,INPUT-OUTPUT omrvar,OUTPUT AppservControll).
   IF VALID-HANDLE(AppservControll) THEN DO: 
      RUN AnvKalkylHmt_UI IN AppservControll (OUTPUT TABLE anvkalkyltt).
      RUN AnvkalKylBort_UI IN AppservControll.
   END.   
   RUN Avsluta_UI.
   RUN ConKalkyldb_UI.
  
   /*hämta de ändrade kalkylerna*/
   FOR EACH anvkalkyltt WHERE NO-LOCK:
      RUN sokkalkylomr_UI IN LocalAppServerHandle (INPUT anvkalkyltt.KALKNR,INPUT anvkalkyltt.OMRADE ,OUTPUT TABLE eutvaldfasttemp APPEND).
   END.   
   
   RUN DisConKalkyldb_UI. 
END PROCEDURE.   
/*bort kalkyl*/
PROCEDURE BortKalkyl_UI :
   DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
   DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
   RUN ConKalkyldb_UI.
   RUN KalkylBort_UI IN LocalAppServerHandle (INPUT kalknr,OUTPUT felmedd).
   IF felmedd = "" THEN.
   ELSE DO:
      MESSAGE felmedd
      VIEW-AS ALERT-BOX.
   END.       
   RUN DisConKalkyldb_UI.  
   
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
PROCEDURE AonrHmtKalk_UI :
   DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER aonrplanin AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR gatill.
   EMPTY TEMP-TABLE gatill NO-ERROR. 
   RUN ConKalkyldb_UI.
   RUN AonrHmtKalk_UI IN LocalAppServerHandle (INPUT Guru.GlobalaVariabler:arendekalk,INPUT aonrvar, INPUT delnrvar,INPUT aonrplanin,OUTPUT TABLE gatill).    
   RUN DisConKalkyldb_UI.  
    
END PROCEDURE.
/*koppla och kopiera kalkyl*/
PROCEDURE KopieraKoppla_UI :
   DEFINE INPUT  PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER nyomrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER nyaonr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER nyadelnr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER aonrplanin AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   RUN ConKalkyldb_UI.
   RUN KopieraKoppla_UI IN LocalAppServerHandle (INPUT KalkNrvar,INPUT omrvar,INPUT nyomrvar,INPUT nyaonr,INPUT nyadelnr,INPUT aonrplanin,
   OUTPUT TABLE tidut).     
   RUN DisConKalkyldb_UI.
END PROCEDURE. 
/*sök kalkyler på urval*/
PROCEDURE UrvalKalkyl_UI :
   DEFINE INPUT  PARAMETER TABLE FOR uppkalktemp.
   DEFINE INPUT PARAMETER TABLE FOR uppavdjud.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR.  
   RUN ConKalkyldb_UI.
   RUN UrvalKalkyl_UI IN LocalAppServerHandle (INPUT Guru.GlobalaVariabler:arendekalk,INPUT TABLE uppkalktemp,INPUT TABLE uppavdjud, OUTPUT TABLE eutvaldfasttemp).
   RUN UrvalBort_UI IN LocalAppServerHandle. 
   RUN DisConKalkyldb_UI.
END PROCEDURE.  
/*sök på kalknr*/
PROCEDURE SokKalkyl_UI :
   DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   RUN ConKalkyldb_UI.
   RUN SokKalkyl_UI IN LocalAppServerHandle (INPUT Guru.GlobalaVariabler:arendekalk,INPUT kalknr,OUTPUT TABLE eutvaldfasttemp).
   RUN UrvalBort_UI IN LocalAppServerHandle. 
   RUN DisConKalkyldb_UI.  
END PROCEDURE.  
/*sök  på aonr*/
PROCEDURE SokAonrKalkyl_UI :
   DEFINE INPUT  PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER allavar AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER skapatom AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   RUN ConKalkyldb_UI.
   RUN SokAonrKalkyl_UI IN LocalAppServerHandle (INPUT Guru.GlobalaVariabler:arendekalk,INPUT aonrvar, INPUT delnrvar,INPUT allavar,INPUT skapatom,OUTPUT TABLE eutvaldfasttemp).
   RUN UrvalBort_UI IN LocalAppServerHandle. 
    
   RUN DisConKalkyldb_UI.
END PROCEDURE.
/*sök på plannr*/
PROCEDURE SokPlannrKalkyl_UI :
   DEFINE INPUT  PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER allavar AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER skapatom AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   RUN ConKalkyldb_UI.
   RUN SokPlannrKalkyl_UI IN LocalAppServerHandle (INPUT aonrvar, INPUT delnrvar,INPUT allavar,INPUT skapatom,OUTPUT TABLE eutvaldfasttemp).
   RUN UrvalBort_UI IN LocalAppServerHandle.    
   RUN DisConKalkyldb_UI.
END PROCEDURE.
/*fix valista efter aonr*/
PROCEDURE LaddaTypBen_UI :
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valdfasttemp.
   RUN ConKalkyldb_UI.
   RUN LaddaTypBen_UI IN LocalAppServerHandle (INPUT-OUTPUT TABLE valdfasttemp).
   RUN UrvalBort_UI IN LocalAppServerHandle.    
   RUN DisConKalkyldb_UI.
END PROCEDURE.

/*fix favo*/
PROCEDURE KalkFavo_UI :
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valdfasttemp.
   RUN ConKalkyldb_UI.
   RUN Kalkfavo_UI IN LocalAppServerHandle (INPUT-OUTPUT TABLE valdfasttemp).
   RUN UrvalBort_UI IN LocalAppServerHandle.    
   RUN DisConKalkyldb_UI.
END PROCEDURE.
PROCEDURE KalkAktiv_UI :
   DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER aktiv AS LOGICAL NO-UNDO.
   RUN ConKalkyldb_UI.
   RUN KalkAktiv_UI IN LocalAppServerHandle (INPUT kalknr, INPUT aktiv).
   RUN UrvalBort_UI IN LocalAppServerHandle.    
   RUN DisConKalkyldb_UI.
END PROCEDURE.
/*vilket pris till uppf*/
PROCEDURE GetKalkUpp_UI :
   DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER xtypmtrl AS INTEGER NO-UNDO.
   RUN ConKalkyldb_UI.
   RUN GetKalkUpp_UI IN LocalAppServerHandle (INPUT kalknr, OUTPUT xtypmtrl).
   RUN DisConKalkyldb_UI.
END PROCEDURE.
/*arbetskoder för beredning*/
PROCEDURE Ptemp_UI:
   DEFINE INPUT PARAMETER rad_typ AS INTEGER NO-UNDO.   
   DEFINE OUTPUT PARAMETER TABLE FOR ptemp.
   EMPTY TEMP-TABLE ptemp NO-ERROR. 
   RUN ConKalkyldb_UI.
   RUN Ptemp_UI IN LocalAppServerHandle (INPUT rad_typ, OUTPUT TABLE ptemp).
   RUN DisConKalkyldb_UI.
END PROCEDURE.
  
/* LÖPkoder för beredning*/
PROCEDURE Loptempn_UI:
   DEFINE INPUT PARAMETER rad_typ AS INTEGER NO-UNDO.   
   DEFINE OUTPUT PARAMETER TABLE FOR loptemp.
   EMPTY TEMP-TABLE loptemp NO-ERROR. 
   RUN ConKalkyldb_UI.
   RUN Loptempn_UI IN LocalAppServerHandle (INPUT rad_typ, OUTPUT TABLE loptemp).
   RUN DisConKalkyldb_UI.
END PROCEDURE.

/*p3 arbetskoder för beredning*/
PROCEDURE Ptemp3_UI:
   DEFINE INPUT PARAMETER rad_typ AS INTEGER NO-UNDO.   
   DEFINE OUTPUT PARAMETER TABLE FOR ptemp3.
   EMPTY TEMP-TABLE ptemp NO-ERROR. 
   RUN ConKalkyldb_UI.
   RUN Ptemp3_UI IN LocalAppServerHandle (INPUT rad_typ, OUTPUT TABLE ptemp3).
   RUN DisConKalkyldb_UI.
END PROCEDURE.
  
/*P3 LÖPkoder för beredning*/
PROCEDURE Loptempn3_UI:
   DEFINE INPUT PARAMETER rad_typ AS INTEGER NO-UNDO.   
   DEFINE OUTPUT PARAMETER TABLE FOR loptemp3.
   EMPTY TEMP-TABLE loptemp NO-ERROR. 
   RUN ConKalkyldb_UI.
   RUN Loptempn3_UI IN LocalAppServerHandle (INPUT rad_typ, OUTPUT TABLE loptemp3).
   RUN DisConKalkyldb_UI.
END PROCEDURE.
/*VISA BERKALK*/
PROCEDURE BerKalkVisa_UI:
   DEFINE INPUT  PARAMETER bernr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER beromr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER benvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tidutorg.
   EMPTY TEMP-TABLE tidutorg NO-ERROR. 
   RUN ConKalkyldbExtra_UI.
   RUN BerKalkVisa_UI IN LocalAppServerExtraHandle (INPUT bernr,INPUT beromr,INPUT benvar,OUTPUT TABLE tidutorg).
   RUN DisConKalkyldbExtra_UI.
END PROCEDURE.
/*skapa kalkyl av berkalk*/
PROCEDURE BerKalkSkapa_UI :
   DEFINE INPUT  PARAMETER valaonr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER ksvar AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   EMPTY TEMP-TABLE KalkylimportTT NO-ERROR. 
   DEFINE VARIABLE KalkNrvar AS INTEGER NO-UNDO.
   RUN ConKalkyldbExtra_UI.
   RUN BerKalkSkapa_UI IN LocalAppServerExtraHandle (INPUT valaonr,INPUT delnrvar,INPUT ksvar, OUTPUT KalkNrvar,OUTPUT TABLE tidut, OUTPUT TABLE KalkylimportTT).
   FIND FIRST KalkylimportTT NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KalkylimportTT THEN DO:
      FIND FIRST tidut WHERE NO-LOCK NO-ERROR.
      IF AVAILABLE tidut THEN DO:
         MESSAGE tidut.UT SKIP 
         "Kalkyl skapades inte!"
          VIEW-AS ALERT-BOX.
          RETURN.
       END. 
   END.  
   omrvar = KalkylimportTT.OMRADE.     
   RUN DisConKalkyldbExtra_UI.
   RUN StartCon_UI. 
   startkalkroot:andrakalkyl(INPUT KalkylimportTTh,INPUT-OUTPUT KalkNrvar,INPUT-OUTPUT omrvar,OUTPUT AppservControll ).
   EMPTY TEMP-TABLE KalkylimportTT NO-ERROR. 
   IF VALID-HANDLE(AppservControll) THEN DO:
      RUN AnvKalkylHmt_UI IN AppservControll (OUTPUT TABLE anvkalkyltt).
      RUN AnvkalKylBort_UI IN AppservControll.
   END.   
   RUN Avsluta_UI.
   RUN ConKalkyldb_UI.  
   /*hämta de ändrade kalkylerna*/
   FOR EACH anvkalkyltt WHERE NO-LOCK:
      RUN sokkalkylomr_UI IN LocalAppServerHandle (INPUT anvkalkyltt.KALKNR,INPUT anvkalkyltt.OMRADE ,OUTPUT TABLE eutvaldfasttemp APPEND).
   END.     
   RUN DisConKalkyldb_UI.
END PROCEDURE.   
PROCEDURE BerKalkUppKoll_UI :
   DEFINE OUTPUT PARAMETER TABLE FOR kbetemp.
   EMPTY TEMP-TABLE kbetemp NO-ERROR. 
   RUN ConKalkyldbExtra_UI.
   RUN BerKalkUppKoll_UI IN LocalAppServerExtraHandle (OUTPUT TABLE kbetemp).
   RUN DisConKalkyldbExtra_UI.
END PROCEDURE.  
/*Start kalkyl objectorientering*/
PROCEDURE StartCon_UI :
   startkalkroot = NEW Guru.Root().
   startkalkroot:startkalkyldb().
END PROCEDURE.
/*Start kalkyladm objectorientering*/
PROCEDURE StartConAdm_UI :
   startkalkroot = NEW Guru.Root().
   startkalkroot:StartKalkylAdmDb().
END PROCEDURE. 
/*avslutar objectorientering*/
PROCEDURE Avsluta_UI :
   DELETE OBJECT startkalkroot NO-ERROR.
END PROCEDURE. 
/*con av db då ej oo*/
PROCEDURE ConKalkyldb_UI:
   IF Guru.Konstanter:appcon THEN DO:
      RUN KALKBERAPPDS.p PERSISTENT SET LocalAppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
   END.
   ELSE DO:
      RUN KALKBERAPPDS.p PERSISTENT SET LocalAppServerHandle (INPUT Guru.Konstanter:globanv).
   END.
END PROCEDURE.
/* discon av db då ej oo*/
PROCEDURE DisConKalkyldb_UI :
    IF VALID-HANDLE(LocalAppServerHandle) THEN DELETE PROCEDURE LocalAppServerHandle.
END PROCEDURE.  

/*con av db då ej oo*/
PROCEDURE ConKalkyldbExtra_UI:
   IF Guru.Konstanter:appcon THEN DO:
      RUN KALKBERAPPDSEXTRA.p PERSISTENT SET LocalAppServerExtraHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
   END.
   ELSE DO:
      RUN KALKBERAPPDSEXTRA.p PERSISTENT SET LocalAppServerExtraHandle (INPUT Guru.Konstanter:globanv).
   END.
END PROCEDURE.
/* discon av db då ej oo*/
PROCEDURE DisConKalkyldbExtra_UI :
    IF VALID-HANDLE(LocalAppServerExtraHandle) THEN DELETE PROCEDURE LocalAppServerExtraHandle.
END PROCEDURE.


/*kalkyldb*/
PROCEDURE KalkyldbApp_UI :
   DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omr AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkantal.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkkostnad.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkylprisertt.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkvisningtt.
   DEFINE OUTPUT PARAMETER DATASET FOR KalkylDS.
   root = NEW Guru.Root().
   startkalkdb = NEW Modules.Kalkyl.Kalkyldb(Root).
   Guru.GlobalaVariabler:FranUppf = TRUE.
   startkalkdb:LaddaKalkyl(INPUT kalknr,INPUT omr).
   startkalkdb:FetchPriser(INPUT kalknr,INPUT omr).
   startkalkdb:RaknaAllaKoder().
   startkalkdb:HmtLaddaKalkylDs(OUTPUT DATASET KalkylDS).
   startkalkdb:HmtLaddaKalkylBer(OUTPUT TABLE kalkantal,OUTPUT TABLE kalkkostnad,OUTPUT TABLE kalkylprisertt, OUTPUT TABLE kalkvisningtt).
   Guru.GlobalaVariabler:FranUppf = FALSE.
   DELETE OBJECT root NO-ERROR.    
   DELETE OBJECT startkalkdb NO-ERROR. 
  
END PROCEDURE.  

   

   