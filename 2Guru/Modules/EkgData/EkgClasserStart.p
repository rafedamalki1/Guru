/*EkgClasserStart.p*/


DEFINE VARIABLE startEkgroot AS Guru.Root NO-UNDO.
DEFINE VARIABLE startEkgdb   AS Modules.EkgData.EkgDatadb NO-UNDO.
DEFINE VARIABLE Root          AS Guru.Root NO-UNDO.
DEFINE VARIABLE LocalAppServerHandle  AS HANDLE NO-UNDO.
DEFINE VARIABLE AppservControll       AS HANDLE  NO-UNDO.
/*DEFINE SHARED VARIABLE hpApi AS HANDLE NO-UNDO.*/
/*EKG data kalkyl*/
PROCEDURE EkgStart_UI:
   RUN StartConEkg_UI.
   startEkgroot:EkgShell(OUTPUT AppservControll).
   RUN Avsluta_UI.  
END PROCEDURE.  


/*Start EKG objectorientering*/
PROCEDURE StartConEkg_UI :
   startEkgroot = NEW Guru.Root().
   startEkgroot:StartEkgDb().
END PROCEDURE. 
/*avslutar objectorientering*/
PROCEDURE Avsluta_UI :
   
   DELETE OBJECT startEkgroot NO-ERROR.
END PROCEDURE. 
/*con av db då ej oo*/
PROCEDURE ConEkgdb_UI:
   IF Guru.Konstanter:appcon THEN DO:
      RUN EKGAPPDS.p PERSISTENT SET LocalAppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
   END.
   ELSE DO:
      RUN EKGAPPDS.p PERSISTENT SET LocalAppServerHandle (INPUT Guru.Konstanter:globanv).
   END.
END PROCEDURE.
/* discon av db då ej oo*/
PROCEDURE DisConEkgdb_UI :
    IF VALID-HANDLE(LocalAppServerHandle) THEN DELETE PROCEDURE LocalAppServerHandle.
END PROCEDURE.  



   

   