DEFINE VARIABLE startkalkroot AS Guru.Root NO-UNDO.
DEFINE VARIABLE LocalAppServerHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE AppservControll AS HANDLE NO-UNDO.
DEFINE VARIABLE KalkylimportTTh AS HANDLE NO-UNDO.
DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kalknr AS INTEGER NO-UNDO.

/*adm kalkyl*/
RUN Kalkyladm_UI.
PROCEDURE Kalkyladm_UI:
RUN StartConAdm_UI.
startkalkroot:fakekalkyl().
RUN Avsluta_UI.

END PROCEDURE.

PROCEDURE StartConAdm_UI :
    startkalkroot = NEW Guru.Root (FALSE).
   /*
   startkalkroot:startkalkyldb().
   */
END PROCEDURE.
PROCEDURE Avsluta_UI :
DELETE OBJECT startkalkroot NO-ERROR.
END PROCEDURE. 
