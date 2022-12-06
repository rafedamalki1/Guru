/*V11InfraCtrlStart.p*/
DEFINE VARIABLE startkalkroot AS Modules.Global.V11InfraControllF NO-UNDO.

PROCEDURE StartV11_UI:
   startkalkroot = NEW Modules.Global.V11InfraControllF().
   startkalkroot:Show().
   RUN Avsluta_UI.  
   
END PROCEDURE.  

/*avslutar objectorientering*/
PROCEDURE Avsluta_UI :
   DELETE OBJECT startkalkroot NO-ERROR.
END PROCEDURE. 
