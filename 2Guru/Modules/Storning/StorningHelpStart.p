/*StorningHelpStart.p*/
/*Anders Olsson Elpool i Umeå AB  28 apr 2017 10:47:59 
DEFINE VARIABLE StorningHelpStartH AS HANDLE NO-UNDO. 
RUN Modules\Storning\StorningHelpStart.p PERSISTENT SET StorningHelpStartH.
RUN  Storning_UI IN StorningHelpStartH.
*/
DEFINE VARIABLE startStorningroot AS Guru.Root NO-UNDO.
           
PROCEDURE DarwinPlusUserStart_UI :
  RUN StartCon_UI.
   startStorningroot:DarwinPlusUserDb().
   startStorningroot:DarwinPlusUser().
  
   RUN Avsluta_UI.
END PROCEDURE.
PROCEDURE Storning_UI : 
   
   
   RUN StartCon_UI.
   startStorningroot:StartStorningDb().
   startStorningroot:StartStorning().
   RUN Avsluta_UI.

END PROCEDURE.

PROCEDURE StartCon_UI :
  startStorningroot = NEW Guru.Root().
  
   
END PROCEDURE.

PROCEDURE Avsluta_UI :
   
   DELETE OBJECT startStorningroot NO-ERROR.
   startStorningroot = ?.
END PROCEDURE. 

/*
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
*/