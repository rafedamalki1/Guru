 
 /*------------------------------------------------------------------------
    File        : GPLDb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Thu Apr 21 10:01:29 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

CLASS Modules.Projekt.GPLDb: 
   DEFINE PUBLIC  PROPERTY AppServerHandle AS HANDLE NO-UNDO 
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC VARIABLE Root                  AS Guru.Root NO-UNDO. 
   DEFINE PUBLIC VARIABLE ControllGPL                 AS Modules.Projekt.GPL NO-UNDO. 
   DEFINE PUBLIC PROPERTY gplstart AS LOGICAL NO-UNDO
   PUBLIC GET. PUBLIC SET.
   
   DEFINE VARIABLE GPLDS AS HANDLE NO-UNDO.
   
   DEFINE PUBLIC PROPERTY Plidvar AS INTEGER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY Aonrvar AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY Delnrvar AS INTEGER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY vad AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   
   
   &Scoped-define PUBLIC PUBLIC
   {GPLDYNTABLEH.I}
   {GPLTT.I}
   {GPLTTH.i}

   
   CONSTRUCTOR PUBLIC GPLDb(INPUT r AS Guru.Root, INPUT aonrvar AS CHARACTER, INPUT delnrvar AS INTEGER, INPUT Plidvar AS INTEGER):
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Aonrvar = aonrvar.
      THIS-OBJECT:Delnrvar = delnrvar.
      THIS-OBJECT:Plidvar = Plidvar.
      {GPLTTTH.i}
   END CONSTRUCTOR.
   
   METHOD PUBLIC VOID Connect():
      IF Guru.Konstanter:appcon THEN DO:
         RUN GPLBER.p PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
      END.
      ELSE DO:
         RUN GPLBER.p PERSISTENT SET AppServerHandle (INPUT Guru.Konstanter:globanv).
      END.

   END METHOD.

   METHOD PUBLIC LOGICAL GPLStart():
      DEFINE VARIABLE ejstart AS LOGICAL NO-UNDO.
      DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
      
      DEFINE VARIABLE fanns AS LOGICAL NO-UNDO. 
      DEFINE VARIABLE plidvar AS INTEGER NO-UNDO. 
      /*
      RUN finnsgpl_UI IN AppServerHandle 
      (INPUT THIS-OBJECT:Aonrvar, INPUT THIS-OBJECT:Delnrvar,
       OUTPUT fanns, OUTPUT plidvar).
       
      IF fanns THEN. ELSE DO:      
         MESSAGE "Det finns ingen personalliggare för detta projekt, vill du starta en? " 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE starta AS LOGICAL.
      
         IF starta = TRUE THEN DO:
            RUN nygpl_UI IN AppServerHandle 
            (INPUT THIS-OBJECT:Aonrvar, INPUT THIS-OBJECT:Delnrvar, 
             OUTPUT plidvar).
         END.
         ELSE DO:
            gplstart = FALSE.
            RETURN gplstart.
         END.
      END.
      
      THIS-OBJECT:Plidvar = plidvar. 
      */
      THIS-OBJECT:laddaGPL().
      
      gplstart = TRUE.
      RETURN   gplstart. 
   END METHOD.
   
   
   METHOD PUBLIC VOID laddaGPL():
      DEFINE VARIABLE vad AS CHARACTER NO-UNDO.
      vad = "GPLHUVUD.PLID = "+ STRING(THIS-OBJECT:Plidvar).
      THIS-OBJECT:vad = vad.

      GPLDS:EMPTY-DATASET() NO-ERROR.
      
      GPLDS = ?.
      gplhuvudtth = ?.
      gplaktivitettth = ?.
      
      RUN laddaGPLDS_UI IN AppServerHandle 
      (INPUT vad,OUTPUT DATASET-HANDLE GPLDS BIND).
      IF VALID-HANDLE(GPLDS) THEN DO:
         THIS-OBJECT:gplhuvudtth = GPLDS:GET-BUFFER-HANDLE(1).
         THIS-OBJECT:gplaktivitettth = GPLDS:GET-BUFFER-HANDLE(2). 
      END.
   END METHOD.
   
  METHOD PUBLIC VOID laddaOmGPL():
      DEFINE VARIABLE vad AS CHARACTER NO-UNDO.
      vad = "GPLHUVUD.PLID = "+ STRING(THIS-OBJECT:Plidvar).
      THIS-OBJECT:vad = vad.
      GPLDS:EMPTY-DATASET() NO-ERROR.
      GPLDS = ?.
      gplhuvudtth = ?.
      gplaktivitettth = ?.
      
      RUN laddaOmGPLDS_UI IN AppServerHandle 
      (INPUT vad,OUTPUT DATASET-HANDLE GPLDS BIND).
      IF VALID-HANDLE(GPLDS) THEN DO:
         THIS-OBJECT:gplhuvudtth = GPLDS:GET-BUFFER-HANDLE(1).
         THIS-OBJECT:gplaktivitettth = GPLDS:GET-BUFFER-HANDLE(2). 
      END.
      
   END METHOD.
   
    
   METHOD PUBLIC VOID aktiveraGPL(INPUT plid AS INTEGER, INPUT lage AS LOGICAL):
      RUN aktiveragpl_UI IN AppServerHandle (INPUT plid, INPUT lage).
   END METHOD.
   
   METHOD PUBLIC VOID uppdateraByggID(INPUT plid AS INTEGER, INPUT nyttNr AS CHARACTER):
      RUN uppdateraByggID_UI IN AppServerHandle (INPUT plid, INPUT nyttNr).
   END METHOD.
  
   METHOD PUBLIC LOGICAL skapaRattelse():
      DEFINE VARIABLE fannsfel AS LOGICAL.
     /*
      gplrattelsetth:FIND-FIRST().
      TT HAND EJ BUFFERH
      RUN skapagplrattelse_UI IN AppServerHandle (INPUT gplrattelsetth, OUTPUT fannsfel).
      RETURN fannsfel.
      */ 
      RETURN fannsfel.
   END METHOD.
  
   
   METHOD PUBLIC VOID InitializeTTs():
    
   END METHOD.
   
   
   
   DESTRUCTOR GPLDb():
      /*EMPTY TEMP-TABLE gplhuvudtth  NO-ERROR.
      EMPTY TEMP-TABLE gplaktivitettth  NO-ERROR.*/
      
      IF VALID-HANDLE(THIS-OBJECT:AppServerHandle) THEN DO:
         RUN avslutagpl_UI IN AppServerHandle.
         DELETE PROCEDURE THIS-OBJECT:AppServerHandle NO-ERROR.
      END.
      
      AppServerHandle = ?.
   END DESTRUCTOR.
   
END CLASS.
