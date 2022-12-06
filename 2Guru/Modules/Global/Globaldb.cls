 
 /*------------------------------------------------------------------------
    File        : Globaldb.cls
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Mon Feb 13 13:20:42 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS Modules.Global.Globaldb: 
   &Scoped-define STATIC STATIC
   &Scoped-define NEW STATIC
   &Scoped-define PUBLIC PUBLIC
   {GLOBALTT.I}
   {DEFVALUESNETTT.I}
   {TIDUTTT.I}
   DEFINE DATASET DefaultDS FOR GuruDefaultsTT.
   DEFINE PUBLIC VARIABLE GuruDefaultsTTh     AS HANDLE    NO-UNDO.
   DEFINE PUBLIC VARIABLE AppServerHandle     AS HANDLE    NO-UNDO.
   DEFINE PUBLIC VARIABLE Root                AS Guru.Root NO-UNDO.
   /* Skapa standardvärden för ribbon OM DEN INTE FINNS*/
   METHOD PUBLIC LOGICAL start():
      FIND FIRST levtemp WHERE NO-LOCK NO-ERROR.
      IF AVAILABLE levtemp THEN RETURN YES.
      ELSE RETURN FALSE.
   END METHOD.   
  /*anders*/
/*  HÄMTA PRODATSETET MED DEFAULT VÄRDEN*/
  METHOD PUBLIC VOID FetchDefaultValues(cPROG AS CHARACTER,ganv AS CHARACTER,huvint AS INTEGER,huvchar AS CHARACTER):
     
     RUN FetchDefaultValues IN AppServerHandle(OUTPUT DATASET DefaultDS, INPUT cPROG,INPUT ganv, INPUT huvint, INPUT huvchar).
     TEMP-TABLE GuruDefaultsTT:TRACKING-CHANGES = TRUE.
     FOR EACH GuruDefaultsTT WHERE GuruDefaultsTT.TOOLGROUP = "Inkludera i visning" NO-LOCK:
        GuruDefaultsTT.TOOLGROUP = "Visningsinställningar". 
     END.
     
     /*
     FOR EACH GuruDefaultsTT WHERE NO-LOCK:
        MESSAGE GuruDefaultsTT.TOOLKEY GuruDefaultsTT.TOOLVALUE GuruDefaultsTT.VALIDTOOLVALUE
        VIEW-AS ALERT-BOX.
     END.  VisFaktorer
     DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
     CREATE DATASET hDSChanges.
     hDSChanges:CREATE-LIKE (DATASET DefaultDS:HANDLE).
     hDSChanges:GET-CHANGES (DATASET DefaultDS:HANDLE).
    */
     THIS-OBJECT:GuruDefaultsTTh = TEMP-TABLE GuruDefaultsTT:HANDLE:DEFAULT-BUFFER-HANDLE.
               
  END METHOD.
 /*HÄMTA ETT VÄRDE UR TT*/ 
  METHOD PUBLIC CHARACTER GetActualValues(cPROG AS CHARACTER,ganv AS CHARACTER,ToolKey AS CHARACTER):
     FIND FIRST GuruDefaultsTT WHERE GuruDefaultsTT.PROGRAM = cPROG AND GuruDefaultsTT.ANVANDARE = ganv AND
     GuruDefaultsTT.TOOLKEY = ToolKey NO-LOCK NO-ERROR.
     IF AVAILABLE GuruDefaultsTT THEN DO:
       
        RETURN GuruDefaultsTT.VALIDTOOLVALUE.
     END.             
  END METHOD.
  /*SPARA ETT VÄRDE TILL TT*/
  METHOD PUBLIC VOID SaveActualValues(cPROG AS CHARACTER,ganv AS CHARACTER,DasTool  AS Infragistics.Win.UltraWinToolbars.ToolBase):
     DEFINE VARIABLE ToolKey      AS CHARACTER                                  NO-UNDO.
     ToolKey  = DasTool:KEY.
     FIND FIRST GuruDefaultsTT WHERE GuruDefaultsTT.PROGRAM = cPROG AND GuruDefaultsTT.ANVANDARE = ganv AND 
    /* GuruDefaultTT.TOOLGROUP =*/ 
     GuruDefaultsTT.TOOLKEY = ToolKey NO-LOCK NO-ERROR.   
     IF AVAILABLE GuruDefaultsTT THEN DO:
        IF GuruDefaultsTT.TOOLTYPE EQ "Infragistics.Win.UltraWinToolbars.StateButtonTool" THEN DO:
           GuruDefaultsTT.VALIDTOOLVALUE = STRING ( CAST(DasTool, Infragistics.Win.UltraWinToolbars.StateButtonTool):CHECKED ).
        END.
        ELSE IF GuruDefaultsTT.TOOLTYPE EQ "Infragistics.Win.UltraWinToolbars.ComboBoxTool" THEN DO:
           GuruDefaultsTT.VALIDTOOLVALUE = STRING ( CAST(DasTool, Infragistics.Win.UltraWinToolbars.ComboBoxTool):SelectedIndex ).
        END.
        ELSE DO:
          
        END.      
     END.   
               
  END METHOD.
  METHOD PUBLIC VOID VisaDef().
     FOR EACH GuruDefaultsTT  NO-LOCK:
        
           MESSAGE "Nu"
        GuruDefaultsTT.PROGRAM 
        GuruDefaultsTT.ANVANDARE
           GuruDefaultsTT.TOOLGROUP 
      GuruDefaultsTT.TOOLKEY 
      
      GuruDefaultsTT.TOOLVALUE  
           
        VIEW-AS ALERT-BOX.
     END.
   END METHOD.  
  /*SKAPA DEFAULT OM DOM INTE FINNS*/
  METHOD PUBLIC VOID CreateActualDefaultTT(cPROG AS CHARACTER,ganv AS CHARACTER,huvint AS INTEGER,huvchar AS CHARACTER, 
     cKEY AS CHARACTER, cVALUE AS CHARACTER, cGROUP AS CHARACTER, cTYPE AS CHARACTER,egetvarde AS LOGICAL, OUTPUT defToolValue AS CHARACTER ):
     FIND FIRST GuruDefaultsTT WHERE GuruDefaultsTT.PROGRAM = cPROG AND GuruDefaultsTT.ANVANDARE = ganv AND GuruDefaultsTT.HUVUDINT = huvint AND GuruDefaultsTT.HUVUDCHAR  = huvchar AND 
     GuruDefaultsTT.TOOLKEY = cKEY  AND GuruDefaultsTT.TOOLGROUP = cGROUP AND GuruDefaultsTT.TOOLTYPE = cTYPE NO-LOCK NO-ERROR.
     IF NOT AVAILABLE GuruDefaultsTT THEN DO :        
        FIND FIRST GuruDefaultsTT WHERE GuruDefaultsTT.PROGRAM = cPROG AND GuruDefaultsTT.HUVUDINT = huvint AND GuruDefaultsTT.HUVUDCHAR  = huvchar AND 
        GuruDefaultsTT.TOOLKEY = cKEY  AND GuruDefaultsTT.TOOLGROUP = cGROUP AND GuruDefaultsTT.TOOLTYPE = cTYPE NO-LOCK NO-ERROR.
     END.   
     IF NOT AVAILABLE GuruDefaultsTT THEN DO :
        CREATE GuruDefaultsTT.
        ASSIGN
        GuruDefaultsTT.PROGRAM = cPROG
        GuruDefaultsTT.ANVANDARE = ganv
        GuruDefaultsTT.HUVUDINT = huvint
        GuruDefaultsTT.HUVUDCHAR = huvchar
        GuruDefaultsTT.TOOLKEY = cKEY
        GuruDefaultsTT.TOOLVALUE = cVALUE
        GuruDefaultsTT.VALIDTOOLVALUE = cVALUE
        GuruDefaultsTT.TOOLGROUP = cGROUP
        GuruDefaultsTT.TOOLTYPE = cTYPE.
        GuruDefaultsTT.TTRECID = RECID(GuruDefaultsTT).
        IF egetvarde = FALSE THEN DO:
           EMPTY TEMP-TABLE GuruDefaultValuesTT NO-ERROR. 
           CREATE GuruDefaultValuesTT.
           BUFFER-COPY GuruDefaultsTT TO GuruDefaultValuesTT.        
           RUN foretagdefault IN AppServerHandle (INPUT-OUTPUT TABLE GuruDefaultValuesTT).
           FIND FIRST GuruDefaultValuesTT WHERE NO-LOCK NO-ERROR.
           BUFFER-COPY GuruDefaultValuesTT TO GuruDefaultsTT.
           EMPTY TEMP-TABLE GuruDefaultValuesTT NO-ERROR.
        END.   
     END.
     ASSIGN
     /*
     GuruDefaultsTT.VALIDTOOLVALUE = GuruDefaultsTT.TOOLVALUE
     */
     defToolValue = GuruDefaultsTT.TOOLVALUE. 
  END METHOD.
  /*SPARA DEFAULT VÄRDEN I PRODATSET TILL DISK*/
  METHOD PUBLIC VOID SaveDefaultValues(cPROG AS CHARACTER,ganv AS CHARACTER,huvint AS INTEGER,huvchar AS CHARACTER):
     DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
     FOR EACH GuruDefaultsTT WHERE GuruDefaultsTT.PROGRAM = cPROG AND GuruDefaultsTT.ANVANDARE = ganv AND GuruDefaultsTT.HUVUDINT = huvint AND GuruDefaultsTT.HUVUDCHAR  = huvchar NO-LOCK BY GuruDefaultsTT.TOOLGROUP:
        IF GuruDefaultsTT.TOOLVALUE NE GuruDefaultsTT.VALIDTOOLVALUE THEN  GuruDefaultsTT.TOOLVALUE = GuruDefaultsTT.VALIDTOOLVALUE.
     END.
     CREATE DATASET hDSChanges.
     hDSChanges:CREATE-LIKE (DATASET DefaultDS:HANDLE).
     hDSChanges:GET-CHANGES (DATASET DefaultDS:HANDLE).
     TEMP-TABLE GuruDefaultsTT:TRACKING-CHANGES = FALSE.
     /*
     DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
      SPARAXML = "C:\CTest" + STRING(TIME) + ".xml". 
      hDSChanges:WRITE-XML("FILE", SPARAXML).
      */ 
     RUN SparaProDataSetDefaultDS IN AppServerHandle (INPUT DATASET-HANDLE hDSChanges).

     hDSChanges:MERGE-CHANGES (DATASET DefaultDS:HANDLE).

     TEMP-TABLE GuruDefaultsTT:TRACKING-CHANGES = TRUE.
      
  END METHOD.
  METHOD PUBLIC VOID XmlExport(INPUT cfile AS CHARACTER):  
      DEFINE VARIABLE cTargetType     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lFormatted      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE cEncoding       AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lWriteSchema    AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lMinSchema      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lWriteBeforeImage   AS LOGICAL   NO-UNDO.
      
      DEFINE VARIABLE dResult AS System.Windows.Forms.DialogResult NO-UNDO.
      ASSIGN
      cTargetType       = "FILE"
      lFormatted        = NO
      cEncoding         = ?
      cSchemaLocation   = ?
      lWriteSchema      = YES
      lMinSchema        = FALSE
      lWriteBeforeImage = FALSE.
        
      /* BYT DATASET TTILL KalkylLoparDS eller likande??*/ 
      DATASET DefaultDS:WRITE-XML (cTargetType, cFile, lFormatted, cEncoding, cSchemaLocation, lWriteSchema, lMinSchema, lWriteBeforeImage). 
      FIND FIRST GuruDefaultsTT  WHERE NO-LOCK NO-ERROR.    
   END METHOD. 
   /*Anders Olsson Elpool i Umeå AB  28 mar 2014 10:06:40 
   query utan grid 
   */
   METHOD PUBLIC HANDLE CreateCustomQuery(INPUT t AS HANDLE, INPUT q AS CHARACTER):
      DEFINE VARIABLE CustomQuery AS HANDLE NO-UNDO.
      IF q = "" THEN q = "FOR EACH " + t:TABLE.
      CREATE QUERY CustomQuery.
      CustomQuery:SET-BUFFERS(t).
      CustomQuery:QUERY-PREPARE(q).
      CustomQuery:QUERY-OPEN().
      RETURN CustomQuery.
   END METHOD.
   
    METHOD PUBLIC INTEGER CreateCustomQueryPS(INPUT t AS HANDLE, INPUT q AS CHARACTER):
      DEFINE VARIABLE CustomQuery AS HANDLE NO-UNDO.
      IF q = "" THEN q = "PRESELECT EACH " + t:TABLE.
      CREATE QUERY CustomQuery.
      CustomQuery:SET-BUFFERS(t).
      CustomQuery:QUERY-PREPARE(q).
      CustomQuery:QUERY-OPEN().
      RETURN CustomQuery:NUM-RESULTS.
   END METHOD.
   
   
   /*FUNKTION OSÄKER GetNumResult funkar inte utan brw*/
   METHOD PUBLIC INTEGER  GetNumResult(INPUT CustomQuery AS HANDLE):
      CustomQuery:GET-LAST(NO-LOCK).
      RETURN CustomQuery:NUM-RESULTS.
   END METHOD.
   METHOD PUBLIC VOID TidutHmt(vad AS CHARACTER):
      CREATE tidut.
      tidut.UT = vad.  
      tidut.TTRECID = RECID(tidut).            
   END METHOD.
   METHOD PUBLIC VOID CloseCustomQuery(INPUT q AS HANDLE):
      q:QUERY-CLOSE() NO-ERROR.
   END METHOD.
   
   METHOD PUBLIC INTEGER AvrundaUpp(uppdec AS DECIMAL):
      DEFINE VARIABLE uppint AS INTEGER NO-UNDO.
      uppint = uppdec.
      IF uppdec = uppint THEN RETURN INTEGER(uppdec).
      ELSE IF uppdec >  uppint THEN RETURN INTEGER(uppint + 1).
      ELSE RETURN INTEGER(uppdec).
      
   END METHOD.
   CONSTRUCTOR PUBLIC Globaldb(INPUT r AS Guru.Root):
       
   /*   {GLOBALSTART.I}*/
      
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Connect().
      THIS-OBJECT:EmptyTT(Guru.GlobalaVariabler:MaterielTTh).
     
      EMPTY TEMP-TABLE mtrltemp.
     
   END CONSTRUCTOR.
  
   DESTRUCTOR Globaldb():
      
      IF VALID-HANDLE(THIS-OBJECT:AppServerHandle) THEN DELETE PROCEDURE THIS-OBJECT:AppServerHandle.
      THIS-OBJECT:AppServerHandle = ?.
   END DESTRUCTOR.
   METHOD PUBLIC VOID Connect (  ): 
      DEBUGGER:SET-BREAK().
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TEMPTABELLERAPP.P PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globnystart).
      END.
      ELSE DO:
         RUN TEMPTABELLERAPP.P PERSISTENT SET AppServerHandle (INPUT Guru.Konstanter:globnystart).                  
      END.   
       
   END METHOD.
   
   /* Hämtar områden ifrån databas*/
   METHOD PUBLIC VOID FetchOmrade():     
      
      FIND FIRST omrtemp WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE omrtemp THEN DO:
         RUN omrade_UI IN AppServerHandle (OUTPUT TABLE omrtemp).
         RUN Bortagnaomrade_UI IN AppServerHandle (OUTPUT TABLE Borttagnaomrtemp).
         RUN jurp_UI IN AppServerHandle (INPUT Guru.Konstanter:globanv,OUTPUT TABLE jurperstemp,OUTPUT TABLE judavdtemp).
         RUN bestkund_UI IN AppServerHandle (OUTPUT TABLE bestkundalltclass).
      END.                 
       
   END METHOD.
   /*
   /* Hämtar användare ifrån databas ska ej köras se fakeroot*/
   METHOD PUBLIC VOID FetchOmrade(INPUT rensavar AS LOGICAL):
      IF rensavar = TRUE THEN DO:         
         THIS-OBJECT:EmptyTT(Guru.Konstanter:OmradeTTh). 
         THIS-OBJECT:EmptyTT(Guru.Konstanter:JurPersTTh).
         THIS-OBJECT:EmptyTT(Guru.Konstanter:JurAvdTTh).
         THIS-OBJECT:EmptyTT(Guru.Konstanter:BestKundTTh).
      END.   
      THIS-OBJECT:FetchOmrade().           
   END METHOD.
  
  /* Hämtar användare ifrån databas ska ej köras se fakeroot*/
   METHOD PUBLIC VOID FetchAnvandare():     
      FIND FIRST anvandartemp WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE anvandartemp THEN DO:
         RUN anvtemp_UI IN AppServerHandle (OUTPUT TABLE anvandartemp).
          Guru.Konstanter:StoppaOmrSek(TEMP-TABLE anvandartemp:HANDLE).          
      END.
      
   END METHOD.
   
   /* Hämtar personal ifrån databas ska ej köras se fakeroot*/
   METHOD PUBLIC VOID FetchPersonal(): 
      FIND FIRST personaltemp WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE personaltemp THEN DO:
         RUN perstemp_UI IN AppServerHandle (OUTPUT TABLE personaltemp).
         Guru.Konstanter:StoppaOmrSek(TEMP-TABLE personaltemp:HANDLE).  
      END. 
      
   END METHOD. 
*/
   /* Hämtar material EN LEV  åt gången*/
   METHOD PUBLIC VOID FetchMateriel(INPUT lev AS CHARACTER, INPUT enr AS CHARACTER, INPUT ben AS CHARACTER, INPUT kundvar AS LOGICAL):
      DEFINE VARIABLE tth AS HANDLE NO-UNDO.
      /*
      EMPTY TEMP-TABLE mtrltemp.
      tth = TEMP-TABLE mtrltemp:HANDLE.
      */
      THIS-OBJECT:EmptyTT(Guru.GlobalaVariabler:MaterielTTh).
      tth = Guru.GlobalaVariabler:MaterielTTh:TABLE-HANDLE.
      IF lev = "Alla leverantörer" THEN lev = "".
      RUN mtrltemp_UI IN AppServerHandle (INPUT-OUTPUT TABLE-HANDLE tth, INPUT lev, INPUT enr, INPUT ben, INPUT kundvar).
      Guru.GlobalaVariabler:MaterielTTh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
      IF Guru.GlobalaVariabler:MaterielTTh:AVAILABLE THEN. 
      ELSE DO:
         System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(206)).
      END.
      
      /*       
      FIND FIRST mtrltemp WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE mtrltemp THEN DO:
         System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(206)).
      END.
      */
   END METHOD.
   
   /* Hämtar material flera på en gång*/
   METHOD PUBLIC VOID FetchMateriel(INPUT lev AS CHARACTER):
      DEFINE VARIABLE Etth AS HANDLE NO-UNDO.
      DEFINE VARIABLE Mtth AS HANDLE NO-UNDO.
      DEFINE VARIABLE MttHbuff AS HANDLE NO-UNDO.
       
      Etth = Guru.GlobalaVariabler:EMaterielTTh:TABLE-HANDLE.
      Mtth = Guru.GlobalaVariabler:MaterielTTh:TABLE-HANDLE.
      
      RUN mtrltemptt_UI IN AppServerHandle (INPUT TABLE-HANDLE Etth, OUTPUT TABLE-HANDLE Mtth, INPUT lev).
      MttHbuff  = Mtth:DEFAULT-BUFFER-HANDLE.
      Guru.GlobalaVariabler:MaterielTTh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
      IF Guru.GlobalaVariabler:MaterielTTh:AVAILABLE THEN. 
      ELSE DO:
         System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(206)).
      END.
      /*
      FIND FIRST mtrltemp WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE mtrltemp THEN DO:
         System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(206)).
      END.
      */
   END METHOD.
  
   /* Hämtar leverantörer SE FAKEROOT*/
   /*
   METHOD PUBLIC VOID FetchLeverantor():
      FIND FIRST levtemp WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE levtemp THEN DO:
         RUN levtemp_UI IN AppServerHandle (OUTPUT TABLE levtemp).
         RUN huvlevtemp_UI IN AppServerHandle (OUTPUT TABLE huvlevtemp).
      END.                       
   END METHOD.
   */
/*SE FAKEROOT
   
 */
   METHOD PUBLIC CHARACTER LaddaLogga(INPUT styrbild AS CHARACTER ):
      DEFINE VARIABLE link AS CHARACTER NO-UNDO.
      RUN LoggaIn IN AppServerHandle (INPUT styrbild, INPUT Guru.Konstanter:gurubilder,OUTPUT link).
      RETURN link.
   END METHOD.
 
   /* Tömmer temptable */
  
   METHOD PUBLIC VOID EmptyTT(tempNamnh AS HANDLE ):
      tempNamnh:EMPTY-TEMP-TABLE() NO-ERROR.                                   
   END METHOD.  
   
   

END CLASS.