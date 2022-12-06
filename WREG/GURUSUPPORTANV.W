&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: GURUSUPPORTANV.W

  Description: KÖR FRÅN STARTADMDB.w 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ALLDEF.I}
{ANVTEMPS.I}
{PERSTEMP.I}
DEFINE TEMP-TABLE exanvandartemp NO-UNDO LIKE anvandartemp.
{PERSONALTEMP.I}
&Scoped-define NEW NEW
{FORETEMP.I}
{GLOBVAR2DEL1.I}
DEFINE VARIABLE losenwebh AS HANDLE NO-UNDO.
DEFINE VARIABLE meddelandevar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felNr AS INTEGER NO-UNDO.
DEFINE VARIABLE alltOk AS LOGICAL NO-UNDO.
DEFINE VARIABLE anvapph AS HANDLE NO-UNDO.    /* ANVDYNAPP.P */
DEFINE VARIABLE ExtradataAppServerHandleSet AS HANDLE NO-UNDO. 
DEFINE VARIABLE ExtraDataDS AS HANDLE NO-UNDO.
DEFINE VARIABLE ExtraFieldH AS HANDLE NO-UNDO.
DEFINE VARIABLE ExtraVadHmt AS CHARACTER NO-UNDO.
DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.

DEFINE VARIABLE brwproch AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbrwh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynfrmh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE openquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE tempcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE fieldh AS HANDLE NO-UNDO.
DEFINE VARIABLE entryfieldh AS HANDLE NO-UNDO.
DEFINE VARIABLE invar AS CHARACTER NO-UNDO.
DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
DEFINE VARIABLE brwvalrowid AS ROWID NO-UNDO.
DEFINE VARIABLE lastbrwrow AS ROWID NO-UNDO.

DEFINE TEMP-TABLE gurusupportTT NO-UNDO
   FIELD GUSER AS CHARACTER LABEL "Os-användare"
   FIELD OLDGUSER AS CHARACTER
   FIELD TTRECID AS RECID 
   INDEX GUSER GUSER.
{VARFORETYP.I}
RUN STYREAPP.P  (INPUT Guru.Konstanter:globforetag, INPUT-OUTPUT varforetypval, INPUT-OUTPUT varforetypchar, INPUT Guru.Konstanter:globnystart).                  
{STYREAPPLADD.I}


/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-NAMN FILL-IN-LOSEN Btn_AVS ~
BTN_ANVANDARE BTN_BORT FILL-IN-ANV 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-NAMN FILL-IN-LOSEN FILL-IN-ANV 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANVANDARE 
     LABEL "Skapa Användare" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_AVS AUTO-GO 
     LABEL "Avsluta" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-ANV AS CHARACTER FORMAT "X(256)":U INITIAL "GURUSUPPORT" 
      VIEW-AS TEXT 
     SIZE 14 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOSEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lösenord" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAMN AS CHARACTER FORMAT "X(256)":U INITIAL "First Line Support" 
     VIEW-AS FILL-IN 
     SIZE 28.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-NAMN AT ROW 4.75 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     FILL-IN-LOSEN AT ROW 6.5 COL 30.5 COLON-ALIGNED WIDGET-ID 10
     Btn_AVS AT ROW 21.67 COL 80.5 WIDGET-ID 6
     BTN_ANVANDARE AT ROW 21.79 COL 20.5 WIDGET-ID 2
     BTN_BORT AT ROW 21.79 COL 57.5 WIDGET-ID 4
     FILL-IN-ANV AT ROW 3 COL 29.5 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     "Lösen anges endast första gången eller om det ska ändras" VIEW-AS TEXT
          SIZE 71 BY 1.5 AT ROW 6.25 COL 48 WIDGET-ID 14
          FONT 17
     SPACE(5.74) SKIP(20.82)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "GuruSupportUsers" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BTN_ANVANDARE:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       BTN_BORT:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       FILL-IN-ANV:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN-NAMN:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* GuruSupportUsers */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANVANDARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANVANDARE Dialog-Frame
ON CHOOSE OF BTN_ANVANDARE IN FRAME Dialog-Frame /* Skapa Användare */
DO:
   RUN NyAnv_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AVS Dialog-Frame
ON CHOOSE OF Btn_AVS IN FRAME Dialog-Frame /* Avsluta */
DO:
   
   RUN SparaGS_UI.
   IF Guru.GlobalaVariabler:avbryt_musz = TRUE THEN DO:
      Guru.GlobalaVariabler:avbryt_musz = FALSE.
      RETURN NO-APPLY.   
   END.   
     
   RUN Avsluta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT Dialog-Frame
ON CHOOSE OF BTN_BORT IN FRAME Dialog-Frame /* Ta bort */
DO:
   RUN TaBortAnv_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  Guru.Konstanter:varforetypval[20] = 4. /*MS Sans Serif, size=8*/
   /*FONT för */
  Guru.Konstanter:varforetypval[26] = 4. /*MS Sans Serif, size=8*/
{DIA_M_START.I}
   {ALLSTARTDYN.I} 
   RUN AnslutDS_UI. 
   RUN newbrw_UI.
   FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = "GURUSUPPORT" NO-LOCK NO-ERROR. 
   IF AVAILABLE anvandartemp THEN DO:
      FILL-IN-NAMN = anvandartemp.AV-NAMN.
   END.     
   RUN enable_UI.
   {FRMSIZED.I} 
   {musarrow.i}   
   {DIA_M_SLUT.I} 
   RUN placera_UI.
   
   
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnslutDS_UI Dialog-Frame 
PROCEDURE AnslutDS_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:appcon THEN DO:
      RUN ANVDYNAPP.P PERSISTENT SET anvapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ANVDYNAPP.P PERSISTENT SET anvapph.      
   END. 
   DEBUGGER:SET-BREAK().
   RUN hamtaenSupport_UI IN anvapph (INPUT "GURUSUPPORT",OUTPUT TABLE anvandartemp).
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMTDS.P PERSISTENT SET ExtradataAppServerHandleSet ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN EXTRADATAHMTDS.p PERSISTENT SET ExtradataAppServerHandleSet.
   END. 
    IF Guru.Konstanter:appcon THEN DO:
      IF NOT VALID-HANDLE(losenwebh) THEN RUN LOSENKOLLWEB.P PERSISTENT SET losenwebh ON Guru.Konstanter:apphand TRANSACTION DISTINCT.  
   END.
   ELSE DO:
      IF NOT VALID-HANDLE(losenwebh) THEN RUN LOSENKOLLWEB.P PERSISTENT SET losenwebh .    
   END.
   Guru.SharedVariable:ExtradataAppServerHandle = ExtradataAppServerHandleSet.
   RUN HamtAnv_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avsluta_UI Dialog-Frame 
PROCEDURE Avsluta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    Guru.Konstanter:AvslutaExtradata().
    DELETE OBJECT ExtraDataDS NO-ERROR.
    ExtraDataDS = ?.     
    DELETE OBJECT ExtradataAppServerHandleSet NO-ERROR.
    ExtradataAppServerHandleSet = ?.
    IF VALID-HANDLE(losenwebh) THEN DELETE PROCEDURE losenwebh NO-ERROR.
     
     losenwebh = ?.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE brwegenskap_UI Dialog-Frame 
PROCEDURE brwegenskap_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN 
   dynbrwh:NAME                     = "BRW_GU"
   dynbrwh:WIDTH                    = 35
   dynbrwh:HEIGHT                   = 10
   dynbrwh:COLUMN                   = 1.5
   dynbrwh:ROW                      = 5 
   dynbrwh:FONT                     = 4
   dynbrwh:MULTIPLE                 = FALSE
   dynbrwh:READ-ONLY                = FALSE
   dynbrwh:ROW-MARKERS              = TRUE
   dynbrwh:SEPARATORS               = TRUE
   dynbrwh:SENSITIVE                = TRUE
   dynbrwh:ALLOW-COLUMN-SEARCHING   = TRUE
   dynbrwh:EXPANDABLE               = FALSE
   dynbrwh:VISIBLE                  = TRUE
   dynbrwh:COLUMN-SCROLLING         = TRUE
   dynbrwh:COLUMN-RESIZABLE         = FALSE
   dynbrwh:MAX-DATA-GUESS           = 100
   dynbrwh:TITLE                    = "GuruSupportUsers".
   ASSIGN
   dynbrwh:FRAME = FRAME {&FRAME-NAME}:HANDLE.
   ASSIGN
   dynbrwh:ROW                      = 1
   dynbrwh:WIDTH                    = dynbrwh:FRAME:WIDTH - 1.75 
   dynbrwh:HEIGHT                   = dynbrwh:FRAME:HEIGHT - 2.
   dynbuffh = TEMP-TABLE gurusupportTT:DEFAULT-BUFFER-HANDLE.
   IF dynbuffh NE ? THEN DO:
      CREATE QUERY dynqueh.
      dynqueh:SET-BUFFERS(dynbuffh).
      dynbuffh = dynqueh:GET-BUFFER-HANDLE. 
      openquery = "FOR EACH " + dynbuffh:TABLE + " NO-LOCK".
      dynqueh:QUERY-PREPARE(openquery).
      /*Sätt queryn*/
      dynbrwh:QUERY = dynqueh.
   END.    
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createfields_UI Dialog-Frame 
PROCEDURE createfields_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE prolista AS CHARACTER NO-UNDO.
   DEFINE VARIABLE fillchar AS CHARACTER NO-UNDO FORMAT "x" INITIAL " ".
   DEFINE VARIABLE rubrikvar AS CHARACTER NO-UNDO.
   IF dynbuffh = ? THEN DO:
      MESSAGE "Något är fel kontakta Elpool i Umeå AB 090/184540"
      VIEW-AS ALERT-BOX.
      RETURN.
   END.   
   DEFINE VARIABLE langd AS INTEGER NO-UNDO.
   /*Skapa och lägg till de fält som ska finnas med*/
   DEFINE VARIABLE frmh AS HANDLE NO-UNDO.
   frmh = dynbrwh:FRAME.
   frmh:HIDDEN = FALSE.
   dynbrwh:HIDDEN = FALSE.
   fieldh = dynbuffh:BUFFER-FIELD("GUSER").
   fieldh:FORMAT = "x(256)".
   tmpcolh = dynbrwh:ADD-LIKE-COLUMN(fieldh).
   ASSIGN
   tmpcolh:WIDTH-CHARS = 12
   tmpcolh:LABEL = "Os-Användare"
   tmpcolh:READ-ONLY = false
   tmpcolh:VISIBLE = TRUE.  
   ON 'ENTRY':U OF tmpcolh PERSISTENT RUN entry_UI IN THIS-PROCEDURE.
   ON 'LEAVE':U OF tmpcolh PERSISTENT RUN leave_UI IN THIS-PROCEDURE.

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dynbrw_UI Dialog-Frame 
PROCEDURE dynbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN DYNBRW.P PERSISTENT SET brwproch (INPUT dynbrwh).
/*
   RUN dynprogextra IN brwproch (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN brwproch (INPUT TRUE).
   */
   RUN brwsetupstop_UI IN brwproch (INPUT 1). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FILL-IN-NAMN FILL-IN-LOSEN FILL-IN-ANV 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-NAMN FILL-IN-LOSEN Btn_AVS BTN_ANVANDARE BTN_BORT FILL-IN-ANV 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE entry_UI Dialog-Frame 
PROCEDURE entry_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   entryfieldh = dynbuffh:BUFFER-FIELD(SELF:NAME) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extratrigg_UI Dialog-Frame 
PROCEDURE extratrigg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HamtAnv_UI Dialog-Frame 
PROCEDURE HamtAnv_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ExtraDataDS = ?.
   ExtraVadHmt =  ' EXTRADATA.PROGRAM = "GURUSUPPORT"' .
   RUN laddaExtraDataDS_UI IN ExtradataAppServerHandleSet (INPUT ExtraVadHmt,OUTPUT DATASET-HANDLE ExtraDataDS BIND).
   IF VALID-HANDLE(ExtraDataDS) THEN DO:
      Guru.SharedVariable:ExtraDataDS = ExtraDataDS.
      Guru.Konstanter:ExtradataLaddTracking().
   END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leave_UI Dialog-Frame 
PROCEDURE leave_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   dynbrwh:SELECT-FOCUSED-ROW() NO-ERROR.
   fieldh = entryfieldh.
   brwrowid = dynbuffh:ROWID.
   gurusupportTT.GUSER = SELF:SCREEN-VALUE.
  
   RUN sparaDS_UI.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newbrw_UI Dialog-Frame 
PROCEDURE newbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF VALID-HANDLE(brwproch) THEN DELETE PROCEDURE brwproch NO-ERROR.
   IF VALID-HANDLE(dynbrwh) THEN DELETE OBJECT dynbrwh NO-ERROR.
   IF VALID-HANDLE(dynqueh) THEN DELETE OBJECT dynqueh NO-ERROR.
   IF VALID-HANDLE(dynbuffh) THEN DO:
      DELETE OBJECT dynbuffh NO-ERROR.
   END.   
   IF VALID-HANDLE(tmpcolh) THEN DELETE OBJECT tmpcolh NO-ERROR.
   IF VALID-HANDLE(fieldh) THEN DELETE OBJECT fieldh NO-ERROR.
   ASSIGN
   brwproch = ?
   dynbrwh = ?
   dynqueh = ?
   dynbuffh = ?
   tmpcolh = ?
   fieldh = ?. 
   CREATE BROWSE dynbrwh.
   RUN extratrigg_UI.
   dynbrwh:HIDDEN = TRUE.
   RUN brwegenskap_UI.
   IF dynbuffh = ? THEN RETURN.
   dynbrwh:HIDDEN = TRUE.
   /*Skapa fast fält*/
   RUN createfields_UI. 
   /*Öppna queryn*/
   dynbrwh:HIDDEN = FALSE.
   RUN dynbrw_UI.
   RUN openbrw_UI.
   dynbrwh:SENSITIVE  = TRUE.  
   dynbrwh:HIDDEN = FALSE.
   dynbrwh:DESELECT-ROWS() NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyAnv_UI Dialog-Frame 
PROCEDURE NyAnv_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST gurusupportTT WHERE gurusupportTT.GUSER = "Ange-os-user" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE gurusupportTT THEN DO:
      CREATE gurusupportTT.
      gurusupportTT.GUSER = "Ange-os-user".
   END.  
   RUN SparaDS_UI. 
   RUN setlastrowid_UI IN brwproch (INPUT ROWID(gurusupportTT)).
   RUN openbdynspec_UI IN brwproch.
   RUN lastselectdyn_UI IN brwproch. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openbrw_UI Dialog-Frame 
PROCEDURE openbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   queryvar =  "FOR EACH " + Guru.SharedVariable:ExtradataDsbufftH:TABLE.
   Guru.Konstanter:CreateCustomQuery(Guru.SharedVariable:ExtradataDsbufftH,queryvar).
   Guru.GlobalaVariabler:CustomQueryDyn:QUERY-OPEN().
   Guru.GlobalaVariabler:CustomQueryDyn:GET-FIRST().
   DO WHILE Guru.GlobalaVariabler:CustomQueryDyn:QUERY-OFF-END = FALSE:    
      CREATE gurusupportTT.
      gurusupportTT.GUSER = Guru.SharedVariable:ExtradataDsbufftH:BUFFER-FIELD("HUVUDCH"):BUFFER-VALUE.
      
      gurusupportTT.OLDGUSER = Guru.SharedVariable:ExtradataDsbufftH:BUFFER-FIELD("HUVUDCH"):BUFFER-VALUE.
      gurusupportTT.TTRECID = Guru.SharedVariable:ExtradataDsbufftH:BUFFER-FIELD("TTRECID"):BUFFER-VALUE.
      Guru.GlobalaVariabler:CustomQueryDyn:GET-NEXT().
  END.     
       
  RUN openbdynspec_UI IN brwproch.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE placera_UI Dialog-Frame 
PROCEDURE placera_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   ASSIGN 
   tmpcolh:WIDTH-CHARS              = 12    
   dynbrwh:WIDTH                    = 50
   dynbrwh:HEIGHT                   = 15
   dynbrwh:COLUMN                   = FILL-IN-ANV:COLUMN IN FRAME {&FRAME-NAME} 
   dynbrwh:ROW                      = 10. 
   
   Guru.GlobalaVariabler:StartKolumnForKnappar = dynbrwh:COLUMN.
   Guru.Konstanter:PlaceraKnapparVagratt(BTN_ANVANDARE:HANDLE ,TRUE).
   Guru.Konstanter:PlaceraKnapparVagratt(BTN_BORT:HANDLE,TRUE).  
   Guru.GlobalaVariabler:StartRadForKnappar = dynbrwh:ROW.
   Guru.Konstanter:PlaceraKnapparLodratt(dynbrwh). 
   Guru.Konstanter:PlaceraKnapparLodratt(BTN_ANVANDARE:HANDLE). 
   ASSIGN  
   BTN_BORT:ROW = BTN_ANVANDARE:ROW
   BTN_AVS:ROW = BTN_ANVANDARE:ROW.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SparaGS_UI Dialog-Frame 
PROCEDURE SparaGS_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   Guru.GlobalaVariabler:avbryt_musz = FALSE.  
   ASSIGN 
   FILL-IN-ANV = INPUT FRAME {&FRAME-NAME} FILL-IN-ANV 
   FILL-IN-NAMN INPUT FRAME {&FRAME-NAME} FILL-IN-NAMN 
   FILL-IN-LOSEN = INPUT FRAME {&FRAME-NAME} FILL-IN-LOSEN.
   
   FILL-IN-LOSEN = INPUT FRAME {&FRAME-NAME} FILL-IN-LOSEN.
   FIND FIRST anvandartemp WHERE NO-LOCK NO-ERROR. 
   IF AVAILABLE anvandartemp THEN DO:
      IF FILL-IN-LOSEN NE "" THEN  DO:
         RUN NylosenReglerKoll_UI IN losenwebh (INPUT FILL-IN-ANV,INPUT-OUTPUT FILL-IN-LOSEN,INPUT "", OUTPUT felNr, OUTPUT alltOk,OUTPUT meddelandevar).
         IF alltOk = FALSE THEN DO:
            Guru.GlobalaVariabler:avbryt_musz = TRUE.
            MESSAGE meddelandevar
            VIEW-AS ALERT-BOX.
            RETURN.
         END.
         anvandartemp.AV-LOSEN = FILL-IN-LOSEN.
         IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN DO:
            anvandartemp.AV-LOSEN = Guru.Konstanter:SaltRetur(FILL-IN-LOSEN).
         END.   
      END.
   END.     
   ELSE DO:
      IF FILL-IN-LOSEN = "" THEN DO:
         MESSAGE "Lösenord får inte vara blankt!"
         VIEW-AS ALERT-BOX.
         Guru.GlobalaVariabler:avbryt_musz = TRUE.  
         RETURN.
      END. 
      RUN NylosenReglerKoll_UI IN losenwebh (INPUT FILL-IN-ANV,INPUT-OUTPUT FILL-IN-LOSEN,INPUT "", OUTPUT felNr, OUTPUT alltOk,OUTPUT meddelandevar).
      IF alltOk = FALSE THEN DO:
         Guru.GlobalaVariabler:avbryt_musz = TRUE.
         MESSAGE meddelandevar
         VIEW-AS ALERT-BOX.
         RETURN.
      END.
      CREATE anvandartemp.
      ASSIGN
      anvandartemp.ANVANDARE = FILL-IN-ANV
      anvandartemp.AV-NAMN = FILL-IN-NAMN
      anvandartemp.AV-LOSEN = FILL-IN-LOSEN
      anvandartemp.SKAPAD-AV = CAPS(Guru.Konstanter:globanv)
      anvandartemp.SKAPAD-DATUMTID = NOW.
      IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN DO:
         IF FILL-IN-LOSEN = "" THEN. 
         ELSE anvandartemp.AV-LOSEN = Guru.Konstanter:SaltRetur(FILL-IN-LOSEN).
      END.   
   END.
   EMPTY TEMP-TABLE exanvandartemp NO-ERROR. 
   CREATE exanvandartemp.
   BUFFER-COPY anvandartemp TO exanvandartemp.
   RUN sparpers_UI IN anvapph (INPUT Guru.Konstanter:globanv,INPUT "", INPUT "",INPUT TABLE exanvandartemp,INPUT TABLE perstemp). 
      
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Spara_UI Dialog-Frame 
PROCEDURE SparaDS_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   Guru.SharedVariable:ExtradataDsbufftH:FIND-FIRST("WHERE HUVUDCH = " + QUOTER(gurusupportTT.OLDGUSER),NO-LOCK) NO-ERROR.
   IF Guru.SharedVariable:ExtradataDsbufftH:AVAILABLE THEN DO:
      Guru.SharedVariable:ExtradataDsbufftH:BUFFER-FIELD("HUVUDCH"):BUFFER-VALUE = gurusupportTT.GUSER.
      gurusupportTT.OLDGUSER = gurusupportTT.GUSER.
   END.
   ELSE DO:
      Guru.SharedVariable:ExtradataDsbufftH:BUFFER-CREATE().
      ASSIGN 
      Guru.SharedVariable:ExtradataDsbufftH:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = "GURUSUPPORT"
      Guru.SharedVariable:ExtradataDsbufftH:BUFFER-FIELD("HUVUDCH"):BUFFER-VALUE = gurusupportTT.GUSER.
      gurusupportTT.OLDGUSER = gurusupportTT.GUSER.
   END.    
   Guru.Konstanter:ExtraDataDSSpar(TRUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TaBortAnv_UI Dialog-Frame 
PROCEDURE TaBortAnv_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   MESSAGE "Vill du ta bort Användaren  - "gurusupportTT.GUSER 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.
   IF svar = TRUE THEN DO:
      Guru.SharedVariable:ExtradataDsbufftH:FIND-FIRST("WHERE HUVUDCH = " + QUOTER(gurusupportTT.OLDGUSER),NO-LOCK) NO-ERROR.
      IF Guru.SharedVariable:ExtradataDsbufftH:AVAILABLE THEN DO:
         Guru.SharedVariable:ExtradataDsbufftH:BUFFER-DELETE().
      END.
      DELETE gurusupportTT.
      RUN selnextprevrow_UI IN brwproch.
      Guru.Konstanter:ExtraDataDSSpar(FALSE).
      RUN openbdynspec_UI IN brwproch.
      RUN lastselectdyn_UI IN brwproch.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

