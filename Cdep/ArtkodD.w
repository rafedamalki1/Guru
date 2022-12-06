&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE OUTPUT PARAMETER rekvar AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE knamnapph AS HANDLE NO-UNDO.   /*KNAMNAPP.P*/
&Scoped-define NEW NEW
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE ktoproch AS HANDLE NO-UNDO. /* Procedure handle till KTOAPP.P */
 

DEFINE VARIABLE brwsortvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempkonto AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempkontonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempjur AS CHARACTER NO-UNDO.
DEFINE VARIABLE montbervar AS INTEGER NO-UNDO.
DEFINE VARIABLE projektledvar AS INTEGER NO-UNDO.
DEFINE VARIABLE projektorvar AS INTEGER NO-UNDO.
DEFINE VARIABLE dokuvar AS INTEGER NO-UNDO.

  
DEFINE NEW SHARED TEMP-TABLE ktotemp               
   FIELD KONTO AS CHARACTER                        
   FIELD KONTONR AS CHARACTER                      
   FIELD BENAMNING AS CHARACTER                    
   FIELD JUDID AS CHARACTER                        
   FIELD VIJUDID AS CHARACTER                      
   FIELD OMRADE AS CHARACTER                       
   FIELD AVDELNINGNR AS INTEGER                    
   FIELD AKTIV AS LOGICAL                          
   FIELD RECTIDVIS AS RECID                        
   INDEX KONTO KONTO KONTONR.                      
DEFINE NEW SHARED TEMP-TABLE kbtemp
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER
   FIELD K5 AS CHARACTER.
DEFINE NEW SHARED TEMP-TABLE jurtemp
   FIELD JUDID AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD VIJUDID AS CHARACTER.   

DEFINE VARIABLE persproch AS HANDLE NO-UNDO.
DEFINE VARIABLE vadgora AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE borec AS RECID NO-UNDO.
DEFINE VARIABLE borec2 AS RECID NO-UNDO.
DEFINE VARIABLE ktorec AS RECID NO-UNDO.
DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE radnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE brec AS RECID NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_K-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ktotemp

/* Definitions for BROWSE BRW_K-3                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_K-3 ktotemp.KONTONR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K-3 
&Scoped-define QUERY-STRING-BRW_K-3 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k3" NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_K-3 OPEN QUERY BRW_K-3 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k3" NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_K-3 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K-3 ktotemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_K-3 Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-VALJ 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-VALJ AS CHARACTER FORMAT "X(256)":U 
     LABEL "art" 
     VIEW-AS FILL-IN 
     SIZE 18.75 BY 1
     FGCOLOR 12 FONT 17 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K3 AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS " Blank" 
     SIZE 12 BY 5.5 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_K-3 FOR 
      ktotemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_K-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K-3 Dialog-Frame _STRUCTURED
  QUERY BRW_K-3 NO-LOCK DISPLAY
      ktotemp.KONTONR COLUMN-LABEL "" FORMAT "x(256)":U WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 11.75 BY 9.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-VALJ AT ROW 3.08 COL 7.5 WIDGET-ID 2
     BRW_K-3 AT ROW 4.83 COL 16.5 WIDGET-ID 200
     SEL_K3 AT ROW 7.08 COL 16.5 NO-LABEL WIDGET-ID 4
     Btn_OK AT ROW 15.5 COL 29
     "Dubbel klicka på raden för att välja!" VIEW-AS TEXT
          SIZE 38 BY 1.5 AT ROW 1.25 COL 4 WIDGET-ID 6
          FONT 17
     SPACE(7.87) SKIP(16.49)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         DEFAULT-BUTTON Btn_OK WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: ktotemp T "?" NO-UNDO temp-db ktotemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_K-3 FILL-IN-VALJ Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VALJ IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-VALJ:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_K3 IN FRAME Dialog-Frame
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_K3:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K-3
/* Query rebuild information for BROWSE BRW_K-3
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k3"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "" "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K-3
&Scoped-define SELF-NAME BRW_K-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K-3 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K-3 IN FRAME Dialog-Frame
DO:
   FILL-IN-VALJ = TRIM(STRING(ktotemp.KONTONR)).
   
   IF FILL-IN-VALJ = "" OR FILL-IN-VALJ ="Blank" THEN DO:
      rekvar = 0.
   END.   
   ELSE DO:
      rekvar = INTEGER(FILL-IN-VALJ).                 
   END.
   DISPLAY  FILL-IN-VALJ WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K-3 Dialog-Frame
ON VALUE-CHANGED OF BRW_K-3 IN FRAME Dialog-Frame
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.     
   
   {&BROWSE-NAME}:TOOLTIP = ktotemp.BENAMNING.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   RUN avslut_UI.

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
  {DIA_M_START.I}  
   {ALLSTARTDYN.I}      
   RUN enable_UI.
  /* {FRMSIZED.I}*/   
  
   RUN kontostrang_UI.
   
   {DIA_M_SLUT.I}
   
   {musarrow.i}
  
  
  
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
      RUN DYNBRW.P PERSISTENT SET brwproc[13]
      (INPUT BRW_K-3:HANDLE IN FRAME {&FRAME-NAME}).         
   /*namn*/
   IF Guru.Konstanter:appcon THEN DO:
      RUN KNAMNAPP.P PERSISTENT SET knamnapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN KNAMNAPP.P PERSISTENT SET knamnapph.
   END.  
   IF Guru.Konstanter:appcon THEN DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN PERSONALAPP.P PERSISTENT SET persproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.   
   END.
   ELSE DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN PERSONALAPP.P PERSISTENT SET persproch.      
                     
   END.
   
    
   /*konton*/
   IF Guru.Konstanter:appcon THEN DO:                           
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN KTOAPP.P PERSISTENT SET ktoproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
   END.
   ELSE DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN KTOAPP.P PERSISTENT SET ktoproch (INPUT Guru.Konstanter:globanv).   
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avslut_UI Dialog-Frame 
PROCEDURE avslut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {BORTBRWPROC.I}
   IF VALID-HANDLE(knamnapph) THEN DELETE PROCEDURE knamnapph.
   IF VALID-HANDLE(ktoproch) THEN DELETE PROCEDURE ktoproch.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN. 
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
  DISPLAY FILL-IN-VALJ 
      WITH FRAME Dialog-Frame.
  ENABLE BRW_K-3 Btn_OK 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kontorad_UI Dialog-Frame 
PROCEDURE kontorad_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST kbtemp NO-LOCK NO-ERROR.
   IF VALID-HANDLE(ktoproch) THEN RUN kbhmt_UI IN ktoproch (INPUT Guru.Konstanter:globforetag,OUTPUT TABLE kbtemp).                  
   FIND FIRST kbtemp NO-ERROR.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kontostrang_UI Dialog-Frame 
PROCEDURE kontostrang_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF VALID-HANDLE(ktoproch) THEN RUN kbhmt_UI IN ktoproch (INPUT Guru.Konstanter:globforetag,OUTPUT TABLE kbtemp).
   FIND FIRST kbtemp NO-ERROR.
   FIND FIRST ktotemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ktotemp THEN DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN KHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 1,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE ktotemp).                        
      END.
      ELSE DO:
         RUN KHMT.P 
         (INPUT 1,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE ktotemp).      
      END.        
   END.
   FIND FIRST ktotemp WHERE ktotemp.KONTO = "K3" AND ktotemp.KONTONR = " Blank" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ktotemp THEN DO:
      CREATE ktotemp.   
      ASSIGN
      ktotemp.KONTO = "K3"
      ktotemp.KONTONR = " Blank"   
      ktotemp.JUDID = "".   
   
   END.
   FILL-IN-VALJ:LABEL  IN FRAME {&FRAME-NAME} = "Vald " + CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2)).
   ASSIGN 
   SEL_K3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " Blank".
   ASSIGN
   ktotemp.KONTONR:LABEL IN BROWSE BRW_K-3 = CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2)).
   FRAME {&FRAME-NAME} :TITLE = CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2)).
   RUN open_UI.
   DISPLAY  FILL-IN-VALJ WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open_UI Dialog-Frame 
PROCEDURE open_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   brwsortvar = ' ktotemp.KONTO = "K3" '.
   RUN setcolsortvar_UI IN brwproc[13] (INPUT brwsortvar).
   RUN openbdynspec_UI IN brwproc[13].                 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

