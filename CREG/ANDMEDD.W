&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 03/25/96 -  4:43 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
                         

DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE varmess AS CHARACTER NO-UNDO.
DEFINE VARIABLE varerror AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE apphandgran AS HANDLE NO-UNDO.
DEFINE VARIABLE appcongran AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE knappval AS CHARACTER NO-UNDO.
DEFINE SHARED TEMP-TABLE meddpers    
   FIELD NAMN AS CHARACTER FORMAT "X(40)" 
   FIELD ANVANDARE AS CHARACTER  FORMAT "X(12)"
   INDEX ANV IS PRIMARY ANVANDARE ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS EDITOR_MEDD BTN_SKR BTN_OK BNT_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_MOTTAGARE FILL-IN_SANDARE ~
EDITOR_MEDD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BNT_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKR 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE VARIABLE EDITOR_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 20 NO-UNDO.

DEFINE VARIABLE FILL-IN_MOTTAGARE AS CHARACTER FORMAT "x(12)" 
     LABEL "Mottagare" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE FILL-IN_SANDARE AS CHARACTER FORMAT "x(12)" 
     LABEL "S?ndare" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_MOTTAGARE AT ROW 3.33 COL 10.5 COLON-ALIGNED
     FILL-IN_SANDARE AT ROW 3.33 COL 35 COLON-ALIGNED
     EDITOR_MEDD AT ROW 5.33 COL 1.5 NO-LABEL
     BTN_SKR AT ROW 12.75 COL 55.5
     BTN_OK AT ROW 25.75 COL 40.5
     BNT_AVB AT ROW 25.75 COL 55.5
     "Meddelande" VIEW-AS TEXT
          SIZE 33 BY 1.29 AT ROW 1.33 COL 1.5
          FONT 17
     SPACE(35.74) SKIP(24.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Medelande till Guruanv?ndare".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MOTTAGARE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_SANDARE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BNT_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BNT_AVB DIALOG-1
ON CHOOSE OF BNT_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   {muswait.i}   
   EDITOR_MEDD = INPUT EDITOR_MEDD.
   {ANDMEDIN.I}
   IF knappval = "ALLA" OR knappval = "MARKERADE" THEN DO:
      
   END.  
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      {AMERICANEUROPEAN.I}
      OUTPUT TO VALUE(Guru.Konstanter:guruvar + "gurumed.txt2").
      PUT UNFORMATTED  EDITOR_MEDD SKIP.
      OUTPUT CLOSE.
      kommando = SEARCH("quoter.exe").
      IF kommando NE ? THEN OS-COMMAND SILENT VALUE(kommando) VALUE(Guru.Konstanter:guruvar + "gurumed.txt2") > VALUE(Guru.Konstanter:guruvar + "gurumed.txt").
      {EUROPEANAMERICAN.I}  
   END.
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-1
ON CHOOSE OF BTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT FALSE).          
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      RUN ut_UI.      
   END.   
   {musarrow.i}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-1
ON MOUSE-MENU-CLICK OF BTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDITOR_MEDD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR_MEDD DIALOG-1
ON LEAVE OF EDITOR_MEDD IN FRAME DIALOG-1
DO:
   EDITOR_MEDD = INPUT EDITOR_MEDD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I} 
   ASSIGN 
   FILL-IN_MOTTAGARE = knappval
   FILL-IN_SANDARE = Guru.Konstanter:globanv.
  
   RUN enable_UI.       
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN_MOTTAGARE FILL-IN_SANDARE EDITOR_MEDD 
      WITH FRAME DIALOG-1.
  ENABLE EDITOR_MEDD BTN_SKR BTN_OK BNT_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE flermed2_UI DIALOG-1 
PROCEDURE flermed2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER gfore AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
/*
   DEFINE VARIABLE conappvar AS CHARACTER NO-UNDO.
   CREATE SERVER apphandgran.
   ASSIGN
   conappvar = valdbtemp.APPCON.
   IF conappvar = "" THEN DO:
      MESSAGE "Kontakta Elpool tel 090/184540 f?r du kan inte ansluta korrekt!"
      VIEW-AS ALERT-BOX.
   END.
    /*obs case-sensitv -AppService appguru9*/
   ELSE DO:
      Guru.Konstanter:appcongran = apphandgran:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gfore).       
   END.
   IF NOT Guru.Konstanter:appcongran THEN DO:
      MESSAGE 
      "Du fick nu en massa fel meddelanden." Skip
      "Dessa meddelanden inneb?r att det inte g?r att skicka meddelande till " gfore skip 
      "Kontakta system ansvarig." 
      VIEW-AS ALERT-BOX.             
   END.
   ELSE DO:
      RUN APASMEDD.P ON apphandgran TRANSACTION DISTINCT 
      (INPUT EDITOR_MEDD, INPUT Guru.Konstanter:globanv).
      IF Guru.Konstanter:appcongran THEN Guru.Konstanter:appcongran = apphandgran:DISCONNECT().
      DELETE OBJECT apphandgran NO-ERROR.
   END.
   */
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE flermed_UI DIALOG-1 
PROCEDURE flermed_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* {FLERMEDU.I}*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI DIALOG-1 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/            
   {AMERICANEUROPEAN.I}
   {PRINTSTAENDE.I}           
   DISPLAY "MEDELANDE FR?N :" AT 6
   FILL-IN_SANDARE AT 23 NO-LABEL  TODAY AT 38
           "TILL           :" AT 6 
   FILL-IN_MOTTAGARE AT 23  NO-LABEL
   EDITOR_MEDD AT 6  VIEW-AS EDITOR SIZE 50 BY 9 NO-LABEL.
   OUTPUT CLOSE. 
   {EUROPEANAMERICAN.I} 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

