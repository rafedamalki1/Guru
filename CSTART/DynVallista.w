&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
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
{ALLDEF.I}
DEFINE VARIABLE brwh AS HANDLE NO-UNDO EXTENT 50.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_ALLBACK Btn_OK BTN_ALLOVER BTN_BACK ~
BTN_OVER 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda konstruktioner tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla konstruktioner väljs"
     FONT 11.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BTN_ALLBACK AT ROW 14.67 COL 46 WIDGET-ID 10
     Btn_OK AT ROW 24.75 COL 82.5
     BTN_ALLOVER AT ROW 7.75 COL 46 WIDGET-ID 12
     BTN_BACK AT ROW 12.33 COL 46 WIDGET-ID 14
     BTN_OVER AT ROW 10.04 COL 46 WIDGET-ID 18
     SPACE(48.87) SKIP(15.07)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Välj"
         DEFAULT-BUTTON Btn_OK WIDGET-ID 100.


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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Välj */
DO:
  APPLY "END-ERROR":U TO SELF.
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
  
  RUN enable_UI.
  FRAME {&FRAME-NAME}:HIDDEN = TRUE. 
    {FRMSIZED.I}
   {ALLSTARTDYN.I} 
   {DIA_M_SLUT.I}
   
  
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
   RUN skapabrw_UI. 
   
   
   
   RUN DYNARROW.P PERSISTENT SET Arrowbrwproc[1] (INPUT brwh[1],INPUT brwh[2],
                                                        INPUT BTN_OVER:HANDLE IN FRAME {&FRAME-NAME}, INPUT BTN_ALLOVER:HANDLE IN FRAME {&FRAME-NAME},
                                                        INPUT BTN_ALLBACK:HANDLE IN FRAME {&FRAME-NAME},INPUT BTN_BACK:HANDLE IN FRAME {&FRAME-NAME}).
  
   
   RUN openbdynspec_UI IN brwproc[1].
   RUN openbdynspec_UI IN brwproc[2].
   RUN PlaceraKnapp_UI.
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
  ENABLE BTN_ALLBACK Btn_OK BTN_ALLOVER BTN_BACK BTN_OVER 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PlaceraKnapp_UI Dialog-Frame 
PROCEDURE PlaceraKnapp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   Guru.GlobalaVariabler:StartKolumnForKnappar = FRAME {&FRAME-NAME}:HANDLE:WIDTH-CHARS.  
   Guru.Konstanter:PlaceraKnapparVagrattFranHoger(BTN_OK:HANDLE).
   
   Guru.GlobalaVariabler:StartKolumnForKnappar = BTN_OVER:COLUMN.  
   Guru.Konstanter:PlaceraKnapparVagrattFranHoger(brwh[1]).
   Guru.GlobalaVariabler:StartKolumnForKnappar = BTN_OVER:COLUMN + 4.
   Guru.Konstanter:PlaceraKnapparVagratt(brwh[2],TRUE).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapabrw_UI Dialog-Frame 
PROCEDURE skapabrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE brwantal AS INTEGER NO-UNDO.
   DEFINE VARIABLE x-multi AS DECIMAL NO-UNDO.
   DEFINE VARIABLE y-multi AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE radvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE colvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE brvnr AS INTEGER NO-UNDO.
    
   brvnr = 1.
   REPEAT:
      IF brvnr > 2 THEN LEAVE.
      RUN DYNBRWINIT.P PERSISTENT SET brwproc[brvnr].
      RUN newbrw_UI IN brwproc[brvnr] (INPUT THIS-PROCEDURE).
      RUN brw_UI IN brwproc[brvnr] (OUTPUT brwh[brvnr]).
      /*FRAME-MTRL*/
      IF Guru.GlobalaVariabler:ttBuffHandleVallistaDyn:NAME = "VLlevtemp" THEN DO: 
         IF brvnr = 1 THEN  RUN brwegenskap_UI IN brwproc[brvnr] (INPUT Guru.GlobalaVariabler:ttBuffHandleVallistaDyn,INPUT "BRW_VLISTAN", INPUT 124, INPUT 10, INPUT TRUE, INPUT TRUE, INPUT "Välj ", INPUT FRAME {&FRAME-NAME}:HANDLE).
         ELSE RUN brwegenskap_UI IN brwproc[brvnr] (INPUT Guru.GlobalaVariabler:ttBuffHandleValdaDyn,INPUT "BRW_VALDA", INPUT 124, INPUT 10, INPUT TRUE, INPUT TRUE, INPUT "Valda ", INPUT  FRAME {&FRAME-NAME}:HANDLE).
      END.
      brwh[brvnr]:PRIVATE-DATA = "ejh".
      RUN getsizf_UI IN framesizeh (OUTPUT x-multi, OUTPUT y-multi).
                         
      /*                                                          bredd     höjd     kol      rad*/
      RUN brwstorlek_UI IN brwproc[brvnr](INPUT 50, INPUT 20, INPUT 1.5, INPUT 1.9, INPUT x-multi, INPUT y-multi).
      /*
      ELSE RUN brwstorlek_UI IN brwproc[brvnr](INPUT 50, INPUT 20, INPUT 100.5, INPUT 1.9, INPUT x-multi, INPUT y-multi).
      */
      RUN fieldinit_UI IN brwproc[brvnr].   
      IF Guru.GlobalaVariabler:ttBuffHandleVallistaDyn:NAME = "VLlevtemp" THEN DO:
         RUN createfields_UI IN brwproc[brvnr] (INPUT "ORDNING", INPUT "O", INPUT 20, INPUT "99999", INPUT TRUE).
         RUN VisibleField_UI IN brwproc[brvnr] (INPUT "ORDNING", INPUT FALSE ).
         RUN createfields_UI IN brwproc[brvnr] (INPUT "LEVKOD", INPUT "Lev-id", INPUT 5, INPUT "x(256)", INPUT TRUE).
         RUN createfields_UI IN brwproc[brvnr] (INPUT "LEVNAMN", INPUT "Leverantörsnamn", INPUT 20, INPUT "x(256)", INPUT TRUE).
         
      END. 
      RUN brwsetupstop_UI IN brwproc[brvnr] (INPUT 1).
      RUN fieldslut_UI IN brwproc[brvnr].
      brvnr = brvnr + 1.                      
   END.  
   END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

