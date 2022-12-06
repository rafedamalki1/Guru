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

  Created: 95/05/03 -  2:05 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER rad AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}

&SCOPED-DEFINE NEW 
&Scoped-define SHARED SHARED
{KOSTREG.I}
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE kostrec2 AS RECID NO-UNDO.
DEFINE VARIABLE kostrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_LEVKOD FILL-IN_FAKTNR ~
FILL-IN_PERSKOST FILL-IN_TRAKTKOST FILL-IN_MASKKOST FILL-IN_MTRL ~
FILL-IN_OVRKR FILL-IN_MOMS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_FAKTNR AS CHARACTER FORMAT "x(15)" 
     LABEL "Faktura.nr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE FILL-IN_LEVKOD AS CHARACTER FORMAT "x(4)" 
     LABEL "Lev-id" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE FILL-IN_MASKKOST AS DECIMAL FORMAT "->>>>9" INITIAL 0 
     LABEL "Mask.kost." 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE FILL-IN_MOMS AS INTEGER FORMAT "->>>>>>>" INITIAL 0 
     LABEL "Moms" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE FILL-IN_MTRL AS INTEGER FORMAT "->>>>>>" INITIAL 0 
     LABEL "Mtrl kr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE FILL-IN_OVRKR AS INTEGER FORMAT "->>>>>>" INITIAL 0 
     LABEL "Övrig kostnad" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE FILL-IN_PERSKOST AS DECIMAL FORMAT "->>>>9" INITIAL 0 
     LABEL "Pers.kost." 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE FILL-IN_TRAKTKOST AS DECIMAL FORMAT "->>>>9" INITIAL 0 
     LABEL "Trakt.kost." 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_LEVKOD AT ROW 4.67 COL 12.13 COLON-ALIGNED
     FILL-IN_FAKTNR AT ROW 4.67 COL 35 COLON-ALIGNED
     FILL-IN_PERSKOST AT ROW 6.17 COL 12.13 COLON-ALIGNED
     FILL-IN_TRAKTKOST AT ROW 6.17 COL 19.25
     FILL-IN_MASKKOST AT ROW 6.17 COL 47.25 COLON-ALIGNED
     FILL-IN_MTRL AT ROW 7.67 COL 12.13 COLON-ALIGNED
     FILL-IN_OVRKR AT ROW 7.67 COL 30.25 COLON-ALIGNED
     FILL-IN_MOMS AT ROW 7.67 COL 47.25 COLON-ALIGNED
     BTN_OK AT ROW 9.21 COL 28.25
     BTN_AVB AT ROW 9.21 COL 43.25
     "  Vill du verkligen ta bort denna kostnadsregistrering?" VIEW-AS TEXT
          SIZE 58.5 BY 1.96 AT ROW 1.54 COL 1.5
          FONT 17
     SPACE(0.00) SKIP(7.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Borttag av kostnadsregistrering":L.


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

/* SETTINGS FOR FILL-IN FILL-IN_FAKTNR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_LEVKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_MASKKOST IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_MOMS IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_MTRL IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_OVRKR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_PERSKOST IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_TRAKTKOST IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Borttag av kostnadsregistrering */
DO:
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Borttag av kostnadsregistrering */
DO:
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:   
   {muswait.i}  
   musz = FALSE.     
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
   FIND FIRST kostregtemp WHERE kostregtemp.AONR = aonrvar AND 
   kostregtemp.DELNR = delnrvar AND kostregtemp.RADNR = rad NO-LOCK NO-ERROR.
   FILL-IN_FAKTNR = kostregtemp.FAKTNR.
   FILL-IN_LEVKOD = kostregtemp.LEVKOD.
   FILL-IN_MASKKOST = kostregtemp.MASKKOST.
   FILL-IN_MOMS = kostregtemp.MOMS. 
   FILL-IN_MTRL = kostregtemp.MTRL. 
   FILL-IN_OVRKR = kostregtemp.OVRKR. 
   FILL-IN_PERSKOST = kostregtemp.PERSKOST.   
   FILL-IN_TRAKTKOST = kostregtemp.TRAKTKOST.
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
  DISPLAY FILL-IN_LEVKOD FILL-IN_FAKTNR FILL-IN_PERSKOST FILL-IN_TRAKTKOST 
          FILL-IN_MASKKOST FILL-IN_MTRL FILL-IN_OVRKR FILL-IN_MOMS 
      WITH FRAME DIALOG-1.
  ENABLE BTN_OK BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

