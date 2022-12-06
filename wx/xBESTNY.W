&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/04 -  1:19 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE frametemp NO-UNDO
   FIELD ORDNING AS INTEGER
   FIELD FRMHAND AS HANDLE
   FIELD FHOJD AS INTEGER
   FIELD FBREDD AS INTEGER
   FIELD MFHOJD AS INTEGER
   FIELD MFBREDD AS INTEGER
   FIELD FRAMENAMN AS CHARACTER
   INDEX ORDNING ORDNING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_VIBESTID FILL-IN_BESTNAMN BTN_OK ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_VIBESTID FILL-IN_BESTNAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 11 BY 1.17.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 11 BY 1.17.

DEFINE VARIABLE FILL-IN_BESTNAMN AS CHARACTER FORMAT "x(25)" 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE FILL-IN_VIBESTID AS CHARACTER FORMAT "X(10)" 
     LABEL "Best-id" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN-EPOST AS CHARACTER FORMAT "X(256)":U 
     LABEL "E-post" 
     VIEW-AS FILL-IN 
     SIZE 49.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_KONTAKT AS CHARACTER FORMAT "x(256)" 
     LABEL "Kontaktperson" 
     VIEW-AS FILL-IN 
     SIZE 49.63 BY 1.

DEFINE VARIABLE FILL-IN_TEL AS CHARACTER FORMAT "X(256)" 
     LABEL "Tel" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     FILL-IN_VIBESTID AT ROW 2.46 COL 24 COLON-ALIGNED
     FILL-IN_BESTNAMN AT ROW 3.75 COL 24 COLON-ALIGNED
     BTN_OK AT ROW 26.29 COL 37
     BTN_AVB AT ROW 26.29 COL 58.25
     SPACE(41.24) SKIP(0.78)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "labels not ok":L.

DEFINE FRAME FRAME-GRUND
     FILL-IN_KONTAKT AT ROW 1.5 COL 20.5 COLON-ALIGNED
     FILL-IN_TEL AT ROW 2.67 COL 20.5 COLON-ALIGNED
     FILL-IN-EPOST AT ROW 3.83 COL 20.5 COLON-ALIGNED
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 18.13 ROW 4.79
         SIZE 73 BY 17
         TITLE "lock at the labels".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME FRAME-GRUND:FRAME = FRAME DIALOG-3:HANDLE.

/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE                                                          */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-GRUND:MOVE-AFTER-TAB-ITEM (FILL-IN_BESTNAMN:HANDLE IN FRAME DIALOG-3)
       XXTABVALXX = FRAME FRAME-GRUND:MOVE-BEFORE-TAB-ITEM (BTN_OK:HANDLE IN FRAME DIALOG-3)
/* END-ASSIGN-TABS */.

ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-GRUND
                                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-GRUND
/* Query rebuild information for FRAME FRAME-GRUND
     _Query            is NOT OPENED
*/  /* FRAME FRAME-GRUND */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* labels not ok */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-3
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-3 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* Ok */
DO:
      APPLY "GO" TO FRAME {&FRAME-NAME}.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BESTNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BESTNAMN DIALOG-3
ON ENTRY OF FILL-IN_BESTNAMN IN FRAME DIALOG-3 /* Namn */
DO:
   RUN tabstop_UI (INPUT FALSE).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-3 


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
   DEFINE VARIABLE retvalkoll AS LOGICAL NO-UNDO.
  DEFINE VARIABLE varforetypval AS INTEGER NO-UNDO EXTENT 50.
  FRAME frame-grund:HIDDEN = FALSE. 
  varforetypval[20] = 6.
DEF VAR vh AS WIDGET-HANDLE NO-UNDO.
IF varforetypval[20] >= 1 THEN do:   
   vh = FRAME {&FRAME-NAME}:HANDLE.
   vh:FONT = varforetypval[20].
   vh = vh:FIRST-CHILD. /* field-group */
   vh = vh:FIRST-CHILD. /* first widget in field-group*/
   REPEAT:               
         /*  RUN  WIDGETDEF.P (INPUT vh).*/
      IF vh NE ? THEN vh=vh:NEXT-SIBLING. /* volgend widget */
      IF NOT VALID-HANDLE(vh) THEN LEAVE.
   END.
END.
   RUN enable_UI.
   
   EMPTY TEMP-TABLE frametemp NO-ERROR. 
   DEFINE VARIABLE frameivar AS INTEGER NO-UNDO.
  
   vh = FRAME {&FRAME-NAME}:HANDLE.
   CREATE frametemp.
   ASSIGN
   frametemp.ORDNING = 1
   frametemp.FRAMENAMN = vh:NAME
   frametemp.FRMHAND = vh.

   vh = vh:FIRST-CHILD. /* field-group */
   vh = vh:FIRST-CHILD. /* first widget in field-group*/
   frameivar = 2.
   ok-lus:
   REPEAT:     
      IF LOOKUP(vh:TYPE,"FRAME") > 0 THEN DO:
         CREATE frametemp.
         ASSIGN
         frametemp.ORDNING = frameivar
         frametemp.FRMHAND = vh
         frametemp.FRAMENAMN = vh:NAME.
         frameivar = frameivar + 1.        
      END.
      IF varforetypval[20] >= 1 THEN DO:
        /* RUN  WIDGETDEF.P (INPUT vh).*/
      END.
      vh=vh:NEXT-SIBLING. /* volgend widget */
      IF NOT VALID-HANDLE(vh) THEN LEAVE ok-lus.
   END.
   
   IF varforetypval[20] >= 1 THEN DO:
      
      FOR EACH frametemp:
         vh = frametemp.FRMHAND.
         vh = vh:FIRST-CHILD. /* field-group */
         vh = vh:FIRST-CHILD. /* first widget in field-group*/
         frametemp.FRMHAND:FONT = varforetypval[20].
         REPEAT:
            /*RUN  WIDGETDEF.P (INPUT vh).*/
            vh=vh:NEXT-SIBLING. /* volgend widget */
            IF NOT VALID-HANDLE(vh) THEN LEAVE.
         END.         
      END.
      
   END.


    FRAME frame-grund:HIDDEN = FALSE. 
   FRAME {&FRAME-NAME}:HIDDEN = FALSE. 
   
   
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-3  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-3.
  HIDE FRAME FRAME-GRUND.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN_VIBESTID FILL-IN_BESTNAMN 
      WITH FRAME DIALOG-3.
  ENABLE FILL-IN_VIBESTID FILL-IN_BESTNAMN BTN_OK BTN_AVB 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
  DISPLAY FILL-IN_KONTAKT FILL-IN_TEL FILL-IN-EPOST 
      WITH FRAME FRAME-GRUND.
  ENABLE FILL-IN_KONTAKT FILL-IN_TEL FILL-IN-EPOST 
      WITH FRAME FRAME-GRUND.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-GRUND}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

