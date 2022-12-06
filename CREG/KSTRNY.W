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

  Created: 95/05/16 -  7:28 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

&Scoped-define NEW 
DEFINE {&NEW} SHARED VARIABLE SEL_K1 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "BLANK" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K2 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "BLANK" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K3 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "BLANK" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K4 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "BLANK" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE  {&NEW} SHARED VARIABLE SEL_K5 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "BLANK" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE org AS CHARACTER NO-UNDO.
DEFINE VARIABLE brec AS RECID NO-UNDO.

DEFINE VARIABLE vadgora AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE kstrang
   FIELD BENAMNING AS CHARACTER
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER
   FIELD K5 AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD RADNR AS INTEGER
   FIELD RECTIDVIS AS RECID
   INDEX KONTO K1 K2 K3 K4 K5
   INDEX KONTO2 RADNR.

DEFINE INPUT PARAMETER radnrvar AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER ktorec AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR kstrang.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_BENAMNING BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS sk1 sk2 sk3 sk4 sk5 FILL-IN_BENAMNING 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_BENAMNING AS CHARACTER FORMAT "X(25)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 50.5 BY 1.

DEFINE VARIABLE sk1 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE VARIABLE sk2 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE VARIABLE sk3 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE VARIABLE sk4 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE VARIABLE sk5 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     sk1 AT ROW 2.63 COL 3.5 COLON-ALIGNED NO-LABEL
     sk2 AT ROW 2.63 COL 11 COLON-ALIGNED NO-LABEL
     sk3 AT ROW 2.63 COL 18.5 COLON-ALIGNED NO-LABEL
     sk4 AT ROW 2.63 COL 25.5 COLON-ALIGNED NO-LABEL
     sk5 AT ROW 2.63 COL 32.5 COLON-ALIGNED NO-LABEL
     FILL-IN_BENAMNING AT ROW 4.25 COL 10 COLON-ALIGNED
     BTN_OK AT ROW 5.75 COL 33.25
     BTN_AVB AT ROW 5.75 COL 48.25
     "Kontosträng" VIEW-AS TEXT
          SIZE 26 BY 1.5 AT ROW 1 COL 1.5
          FONT 17
     SPACE(35.49) SKIP(4.57)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kontosträng":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN sk1 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sk2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sk3 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sk4 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sk5 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Kontosträng */
DO:
   {muswait.i}
   FIND FIRST kstrang WHERE RECID(kstrang) = brec EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE kstrang THEN DELETE kstrang.
   {BORTBRWPROC.I}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Kontosträng */
DO:
   APPLY "END-ERROR" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   {muswait.i}
   IF INPUT FILL-IN_BENAMNING = "" THEN DO:
      MESSAGE "Benämningen kan inte vara blank." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ELSE DO TRANSACTION:   
      FIND FIRST kstrang WHERE RECID(kstrang) = brec EXCLUSIVE-LOCK NO-ERROR.
      FILL-IN_BENAMNING = INPUT FILL-IN_BENAMNING. 
      ASSIGN kstrang.OMRADE = org      
      kstrang.BENAMNING = FILL-IN_BENAMNING. 
      IF SEL_K1 = " BLANK" THEN ASSIGN kstrang.K1 = "" .
      ELSE IF SEL_K1 = ? THEN ASSIGN kstrang.K1 = "" .
      ELSE ASSIGN kstrang.K1 = SEL_K1.
      IF SEL_K2 = " BLANK" THEN ASSIGN kstrang.K2 = "" .
      ELSE IF SEL_K2 = ? THEN ASSIGN kstrang.K2 = "" .
      ELSE ASSIGN kstrang.K2 = SEL_K2.
      IF SEL_K3 = " BLANK" THEN ASSIGN kstrang.K3 = "" .
      ELSE IF SEL_K3 = ? THEN ASSIGN kstrang.K3 = "" .
      ELSE ASSIGN kstrang.K3 = SEL_K3.
      IF SEL_K4 = " BLANK" THEN ASSIGN kstrang.K4 = "" .
      ELSE IF SEL_K4 = ? THEN ASSIGN kstrang.K4 = "" .   
      ELSE ASSIGN kstrang.K4 = SEL_K4.
      IF SEL_K5 = " BLANK" THEN ASSIGN kstrang.K5 = "" .
      ELSE IF SEL_K5 = ? THEN ASSIGN kstrang.K5 = "" .
      ELSE ASSIGN kstrang.K5 = SEL_K5.     
      ktorec = kstrang.RECTIDVIS.
      vadgora = 1.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN KSTUPPD.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT vadgora,INPUT-OUTPUT ktorec,INPUT-OUTPUT TABLE kstrang).                        
      END.
      ELSE DO:
         RUN KSTUPPD.P 
         (INPUT vadgora,INPUT-OUTPUT ktorec,INPUT-OUTPUT TABLE kstrang).      
      END.     
      FIND FIRST kstrang WHERE kstrang.RECTIDVIS = ktorec NO-ERROR.
      brec = RECID(kstrang).
   END.
   {BORTBRWPROC.I} 
   APPLY "GO" TO FRAME {&FRAME-NAME}.
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
   IF SEL_K1 = " BLANK" THEN sk1 = "" .
   ELSE IF SEL_K1 = ? THEN sk1 = "" .
   ELSE sk1 = SEL_K1.
   IF SEL_K2 = " BLANK" THEN sk2 = "" .
   ELSE IF SEL_K2 = ? THEN sk2 = "" .
   ELSE sk2 = SEL_K2.
   IF SEL_K3 = " BLANK" THEN sk3 = "" .  
   ELSE IF SEL_K3 = ? THEN sk3 = "" .
   ELSE sk3 = SEL_K3.
   IF SEL_K4 = " BLANK" THEN sk4 = "" .
   ELSE IF SEL_K4 = ? THEN sk4 = "" .   
   ELSE sk4 = SEL_K4.
   IF SEL_K5 = " BLANK" THEN sk5 = "" .
   ELSE IF SEL_K5 = ? THEN sk5 = "" .
   ELSE sk5 = SEL_K5. 
   /*FIND LAST kstrang USE-INDEX KONTO2 NO-LOCK NO-ERROR.
   IF AVAILABLE kstrang THEN radnrvar = kstrang.RADNR + 1.
   ELSE radnrvar = 1.*/
   CREATE kstrang. 
   ASSIGN kstrang.RADNR = radnrvar FILL-IN_BENAMNING = ""
   brec = RECID(kstrang)
   kstrang.RECTIDVIS = ?.
   RUN enable_UI.       
   {FRMSIZED.I}
   APPLY "ENTRY" TO FILL-IN_BENAMNING IN FRAME {&FRAME-NAME}.
   {musarrow.i}
   /*FIND KONTOSTRANG WHERE RECID(KONTOSTRANG) = brec EXCLUSIVE-LOCK NO-ERROR.*/
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
  DISPLAY sk1 sk2 sk3 sk4 sk5 FILL-IN_BENAMNING 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN_BENAMNING BTN_OK BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

