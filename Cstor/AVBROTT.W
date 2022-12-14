&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE NEW SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE valrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.   
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE startdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE starttid AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE lspantal AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE hspantal AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE bortrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE VARIABLE totlsp AS INTEGER NO-UNDO.
DEFINE VARIABLE tothsp AS INTEGER NO-UNDO.

DEFINE SHARED TEMP-TABLE bort_temp
   FIELD ATERDATUM AS DATE
   FIELD ATERKLOCKAN AS DECIMAL
   FIELD ANTALHSP AS INTEGER
   FIELD ANTALLSP AS INTEGER
   FIELD AVBROTTSTID AS DECIMAL
   FIELD AKTIVITET AS CHARACTER
   INDEX AVBROTT ATERDATUM ATERKLOCKAN ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_UPDAT1 FILL-IN-STARTDAT BTN_MINDAT1 ~
FILL-IN-START FILL-IN-HSPKUND FILL-IN-LSPKUND FILL-IN-AKTIVITET BTN_OK ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-STARTDAT FILL-IN-START ~
FILL-IN-HSPKUND FILL-IN-LSPKUND FILL-IN-AKTIVITET 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_MINDAT1 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_UPDAT1 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE FILL-IN-AKTIVITET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aktivitet" 
     VIEW-AS FILL-IN 
     SIZE 33.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-HSPKUND AS INTEGER FORMAT ">>>>>":U INITIAL 0 
     LABEL "HSP-kunder" 
     VIEW-AS FILL-IN 
     SIZE 7.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LSPKUND AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     LABEL "LSP-kunder" 
     VIEW-AS FILL-IN 
     SIZE 7.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-START AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Klockan" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BTN_UPDAT1 AT ROW 1.75 COL 25.13
     FILL-IN-STARTDAT AT ROW 2.04 COL 12.13 COLON-ALIGNED
     BTN_MINDAT1 AT ROW 2.88 COL 25.13
     FILL-IN-START AT ROW 4.04 COL 12.13 COLON-ALIGNED
     FILL-IN-HSPKUND AT ROW 6.04 COL 12.13 COLON-ALIGNED
     FILL-IN-LSPKUND AT ROW 6.04 COL 32.88 COLON-ALIGNED
     FILL-IN-AKTIVITET AT ROW 8.04 COL 12.13 COLON-ALIGNED
     BTN_OK AT ROW 9.33 COL 18.25
     BTN_AVB AT ROW 9.42 COL 33.25
     SPACE(0.74) SKIP(0.15)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         DEFAULT-BUTTON BTN_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN-START:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR,ENDKEY OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
   {BORTBRWPROC.I}
   musz = TRUE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MINDAT1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MINDAT1 Dialog-Frame
ON CHOOSE OF BTN_MINDAT1 IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   FILL-IN-START = INPUT FILL-IN-START
   FILL-IN-HSPKUND = INPUT FILL-IN-HSPKUND
   FILL-IN-LSPKUND = INPUT FILL-IN-LSPKUND
   FILL-IN-AKTIVITET = INPUT FILL-IN-AKTIVITET.
   IF FILL-IN-STARTDAT >= startdatum THEN DO:
      IF FILL-IN-STARTDAT = startdatum THEN DO:
         IF FILL-IN-START < starttid THEN DO:
            MESSAGE "Tidpunkt m?ste vara st?rre ?n starttidpunkten."
            VIEW-AS ALERT-BOX TITLE "Meddelande".
            APPLY "ENTRY" TO FILL-IN-START IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.        
      END.   
   END.
   ELSE DO:
      MESSAGE "Datum m?ste vara st?rre ?n eller lika med startdatum " + STRING(startdatum) + "."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-STARTDAT IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.      
   END.
   IF andra = FALSE THEN DO:
      DO TRANSACTION:
         CREATE bort_temp.
         ASSIGN
         bort_temp.ATERDATUM = FILL-IN-STARTDAT
         bort_temp.ATERKLOCKAN = FILL-IN-START
         bort_temp.ANTALHSP = FILL-IN-HSPKUND
         bort_temp.ANTALLSP = FILL-IN-LSPKUND
         bort_temp.AKTIVITET = FILL-IN-AKTIVITET
         bortrec = RECID(bort_temp).
         VALIDATE bort_temp.
      END.   
   END.   
   ELSE DO:
      DO TRANSACTION:
         FIND FIRST bort_temp WHERE RECID(bort_temp) = bortrec 
         EXCLUSIVE-LOCK NO-ERROR.
         ASSIGN
         bort_temp.ATERDATUM = FILL-IN-STARTDAT
         bort_temp.ATERKLOCKAN = FILL-IN-START
         bort_temp.ANTALHSP = FILL-IN-HSPKUND
         bort_temp.ANTALLSP = FILL-IN-LSPKUND
         bort_temp.AKTIVITET = FILL-IN-AKTIVITET.
         VALIDATE bort_temp.
      END.
   END.
   FIND FIRST bort_temp WHERE RECID(bort_temp) NE bortrec NO-LOCK NO-ERROR.
   IF AVAILABLE bort_temp THEN DO:
      IF FILL-IN-HSPKUND > hspantal THEN DO:
         hspantal = FILL-IN-HSPKUND.
      END.
      IF FILL-IN-LSPKUND > lspantal THEN DO:
         lspantal = FILL-IN-LSPKUND.
      END.
   END.
   ELSE DO:
      ASSIGN
      hspantal = FILL-IN-HSPKUND
      lspantal = FILL-IN-LSPKUND.
   END.   
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPDAT1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPDAT1 Dialog-Frame
ON CHOOSE OF BTN_UPDAT1 IN FRAME Dialog-Frame /* + */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-START Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN-START IN FRAME Dialog-Frame /* Klockan */
DO:
   klocka = INPUT FILL-IN-START.
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   FILL-IN-START = klocka.
   DISPLAY FILL-IN-START WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT Dialog-Frame
ON LEAVE OF FILL-IN-STARTDAT IN FRAME Dialog-Frame /* Datum */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME Dialog-Frame /* Datum */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w. 
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.  
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
   IF andra = FALSE THEN DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny aktivitet".
      FIND LAST bort_temp WHERE 
      USE-INDEX AVBROTT NO-LOCK NO-ERROR.
      IF AVAILABLE bort_temp THEN DO:
         ASSIGN         
         FILL-IN-STARTDAT = bort_temp.ATERDATUM
         FILL-IN-START = bort_temp.ATERKLOCKAN + 1.
      END.
      ELSE DO:
         ASSIGN
         FILL-IN-STARTDAT = startdatum
         FILL-IN-START = starttid + 1
         FILL-IN-HSPKUND = hspantal
         FILL-IN-LSPKUND = lspantal.
      END.
   END.
   ELSE DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "?ndring av aktivitet".
      FIND FIRST bort_temp WHERE RECID(bort_temp) = bortrec NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN-STARTDAT = bort_temp.ATERDATUM
      FILL-IN-START = bort_temp.ATERKLOCKAN
      FILL-IN-HSPKUND = bort_temp.ANTALHSP
      FILL-IN-LSPKUND = bort_temp.ANTALLSP
      FILL-IN-AKTIVITET = bort_temp.AKTIVITET.
   END.
  RUN enable_UI.       
   {FRMSIZED.I}
   {DIA_M_SLUT.I}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY FILL-IN-STARTDAT FILL-IN-START FILL-IN-HSPKUND FILL-IN-LSPKUND 
          FILL-IN-AKTIVITET 
      WITH FRAME Dialog-Frame.
  ENABLE BTN_UPDAT1 FILL-IN-STARTDAT BTN_MINDAT1 FILL-IN-START FILL-IN-HSPKUND 
         FILL-IN-LSPKUND FILL-IN-AKTIVITET BTN_OK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

