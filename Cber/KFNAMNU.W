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
DEFINE INPUT-OUTPUT PARAMETER kongrkod AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
{BBENAMNTEMP.I}
{UTBYTESLNAMNTEMP.I}
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE nykonst AS LOGICAL NO-UNDO.
DEFINE VARIABLE fnamnapph AS HANDLE NO-UNDO.
DEFINE VARIABLE utbytlg AS LOGICAL NO-UNDO.
DEFINE VARIABLE ers_uid AS INTEGER NO-UNDO INIT 0.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-3

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-BEN-2 TOG_KON TOG_KONVAL ~
TOG_KONMATRL TOG_P3 TOG_KONANM BTN_OK BTN_AVBRYT FILL-IN-BEN 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-BEN-2 TOG_KON TOG_KONVAL ~
TOG_KONMATRL TOG_P3 TOG_KONANM FILL-IN-BEN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE cmb_utbyteslkund AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Utbyteslista" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 30.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(30)":U 
     LABEL "Konstruktionsgrupp" 
      VIEW-AS TEXT 
     SIZE 30.38 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN-2 AS CHARACTER FORMAT "X(30)":U 
     LABEL "Konstruktionsgrupp" 
     VIEW-AS FILL-IN 
     SIZE 30.38 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_KON AS LOGICAL INITIAL no 
     LABEL "Kopiera konstruktioner" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_KONANM AS LOGICAL INITIAL no 
     LABEL "Kopiera fasta anmärkningar" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_KONMATRL AS LOGICAL INITIAL no 
     LABEL "Kopiera materiel" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_KONVAL AS LOGICAL INITIAL no 
     LABEL "Kopiera konstruktionsval" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_P3 AS LOGICAL INITIAL no 
     LABEL "Kopiera P3-koder" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .79 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     FILL-IN-BEN-2 AT ROW 2.92 COL 21 COLON-ALIGNED
     cmb_utbyteslkund AT ROW 4.25 COL 21 COLON-ALIGNED WIDGET-ID 4
     TOG_KON AT ROW 5.79 COL 22.5
     TOG_KONVAL AT ROW 6.88 COL 22.5
     TOG_KONMATRL AT ROW 7.96 COL 22.5
     TOG_P3 AT ROW 9.04 COL 22.5
     TOG_KONANM AT ROW 10 COL 22.5 WIDGET-ID 2
     BTN_OK AT ROW 11.42 COL 36.5
     BTN_AVBRYT AT ROW 11.42 COL 51.5
     FILL-IN-BEN AT ROW 1.75 COL 21 COLON-ALIGNED
     SPACE(13.36) SKIP(10.91)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX cmb_utbyteslkund IN FRAME DIALOG-3
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cmb_utbyteslkund:HIDDEN IN FRAME DIALOG-3           = TRUE.

ASSIGN 
       FILL-IN-BEN:READ-ONLY IN FRAME DIALOG-3        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3
DO:
   IF VALID-HANDLE(fnamnapph) THEN DELETE PROCEDURE fnamnapph.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT DIALOG-3
ON CHOOSE OF BTN_AVBRYT IN FRAME DIALOG-3 /* Avbryt */
DO: 
   musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* Ok */
DO:
   {muswait.i}.
   ASSIGN
   TOG_KON = INPUT TOG_KON
   TOG_KONVAL = INPUT TOG_KONVAL
   TOG_KONMATRL = INPUT TOG_KONMATRL
   TOG_P3 = INPUT TOG_P3
   TOG_KONANM = INPUT TOG_KONANM
   FILL-IN-BEN-2 = INPUT FILL-IN-BEN-2.
   IF TOG_KON = FALSE OR TOG_KONVAL = FALSE THEN TOG_KONMATRL = FALSE.
   IF TOG_KON = FALSE OR TOG_KONVAL = FALSE THEN TOG_P3 = FALSE.
   IF FILL-IN-BEN-2 = "" THEN DO:
      MESSAGE "Konstruktionsgruppen får inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN-2 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
    
   ers_uid = INPUT cmb_utbyteslkund.
   IF ers_uid = ? THEN ers_uid = 0.
   
   RUN btnkop_UI IN fnamnapph (INPUT-OUTPUT kongrkod,
                               INPUT FILL-IN-BEN-2,
                               INPUT TOG_KON,
                               INPUT TOG_KONVAL,
                               INPUT TOG_KONMATRL,
                               INPUT TOG_P3,
                               INPUT TOG_KONANM,   
                               INPUT ers_uid,    
                               OUTPUT TABLE konstgrptemp APPEND,
                               OUTPUT TABLE bbenamntemp APPEND,
                               OUTPUT TABLE konsttemp APPEND).
   IF VALID-HANDLE(fnamnapph) THEN DELETE PROCEDURE fnamnapph.
   
   APPLY "GO" TO FRAME {&FRAME-NAME}.             
   {musarrow.i}.       
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
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   
   utbytlg = TRUE.
   CMB_utbyteslkund:LIST-ITEM-PAIRS = ?.
   
   IF utbytlg = TRUE AND kongrkod NE 0 THEN 
   DO:
      ENABLE cmb_utbyteslkund with FRAME {&FRAME-NAME}.
      cmb_utbyteslkund:LABEL = Guru.Konstanter:gutbytk.
               
      FOR EACH utbyteslnamntemp. 
         status-ok = CMB_utbyteslkund:ADD-LAST(utbyteslnamntemp.UNAMN,utbyteslnamntemp.UID).               
      END.
      DISPLAY cmb_utbyteslkund with FRAME {&FRAME-NAME}.
   END.
   ELSE cmb_utbyteslkund:HIDDEN = TRUE.
   ASSIGN FRAME {&FRAME-NAME}:TITLE = "Kopiering av konstruktionsgrupp".
   FIND FIRST konstgrptemp WHERE konstgrptemp.KONSKOD = kongrkod NO-LOCK NO-ERROR.
   FILL-IN-BEN = konstgrptemp.BENAMNING.
   cmb_utbyteslkund = konstgrptemp.UID.
   IF utbytlg = TRUE AND kongrkod NE 0 THEN
      DISPLAY cmb_utbyteslkund WITH FRAME {&FRAME-NAME}.
   
   RUN enable_UI.       
   {FRMSIZED.I}
   {musarrow.i}.
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/ 
   IF Guru.Konstanter:appcon THEN DO:
      RUN FNAMNAPP.P PERSISTENT SET fnamnapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN FNAMNAPP.P PERSISTENT SET fnamnapph.
   END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY FILL-IN-BEN-2 TOG_KON TOG_KONVAL TOG_KONMATRL TOG_P3 TOG_KONANM 
          FILL-IN-BEN 
      WITH FRAME DIALOG-3.
  ENABLE FILL-IN-BEN-2 TOG_KON TOG_KONVAL TOG_KONMATRL TOG_P3 TOG_KONANM BTN_OK 
         BTN_AVBRYT FILL-IN-BEN 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

