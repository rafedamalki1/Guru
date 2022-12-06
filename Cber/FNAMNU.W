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

{EDIGRUPP.I}
{SOKDEF.I}
&Scoped-define SHARED SHARED
{MTRLTEMP.I}
{KONSTRMTRL.I}
{BBENAMNTEMP.I}
{UTBYTESLNAMNTEMP.I}
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE nykonst AS LOGICAL NO-UNDO.
DEFINE VARIABLE var2 AS CHARACTER NO-UNDO.  
DEFINE VARIABLE var3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE var4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE var5 AS CHARACTER NO-UNDO.
DEFINE VARIABLE var6 AS CHARACTER NO-UNDO.
DEFINE VARIABLE namn AS CHARACTER NO-UNDO.
DEFINE VARIABLE nynamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE kod AS INTEGER NO-UNDO.   
DEFINE VARIABLE ord AS INTEGER NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS FILL-IN-BEN FILL-IN-B1 FILL-IN-ID1 ~
FILL-IN-B2 FILL-IN-ID2 FILL-IN-B3 FILL-IN-B4 FILL-IN-B5 FILL-IN-B6 BTN_OK ~
BTN_AVBRYT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-BEN FILL-IN-B1 FILL-IN-ID1 ~
FILL-IN-B2 FILL-IN-ID2 FILL-IN-B3 FILL-IN-B4 FILL-IN-B5 CMB_EDI FILL-IN-B6 

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

DEFINE VARIABLE CMB_EDI AS CHARACTER FORMAT "X(10)":U 
     LABEL "Edi-grupp" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE cmb_utbyteslkund AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Utbyteslista" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 30.5 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-B1 AS CHARACTER FORMAT "X(20)" 
     LABEL "Fält 1" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE FILL-IN-B2 AS CHARACTER FORMAT "X(20)" 
     LABEL "Fält 2" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE FILL-IN-B3 AS CHARACTER FORMAT "X(20)" 
     LABEL "Fält 3" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE FILL-IN-B4 AS CHARACTER FORMAT "X(20)" 
     LABEL "Fält 4" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE FILL-IN-B5 AS CHARACTER FORMAT "X(20)" 
     LABEL "Fält 5" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE FILL-IN-B6 AS CHARACTER FORMAT "X(20)" 
     LABEL "Fält 6" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(30)":U 
     LABEL "Konstruktionsgrupp" 
     VIEW-AS FILL-IN 
     SIZE 30.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ID1 AS CHARACTER FORMAT "X(20)" 
     LABEL "Fält 1" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE FILL-IN-ID2 AS CHARACTER FORMAT "X(20)" 
     LABEL "Fält 2" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     FILL-IN-BEN AT ROW 1.42 COL 21 COLON-ALIGNED
     cmb_utbyteslkund AT ROW 2.63 COL 21 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-B1 AT ROW 6.75 COL 9.25 COLON-ALIGNED
     FILL-IN-ID1 AT ROW 6.75 COL 42.75 COLON-ALIGNED
     FILL-IN-B2 AT ROW 8.25 COL 9.25 COLON-ALIGNED
     FILL-IN-ID2 AT ROW 8.25 COL 42.75 COLON-ALIGNED
     FILL-IN-B3 AT ROW 9.75 COL 9.25 COLON-ALIGNED
     FILL-IN-B4 AT ROW 11.25 COL 9.25 COLON-ALIGNED
     FILL-IN-B5 AT ROW 12.75 COL 9.25 COLON-ALIGNED
     CMB_EDI AT ROW 14.17 COL 45.88 COLON-ALIGNED
     FILL-IN-B6 AT ROW 14.25 COL 9.25 COLON-ALIGNED
     BTN_OK AT ROW 16 COL 37
     BTN_AVBRYT AT ROW 16 COL 52
     "Materiel" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 5.25 COL 11.25
          BGCOLOR 7 
     "Identifiering" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 5.25 COL 44.63
          BGCOLOR 7 
     "Fältnamn för konstruktionsgruppen" VIEW-AS TEXT
          SIZE 37 BY 1.13 AT ROW 3.92 COL 16.5
          FONT 17
     SPACE(13.74) SKIP(12.32)
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

/* SETTINGS FOR COMBO-BOX CMB_EDI IN FRAME DIALOG-3
   NO-ENABLE                                                            */
ASSIGN 
       CMB_EDI:HIDDEN IN FRAME DIALOG-3           = TRUE.

/* SETTINGS FOR COMBO-BOX cmb_utbyteslkund IN FRAME DIALOG-3
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cmb_utbyteslkund:HIDDEN IN FRAME DIALOG-3           = TRUE.

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
   EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
   IF nykonst = FALSE THEN DO: 
      IF Guru.Konstanter:varforetypval[32] = 3 OR Guru.Konstanter:varforetypval[32] = 5 THEN DO:
         IF INPUT FILL-IN-ID2 = "" THEN DO:
            MESSAGE "Idenifiering får inte vara blankt." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-ID2 IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF INPUT FILL-IN-BEN = "" THEN DO:
         MESSAGE "Konstruktionsgruppen får inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      END.   
      ELSE IF INPUT FILL-IN-B1 = "" THEN DO:
         MESSAGE "Fält 1 får inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-B1 IN FRAME {&FRAME-NAME}.
      END.    
      ELSE IF INPUT FILL-IN-B2 = "" THEN DO:
         MESSAGE "Fält 2 får inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-B2 IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO: 
         CREATE valsoktemp.
         ASSIGN
         valsoktemp.SOKINT[1] = 1
         valsoktemp.SOKCHAR[2] = var2
         valsoktemp.SOKCHAR[3] = var3
         valsoktemp.SOKCHAR[4] = var4
         valsoktemp.SOKCHAR[5] = var5
         valsoktemp.SOKCHAR[6] = var6.
         CREATE valsoktemp.
         ASSIGN 
         valsoktemp.SOKINT[1] = 2
         valsoktemp.SOKCHAR[1] = INPUT FILL-IN-B1     
         valsoktemp.SOKCHAR[2] = INPUT FILL-IN-B2     
         valsoktemp.SOKCHAR[3] = INPUT FILL-IN-B3     
         valsoktemp.SOKCHAR[4] = INPUT FILL-IN-B4     
         valsoktemp.SOKCHAR[5] = INPUT FILL-IN-B5     
         valsoktemp.SOKCHAR[6] = INPUT FILL-IN-B6     
         valsoktemp.SOKCHAR[7] = INPUT FILL-IN-ID1   
         valsoktemp.SOKCHAR[8] = INPUT FILL-IN-ID2
         valsoktemp.SOKCHAR[9] = INPUT FILL-IN-BEN. 
         
         ers_uid = INPUT cmb_utbyteslkund.
         IF ers_uid = ? THEN
            ers_uid = 0.
                  
         RUN btnok_UI IN fnamnapph (INPUT-OUTPUT kongrkod,INPUT nykonst,
                           INPUT CMB_EDI,INPUT ers_uid, INPUT TABLE valsoktemp,
                           INPUT-OUTPUT TABLE konstgrptemp,
                           INPUT-OUTPUT TABLE  bbenamntemp). 
         IF VALID-HANDLE(fnamnapph) THEN DELETE PROCEDURE fnamnapph.
         APPLY "GO" TO FRAME {&FRAME-NAME}.          
      END.   
   END.
   ELSE DO:
      IF Guru.Konstanter:varforetypval[32] = 1 THEN DO:
         IF INPUT FILL-IN-ID2 = "" THEN DO:
            MESSAGE "Idenifiering får inte vara blankt." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-ID2 IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF INPUT FILL-IN-BEN = "" THEN DO:
         MESSAGE "Konstruktionsgruppen får inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      END.   
      ELSE IF INPUT FILL-IN-B1 = "" THEN DO:
         MESSAGE "Fält 1 får inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-B1 IN FRAME {&FRAME-NAME}.
      END.    
      ELSE IF INPUT FILL-IN-B2 = "" THEN DO:
         MESSAGE "Fält 2 får inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-B2 IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:    
         CREATE valsoktemp.
         ASSIGN 
         valsoktemp.SOKINT[1] = 2
         valsoktemp.SOKCHAR[1] = INPUT FILL-IN-B1     
         valsoktemp.SOKCHAR[2] = INPUT FILL-IN-B2     
         valsoktemp.SOKCHAR[3] = INPUT FILL-IN-B3     
         valsoktemp.SOKCHAR[4] = INPUT FILL-IN-B4     
         valsoktemp.SOKCHAR[5] = INPUT FILL-IN-B5     
         valsoktemp.SOKCHAR[6] = INPUT FILL-IN-B6     
         valsoktemp.SOKCHAR[7] = INPUT FILL-IN-ID1   
         valsoktemp.SOKCHAR[8] = INPUT FILL-IN-ID2
         valsoktemp.SOKCHAR[9] = INPUT FILL-IN-BEN.   
         
         ers_uid = INPUT cmb_utbyteslkund.
         IF ers_uid = ? THEN
            ers_uid = 0.
         RUN btnok_UI IN fnamnapph (INPUT-OUTPUT kongrkod,INPUT nykonst,
                           INPUT CMB_EDI, INPUT ers_uid, INPUT TABLE valsoktemp,
                           INPUT-OUTPUT TABLE konstgrptemp,
                           INPUT-OUTPUT TABLE bbenamntemp). 
                     
         IF VALID-HANDLE(fnamnapph) THEN DELETE PROCEDURE fnamnapph.
         APPLY "GO" TO FRAME {&FRAME-NAME}.     
      END.
   END.  
   {musarrow.i}.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_EDI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_EDI DIALOG-3
ON VALUE-CHANGED OF CMB_EDI IN FRAME DIALOG-3 /* Edi-grupp */
DO:                                 
   CMB_EDI  = INPUT CMB_EDI.   
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
   RUN laddaedi_UI IN fnamnapph (OUTPUT TABLE edigrupptemp).
   DO TRANSACTION:     
      IF nykonst  = FALSE THEN DO:
         ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändring av konstruktionsgrupp".
         FIND FIRST konstgrptemp WHERE konstgrptemp.KONSKOD = kongrkod NO-LOCK NO-ERROR.
         FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = konstgrptemp.KONSKOD EXCLUSIVE-LOCK NO-ERROR.
         ASSIGN
         FILL-IN-BEN = konstgrptemp.BENAMNING
         FILL-IN-B1 = bbenamntemp.B1   
         FILL-IN-B2 = bbenamntemp.B2
         FILL-IN-B3 = bbenamntemp.B3
         FILL-IN-B4 = bbenamntemp.B4
         FILL-IN-B5 = bbenamntemp.B5
         FILL-IN-B6 = bbenamntemp.B6
         FILL-IN-ID1 = bbenamntemp.ID1
         FILL-IN-ID2 = bbenamntemp.ID2
         var2 = bbenamntemp.B2
         var3 = bbenamntemp.B3
         var4 = bbenamntemp.B4
         var5 = bbenamntemp.B5
         var6 = bbenamntemp.B6.
         cmb_utbyteslkund = konstgrptemp.UID.
         IF utbytlg = TRUE AND kongrkod NE 0 THEN
            DISPLAY cmb_utbyteslkund WITH FRAME {&FRAME-NAME}.
         
      END.    
      ELSE DO:      
        ASSIGN FRAME {&FRAME-NAME}:TITLE = "Uppläggning av konstruktionsgrupp".
      END.   
      ASSIGN         
      status-ok = CMB_EDI:DELETE("0").
      FOR EACH edigrupptemp WHERE edigrupptemp.KONSKOD = 99999 NO-LOCK:
         ASSIGN      
         status-ok = CMB_EDI:ADD-LAST(edigrupptemp.BENAMNING)IN FRAME {&FRAME-NAME}
         CMB_EDI:SCREEN-VALUE = edigrupptemp.BENAMNING.
      END.
      CMB_EDI = INPUT CMB_EDI. 
      IF nykonst = FALSE THEN DO:
         FIND FIRST edigrupptemp WHERE edigrupptemp.KONSKOD = konstgrptemp.KONSKOD 
         NO-LOCK NO-ERROR.
         IF AVAILABLE edigrupptemp THEN DO:
            CMB_EDI:SCREEN-VALUE = edigrupptemp.BENAMNING.
            CMB_EDI = INPUT CMB_EDI.
         END.   
      END.      
   END.    
   RUN enable_UI.       
   {FRMSIZED.I}
   IF Guru.Konstanter:globniv = 0 THEN ENABLE CMB_EDI WITH FRAME {&FRAME-NAME}.
   ELSE CMB_EDI:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
   
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
  DISPLAY FILL-IN-BEN FILL-IN-B1 FILL-IN-ID1 FILL-IN-B2 FILL-IN-ID2 FILL-IN-B3 
          FILL-IN-B4 FILL-IN-B5 CMB_EDI FILL-IN-B6 
      WITH FRAME DIALOG-3.
  ENABLE FILL-IN-BEN FILL-IN-B1 FILL-IN-ID1 FILL-IN-B2 FILL-IN-ID2 FILL-IN-B3 
         FILL-IN-B4 FILL-IN-B5 FILL-IN-B6 BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

