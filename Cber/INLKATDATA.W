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

  Created: 95/05/18 -  9:13 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{LEVTEMP.I}
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO. 
DEFINE SHARED VARIABLE leverant AS CHARACTER NO-UNDO.
DEFINE VARIABLE namn AS CHARACTER NO-UNDO.
DEFINE VARIABLE namn2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE mtrladmapph AS HANDLE NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-UPPDN FILL-IN-INLDAT FILL-IN-TID ~
FILL-IN-STARTDAT FILL-IN-SLUTDAT btn_ok BTN_AVB BTN_NVE-3 BTN_FVE-3 ~
BTN_NVE-4 BTN_FVE-5 BTN_NVE-5 BTN_FVE-4 FILL-IN-PRISJMF 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-UPPDN FILL-IN-NAMN FILL-IN-NAMN-2 ~
FILL-IN-LEVKOD FILL-IN-LEVNAMN FILL-IN-INLDAT FILL-IN-TID FILL-IN-STARTDAT ~
FILL-IN-SLUTDAT FILL-IN-ANV FILL-IN-ANV-2 FILL-IN-PRISJMF 

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

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-4 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-5 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-4 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-5 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON btn_ok 
     LABEL "OK":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-ANV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Senaste uppdaterad av" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANV-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ersätt med användare" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-INLDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Inläst datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEVKOD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev-id" 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEVNAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Leverantör" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAMN AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAMN-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRISJMF AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Med vid pris jmf" 
     VIEW-AS FILL-IN 
     SIZE 5.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Gäller till datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Gäller från datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TID AS CHARACTER FORMAT "X(256)":U 
     LABEL "Inläst tid" 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UPPDN AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Uppdat beredningar i natt" 
     VIEW-AS FILL-IN 
     SIZE 5.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-UPPDN AT ROW 13.38 COL 26.5 COLON-ALIGNED
     FILL-IN-NAMN AT ROW 10.38 COL 38 COLON-ALIGNED NO-LABEL
     FILL-IN-NAMN-2 AT ROW 11.88 COL 38 COLON-ALIGNED NO-LABEL
     FILL-IN-LEVKOD AT ROW 1.25 COL 26.5 COLON-ALIGNED
     FILL-IN-LEVNAMN AT ROW 2.75 COL 26.5 COLON-ALIGNED
     FILL-IN-INLDAT AT ROW 4.25 COL 26.5 COLON-ALIGNED
     FILL-IN-TID AT ROW 5.75 COL 26.5 COLON-ALIGNED
     FILL-IN-STARTDAT AT ROW 7.25 COL 26.5 COLON-ALIGNED
     FILL-IN-SLUTDAT AT ROW 8.75 COL 26.5 COLON-ALIGNED
     FILL-IN-ANV AT ROW 10.38 COL 26.5 COLON-ALIGNED
     FILL-IN-ANV-2 AT ROW 11.88 COL 26.5 COLON-ALIGNED
     btn_ok AT ROW 17 COL 41.88
     BTN_AVB AT ROW 17 COL 56.38
     BTN_NVE-3 AT ROW 6.79 COL 39
     BTN_FVE-3 AT ROW 7.67 COL 39
     BTN_NVE-4 AT ROW 8.67 COL 39
     BTN_FVE-5 AT ROW 4.88 COL 38.88
     BTN_NVE-5 AT ROW 4 COL 38.88
     BTN_FVE-4 AT ROW 9.54 COL 39
     FILL-IN-PRISJMF AT ROW 14.88 COL 26.5 COLON-ALIGNED WIDGET-ID 2
     SPACE(37.12) SKIP(2.74)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Katalog inläst datum":L
         DEFAULT-BUTTON btn_ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ANV IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-ANV-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-LEVKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-LEVNAMN IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-NAMN IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-NAMN-2 IN FRAME DIALOG-1
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

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 DIALOG-1
ON CHOOSE OF BTN_FVE-3 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1. 
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-4 DIALOG-1
ON CHOOSE OF BTN_FVE-4 IN FRAME DIALOG-1 /* - */
DO:   
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.   
   FILL-IN-SLUTDAT = FILL-IN-SLUTDAT - 1.      
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-5 DIALOG-1
ON CHOOSE OF BTN_FVE-5 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-INLDAT = INPUT FILL-IN-INLDAT.   
   FILL-IN-INLDAT = FILL-IN-INLDAT - 1. 
   DISPLAY FILL-IN-INLDAT WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-3 DIALOG-1
ON CHOOSE OF BTN_NVE-3 IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.        
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-4 DIALOG-1
ON CHOOSE OF BTN_NVE-4 IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.   
   FILL-IN-SLUTDAT = FILL-IN-SLUTDAT + 1.        
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-5 DIALOG-1
ON CHOOSE OF BTN_NVE-5 IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-INLDAT = INPUT FILL-IN-INLDAT.   
   FILL-IN-INLDAT = FILL-IN-INLDAT + 1.        
   DISPLAY FILL-IN-INLDAT WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* OK */
DO: 
   {muswait.i} 
   ASSIGN
   FILL-IN-INLDAT = INPUT FILL-IN-INLDAT
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT
   FILL-IN-ANV = INPUT FILL-IN-ANV
   FILL-IN-TID = INPUT FILL-IN-TID
   FILL-IN-UPPDN = INPUT FILL-IN-UPPDN
   FILL-IN-PRISJMF = INPUT FILL-IN-PRISJMF.

   EMPTY TEMP-TABLE extradatatemp  NO-ERROR. 
   CREATE extradatatemp.
   ASSIGN
   extradatatemp.PROGRAM = "INLKAT"                   
   extradatatemp.HUVUDCH = leverant  
   extradatatemp.SOKDATE[1] = FILL-IN-INLDAT
   extradatatemp.SOKDATE[2] = FILL-IN-STARTDAT
   extradatatemp.SOKDATE[3] = FILL-IN-SLUTDAT
   extradatatemp.SOKCHAR[1] = STRING(FILL-IN-TID)
   extradatatemp.SOKCHAR[2] =  FILL-IN-ANV-2
   extradatatemp.SOKLOG[1] =  FILL-IN-UPPDN
   extradatatemp.SOKLOG[2] = FILL-IN-PRISJMF.
   RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
   APPLY "GO" TO FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-INLDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-INLDAT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-INLDAT IN FRAME DIALOG-1 /* Inläst datum */
DO:
  ASSIGN
   FILL-IN-INLDAT = INPUT FILL-IN-INLDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-INLDAT.
   IF Guru.GlobalaVariabler:regdatum = ?  THEN Guru.GlobalaVariabler:regdatum = TODAY.
   RUN AlmanBtn.w.
   FILL-IN-INLDAT = TODAY. 
   FILL-IN-INLDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-INLDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PRISJMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PRISJMF DIALOG-1
ON LEAVE OF FILL-IN-PRISJMF IN FRAME DIALOG-1 /* Med vid pris jmf */
DO:
  FILL-IN-PRISJMF = INPUT FILL-IN-PRISJMF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PRISJMF DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-PRISJMF IN FRAME DIALOG-1 /* Med vid pris jmf */
DO:
  IF INPUT FILL-IN-PRISJMF = TRUE THEN FILL-IN-PRISJMF = FALSE.
   IF INPUT FILL-IN-PRISJMF = FALSE THEN FILL-IN-PRISJMF = TRUE.
   DISPLAY FILL-IN-PRISJMF WITH FRAME {&FRAME-NAME}. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT DIALOG-1
ON ENTRY OF FILL-IN-SLUTDAT IN FRAME DIALOG-1 /* Gäller till datum */
DO:
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT DIALOG-1
ON LEAVE OF FILL-IN-SLUTDAT IN FRAME DIALOG-1 /* Gäller till datum */
DO:
  FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-SLUTDAT IN FRAME DIALOG-1 /* Gäller till datum */
DO:
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-SLUTDAT.
   IF Guru.GlobalaVariabler:regdatum = ?  THEN Guru.GlobalaVariabler:regdatum = TODAY.
   RUN AlmanBtn.w.
   FILL-IN-SLUTDAT = TODAY.
   FILL-IN-SLUTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON ENTRY OF FILL-IN-STARTDAT IN FRAME DIALOG-1 /* Gäller från datum */
DO:
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON LEAVE OF FILL-IN-STARTDAT IN FRAME DIALOG-1 /* Gäller från datum */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME DIALOG-1 /* Gäller från datum */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   IF Guru.GlobalaVariabler:regdatum = ?  THEN Guru.GlobalaVariabler:regdatum = TODAY.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = TODAY.
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-UPPDN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-UPPDN DIALOG-1
ON LEAVE OF FILL-IN-UPPDN IN FRAME DIALOG-1 /* Uppdat beredningar i natt */
DO:
  FILL-IN-UPPDN = INPUT FILL-IN-UPPDN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-UPPDN DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-UPPDN IN FRAME DIALOG-1 /* Uppdat beredningar i natt */
DO:
  IF INPUT FILL-IN-UPPDN = TRUE THEN FILL-IN-UPPDN = FALSE.
   IF INPUT FILL-IN-UPPDN = FALSE THEN FILL-IN-UPPDN = TRUE.
   DISPLAY FILL-IN-UPPDN WITH FRAME {&FRAME-NAME}. 

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
   {ALLSTARTDYN.I}
   FIND FIRST levtemp WHERE levtemp.LEVKOD = leverant NO-LOCK NO-ERROR.                                                             
   ASSIGN FILL-IN-LEVKOD = leverant
   FILL-IN-LEVNAMN = levtemp.LEVNAMN
   FILL-IN-ANV-2 = Guru.Konstanter:globanv.
   RUN levinlast2_UI (INPUT leverant).
   RUN namnhmt_UI IN mtrladmapph (INPUT FILL-IN-ANV, INPUT FILL-IN-ANV-2,OUTPUT namn, OUTPUT namn2).         
   ASSIGN
   FILL-IN-NAMN = namn
   FILL-IN-NAMN-2 = namn2.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRKADMAPP.P PERSISTENT SET mtrladmapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRKADMAPP.P PERSISTENT SET mtrladmapph.
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY FILL-IN-UPPDN FILL-IN-NAMN FILL-IN-NAMN-2 FILL-IN-LEVKOD 
          FILL-IN-LEVNAMN FILL-IN-INLDAT FILL-IN-TID FILL-IN-STARTDAT 
          FILL-IN-SLUTDAT FILL-IN-ANV FILL-IN-ANV-2 FILL-IN-PRISJMF 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-UPPDN FILL-IN-INLDAT FILL-IN-TID FILL-IN-STARTDAT 
         FILL-IN-SLUTDAT btn_ok BTN_AVB BTN_NVE-3 BTN_FVE-3 BTN_NVE-4 BTN_FVE-5 
         BTN_NVE-5 BTN_FVE-4 FILL-IN-PRISJMF 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE levinlast2_UI DIALOG-1 
PROCEDURE levinlast2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER lekod AS CHARACTER NO-UNDO.   
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "INLKAT"                   
   inextradatatemp.HUVUDCH = lekod.                                              
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:
      ASSIGN
      FILL-IN-INLDAT = extradatatemp.SOKDATE[1]
      FILL-IN-STARTDAT = extradatatemp.SOKDATE[2]
      FILL-IN-SLUTDAT = extradatatemp.SOKDATE[3]
      FILL-IN-TID = extradatatemp.SOKCHAR[1]
      FILL-IN-ANV = extradatatemp.SOKCHAR[2]
      FILL-IN-UPPDN = extradatatemp.SOKLOG[1]
      FILL-IN-PRISJMF = extradatatemp.SOKLOG[2].
   END.   
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

