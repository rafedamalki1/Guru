&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-3


/* Temp-Table and Buffer definitions                                    */



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
DEFINE INPUT-OUTPUT PARAMETER vem AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
{LEVTEMP.I}
{GLOBVAR2DEL1.I}
{EXTRADATA.I}
DEFINE SHARED VARIABLE edataapph AS HANDLE NO-UNDO.
&Scoped-define NEW NEW
DEFINE VARIABLE hlev AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE levapph AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES levtemp

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define QUERY-STRING-DIALOG-3 FOR EACH levtemp NO-LOCK
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH levtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 levtemp
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 levtemp


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS levtemp.LEVNAMN levtemp.LEVKONTAKT ~
levtemp.LEVTEL levtemp.LEVADR levtemp.LEVPNR levtemp.LEVORT 
&Scoped-define ENABLED-TABLES levtemp
&Scoped-define FIRST-ENABLED-TABLE levtemp
&Scoped-Define ENABLED-OBJECTS FILL-IN-LEVSTAT FILL-IN-PRISJMF BTN_OK ~
BTN_AVBRYT 
&Scoped-Define DISPLAYED-FIELDS levtemp.LEVKOD levtemp.LEVNAMN ~
levtemp.LEVKONTAKT levtemp.LEVTEL levtemp.LEVADR levtemp.LEVPNR ~
levtemp.LEVORT 
&Scoped-define DISPLAYED-TABLES levtemp
&Scoped-define FIRST-DISPLAYED-TABLE levtemp
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-LEVSTAT FILL-IN-PRISJMF 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-LEVSTAT AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Huvudleverantör" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRISJMF AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Med vid pris jmf" 
     VIEW-AS FILL-IN 
     SIZE 5.5 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-3 FOR 
      levtemp SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     levtemp.LEVKOD AT ROW 1.5 COL 17.75 COLON-ALIGNED
          LABEL "Lev-id"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     levtemp.LEVNAMN AT ROW 3 COL 17.75 COLON-ALIGNED
          LABEL "Företagsnamn"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     FILL-IN-LEVSTAT AT ROW 4.5 COL 17.75 COLON-ALIGNED
     levtemp.LEVKONTAKT AT ROW 6 COL 17.75 COLON-ALIGNED
          LABEL "Kontaktperson"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     levtemp.LEVTEL AT ROW 7.5 COL 17.75 COLON-ALIGNED
          LABEL "Tel."
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     levtemp.LEVADR AT ROW 9 COL 17.88 COLON-ALIGNED
          LABEL "Adress"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     levtemp.LEVPNR AT ROW 10.5 COL 17.88 COLON-ALIGNED
          LABEL "Postnr"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     levtemp.LEVORT AT ROW 12 COL 17.88 COLON-ALIGNED
          LABEL "Ort"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     FILL-IN-PRISJMF AT ROW 13.42 COL 17.88 COLON-ALIGNED WIDGET-ID 2
     BTN_OK AT ROW 17.25 COL 17
     BTN_AVBRYT AT ROW 17.25 COL 32
     SPACE(0.87) SKIP(0.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Levrantör":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: levtemp T "?" NO-UNDO temp-db levtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-LEVSTAT IN FRAME DIALOG-3
   SHARED                                                               */
ASSIGN 
       FILL-IN-LEVSTAT:HIDDEN IN FRAME DIALOG-3           = TRUE.

/* SETTINGS FOR FILL-IN levtemp.LEVADR IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN levtemp.LEVKOD IN FRAME DIALOG-3
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN levtemp.LEVKONTAKT IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN levtemp.LEVNAMN IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN levtemp.LEVORT IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN levtemp.LEVPNR IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN levtemp.LEVTEL IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "Temp-Tables.levtemp"
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* Levrantör */
DO:
   {muswait.i}
   musz = TRUE.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* Levrantör */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT DIALOG-3
ON CHOOSE OF BTN_AVBRYT IN FRAME DIALOG-3 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* Ok */
DO:
   {muswait.i}
   hlev = TRUE.
   ASSIGN 
   levtemp.LEVKOD       = INPUT levtemp.LEVKOD                
   levtemp.LEVNAMN      = INPUT levtemp.LEVNAMN                           
   levtemp.LEVADR       = INPUT levtemp.LEVADR                
   levtemp.LEVORT       = INPUT levtemp.LEVORT                
   levtemp.LEVTEL       = INPUT levtemp.LEVTEL                
   levtemp.LEVKONTAKT   = INPUT levtemp.LEVKONTAKT            
   levtemp.LEVPNR       = INPUT levtemp.LEVPNR.                   
      
   RUN levspara_UI IN levapph (INPUT vem,INPUT TABLE levtemp,OUTPUT TABLE felmeddtemp).                   
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN NO-APPLY.
   END.
   FILL-IN-LEVSTAT = INPUT FILL-IN-LEVSTAT.
   IF FILL-IN-LEVSTAT = TRUE THEN DO:
      RUN hlevkoll_UI IN levapph (INPUT vem , INPUT FILL-IN-LEVSTAT , OUTPUT TABLE felmeddtemp).                   
      FIND FIRST felmeddtemp NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL TITLE "Huvudleverantör"
         UPDATE answer AS LOGICAL.
         IF answer THEN DO:
            musz = FALSE.                              
         END.                                               
         ELSE DO:
            APPLY "ENTRY" TO FILL-IN-LEVSTAT IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY. 
         END.
      END.           
   END.
   vem = levtemp.LEVKOD.
   RUN huvlev_UI IN levapph (INPUT vem,INPUT hlev,INPUT-OUTPUT FILL-IN-LEVSTAT).             
   musz = FALSE.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "INLKAT"
   inextradatatemp.HUVUDCH = levtemp.LEVKOD.                                              
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp).
   FIND FIRST extradatatemp WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE extradatatemp THEN DO:
      CREATE extradatatemp.
      ASSIGN
      extradatatemp.PROGRAM = "INLKAT"                   
      extradatatemp.HUVUDCH = levtemp.LEVKOD   
      extradatatemp.SOKINT[1] = 0 .   
   END.   
   extradatatemp.SOKLOG[2] = FILL-IN-PRISJMF.
     
   RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   
   
                        
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LEVSTAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LEVSTAT DIALOG-3
ON LEAVE OF FILL-IN-LEVSTAT IN FRAME DIALOG-3 /* Huvudleverantör */
DO:
   FILL-IN-LEVSTAT = INPUT FILL-IN-LEVSTAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LEVSTAT DIALOG-3
ON MOUSE-SELECT-CLICK OF FILL-IN-LEVSTAT IN FRAME DIALOG-3 /* Huvudleverantör */
DO:
  IF INPUT FILL-IN-LEVSTAT = FALSE THEN FILL-IN-LEVSTAT = TRUE. 
  IF INPUT FILL-IN-LEVSTAT = TRUE THEN FILL-IN-LEVSTAT = FALSE. 
  DISPLAY FILL-IN-LEVSTAT WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PRISJMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PRISJMF DIALOG-3
ON LEAVE OF FILL-IN-PRISJMF IN FRAME DIALOG-3 /* Med vid pris jmf */
DO:
  FILL-IN-PRISJMF = INPUT FILL-IN-PRISJMF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PRISJMF DIALOG-3
ON MOUSE-SELECT-CLICK OF FILL-IN-PRISJMF IN FRAME DIALOG-3 /* Med vid pris jmf */
DO:
  IF INPUT FILL-IN-PRISJMF = TRUE THEN FILL-IN-PRISJMF = FALSE.
   IF INPUT FILL-IN-PRISJMF = FALSE THEN FILL-IN-PRISJMF = TRUE.
   DISPLAY FILL-IN-PRISJMF WITH FRAME {&FRAME-NAME}. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME levtemp.LEVKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL levtemp.LEVKOD DIALOG-3
ON LEAVE OF levtemp.LEVKOD IN FRAME DIALOG-3 /* Lev-id */
DO:
   IF INPUT levtemp.LEVKOD = "" THEN DO:
      MESSAGE "Leverantör kan inte vara blank!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF levtemp.LEVKOD NE INPUT levtemp.LEVKOD THEN DO:
      levtemp.LEVKOD = INPUT levtemp.LEVKOD.
      RUN namnkoll_UI IN levapph (INPUT vem,INPUT levtemp.LEVKOD,OUTPUT TABLE felmeddtemp).                   
      FIND FIRST felmeddtemp NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         RETURN NO-APPLY.   
      END.
   END.   
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
   RUN hamtaen_UI IN levapph (INPUT vem,OUTPUT TABLE levtemp).                   
   FIND FIRST levtemp NO-ERROR.
   IF vem = ? THEN DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny".     
   END.
   ELSE DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändra-" + levtemp.LEVNAMN.
     
   END. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "INLKAT"  
   inextradatatemp.HUVUDCH = levtemp.LEVKOD.                                              
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:
      FILL-IN-PRISJMF = extradatatemp.SOKLOG[2].
   END.      
   RUN enable_UI.  

   DISPLAY levtemp.LEVKOD FORMAT "X(10)" WITH FRAME {&FRAME-NAME}.     
   {FRMSIZED.I}
   ASSIGN
   hlev = FALSE.
   FILL-IN-LEVSTAT:HIDDEN = FALSE.
   IF vem = ? THEN vem = vem.
   ELSE RUN huvlev_UI IN levapph (INPUT vem,INPUT hlev,INPUT-OUTPUT FILL-IN-LEVSTAT).             
   DISPLAY FILL-IN-LEVSTAT WITH FRAME {&FRAME-NAME}.  
   IF vem = ? THEN ENABLE levtemp.LEVKOD WITH FRAME {&FRAME-NAME}. 
   levtemp.LEVPNR:FORMAT IN FRAME {&FRAME-NAME} = "999 99".
   {musarrow.i}
   {DIA_M_SLUT.I}
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
  DISPLAY FILL-IN-LEVSTAT FILL-IN-PRISJMF 
      WITH FRAME DIALOG-3.
  IF AVAILABLE levtemp THEN 
    DISPLAY levtemp.LEVKOD levtemp.LEVNAMN levtemp.LEVKONTAKT levtemp.LEVTEL 
          levtemp.LEVADR levtemp.LEVPNR levtemp.LEVORT 
      WITH FRAME DIALOG-3.
  ENABLE levtemp.LEVNAMN FILL-IN-LEVSTAT levtemp.LEVKONTAKT levtemp.LEVTEL 
         levtemp.LEVADR levtemp.LEVPNR levtemp.LEVORT FILL-IN-PRISJMF BTN_OK 
         BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

