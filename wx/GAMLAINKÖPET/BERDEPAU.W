&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 08/12/96 -  3:11 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */ 
DEFINE INPUT PARAMETER mtrl_rowid AS ROWID.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
&Scoped-define SHARED SHARED
{BESTMTRL.I}
DEFINE SHARED VARIABLE datvar AS DATE NO-UNDO. 
DEFINE VARIABLE varpris AS DECIMAL NO-UNDO. 
DEFINE VARIABLE forsta AS ROWID NO-UNDO.       
DEFINE VARIABLE kodlev AS CHARACTER NO-UNDO.

DEFINE BUFFER bmtrlbuf FOR best_mtrl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-2

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_UP FILL-IN-ANTAL BTN_MIN BTN_OK BTN_AVB ~
FILL-IN-ENR FILL-IN-BEN FILL-IN-BESTANT FILL-IN-ENHET FILL-IN-LEV 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ANTAL FILL-IN-ENR FILL-IN-BEN ~
FILL-IN-BESTANT FILL-IN-ENHET FILL-IN-LEV 

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

DEFINE BUTTON BTN_MIN 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UP 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Totalt antal." 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ben?mning" 
      VIEW-AS TEXT 
     SIZE 33 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-BESTANT AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Antal till bered." 
      VIEW-AS TEXT 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
      VIEW-AS TEXT 
     SIZE 5.63 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEV AS CHARACTER FORMAT "X(4)":U INITIAL "0" 
     LABEL "Lev" 
      VIEW-AS TEXT 
     SIZE 6 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     BTN_UP AT ROW 7.25 COL 32.63
     FILL-IN-ANTAL AT ROW 7.75 COL 21.75 COLON-ALIGNED
     BTN_MIN AT ROW 8.42 COL 32.63
     BTN_OK AT ROW 10.25 COL 19.38
     BTN_AVB AT ROW 10.25 COL 34.38
     FILL-IN-ENR AT ROW 1.75 COL 5.75 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.25 COL 11.75 COLON-ALIGNED
     FILL-IN-BESTANT AT ROW 4.75 COL 19.75 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 4.75 COL 36.13 COLON-ALIGNED
     FILL-IN-LEV AT ROW 6.25 COL 5.75 COLON-ALIGNED
     SPACE(35.24) SKIP(4.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Antal ut?ver beredning".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-2
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-2:SCROLLABLE       = FALSE
       FRAME DIALOG-2:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON END-ERROR OF FRAME DIALOG-2 /* Antal ut?ver beredning */
DO:
  APPLY "GO" TO FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON ENDKEY OF FRAME DIALOG-2 /* Antal ut?ver beredning */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-2
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-2 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MIN DIALOG-2
ON CHOOSE OF BTN_MIN IN FRAME DIALOG-2 /* - */
DO:     
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   IF FILL-IN-ANTAL >= best_mtrl.ANTAL + 1 THEN DO:
      FILL-IN-ANTAL = FILL-IN-ANTAL - 1.
   END.   
   ELSE DO:
      MESSAGE "Antal kan inte vara mindre ?n antal till beredningen." VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.         
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-2
ON CHOOSE OF BTN_OK IN FRAME DIALOG-2 /* Ok */
DO:           
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   IF FILL-IN-ANTAL < best_mtrl.ANTAL THEN DO:
      MESSAGE "Antal kan inte vara mindre ?n antal till beredningen." 
      VIEW-AS ALERT-BOX TITLE "Meddelande".      
      FILL-IN-ANTAL = best_mtrl.ANTAL.    
      DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}.
      APPLY "ENTRY" TO FILL-IN-ANTAL.
      RETURN NO-APPLY.
   END. 
   FIND bmtrlbuf WHERE bmtrlbuf.ENR = best_mtrl.ENR AND 
   bmtrlbuf.LEVKOD = bmtrlbuf.LEVKOD AND bmtrlbuf.DBEST = "DEP?" AND
   bmtrlbuf.DATUM = best_mtrl.DATUM AND bmtrlbuf.DELNR = best_mtrl.DELNR NO-ERROR.
   IF NOT AVAILABLE bmtrlbuf THEN DO:  
      IF FILL-IN-ANTAL = best_mtrl.ANTAL THEN DO:
         best_mtrl.ANTAL = best_mtrl.ANTAL.
      END.                                 
      ELSE DO:    
         CREATE bmtrlbuf.
         BUFFER-COPY best_mtrl TO bmtrlbuf.         
         ASSIGN
         bmtrlbuf.ANTAL = FILL-IN-ANTAL - best_mtrl.ANTAL
         bmtrlbuf.LEVKOD = kodlev
         bmtrlbuf.DBEST = "DEP?".
         /*
         ASSIGN          
         bmtrlbuf.ENR = FILL-IN-ENR
         bmtrlbuf.BENAMNING = FILL-IN-BEN
         bmtrlbuf.ENHET = FILL-IN-ENHET            
         bmtrlbuf.ANTAL = FILL-IN-ANTAL - best_mtrl.ANTAL
         bmtrlbuf.PRIS = varpris 
         bmtrlbuf.OPRIS = varpris         
         bmtrlbuf.LEVKOD = kodlev
         bmtrlbuf.DBEST = "DEP?"
         bmtrlbuf.DATUM = datvar.                
         */
      END.   
      APPLY "GO" TO FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      IF FILL-IN-ANTAL = best_mtrl.ANTAL THEN DO:
         DELETE bmtrlbuf.
      END.
      ELSE DO:
         ASSIGN               
         bmtrlbuf.ANTAL = FILL-IN-ANTAL - best_mtrl.ANTAL.      
      END.   
      APPLY "GO" TO FRAME {&FRAME-NAME}.
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UP DIALOG-2
ON CHOOSE OF BTN_UP IN FRAME DIALOG-2 /* + */
DO:
   ASSIGN
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.    
   FILL-IN-ANTAL = FILL-IN-ANTAL + 1.  
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-2 


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
   FIND best_mtrl WHERE ROWID(best_mtrl) = mtrl_rowid NO-ERROR.
   ASSIGN
   kodlev = best_mtrl.LEVKOD
   forsta = mtrl_rowid.
   FIND bmtrlbuf WHERE bmtrlbuf.ENR = best_mtrl.ENR AND 
   bmtrlbuf.LEVKOD = best_mtrl.LEVKOD AND bmtrlbuf.DBEST = "DEP?" AND
   bmtrlbuf.DATUM = best_mtrl.DATUM AND bmtrlbuf.DELNR = best_mtrl.DELNR NO-ERROR.
   IF NOT AVAILABLE bmtrlbuf THEN DO:       
      ASSIGN 
      varpris = best_mtrl.PRIS       
      FILL-IN-BEN = best_mtrl.BENAMNING
      FILL-IN-ENR = best_mtrl.ENR
      FILL-IN-LEV = best_mtrl.LEVKOD
      FILL-IN-ENHET = best_mtrl.ENHET
      FILL-IN-BESTANT = best_mtrl.ANTAL
      FILL-IN-ANTAL = best_mtrl.ANTAL.
   END.
   ELSE DO:                   
      ASSIGN 
      varpris = best_mtrl.PRIS       
      FILL-IN-BEN = best_mtrl.BENAMNING
      FILL-IN-ENR = best_mtrl.ENR
      FILL-IN-LEV = best_mtrl.LEVKOD
      FILL-IN-ENHET = best_mtrl.ENHET     
      FILL-IN-ANTAL = bmtrlbuf.ANTAL + best_mtrl.ANTAL   
      FILL-IN-BESTANT = best_mtrl.ANTAL.
   END.   
  RUN enable_UI.       
   {FRMSIZED.I}  
   {musarrow.i}
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.
   {DIA_M_SLUT.I}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-2  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-2  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-ANTAL FILL-IN-ENR FILL-IN-BEN FILL-IN-BESTANT FILL-IN-ENHET 
          FILL-IN-LEV 
      WITH FRAME DIALOG-2.
  ENABLE BTN_UP FILL-IN-ANTAL BTN_MIN BTN_OK BTN_AVB FILL-IN-ENR FILL-IN-BEN 
         FILL-IN-BESTANT FILL-IN-ENHET FILL-IN-LEV 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


