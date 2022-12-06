&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt9              PROGRESS
*/
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
DEFINE INPUT PARAMETER BESTRECO AS RECID NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE gamlaprist LIKE AONRTAB.PRISTYP NO-UNDO.
DEFINE QUERY pq FOR PERSONALTAB.
DEFINE QUERY timq FOR TIMKOSTNADSTAB.
DEFINE QUERY aq FOR AONRTAB.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES AUTOMREG

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define FIELDS-IN-QUERY-DIALOG-3 AUTOMREG.PRISTYP AUTOMREG.PRIORITET ~
AUTOMREG.PRISA AUTOMREG.NORMALTID AUTOMREG.RESTIDREG AUTOMREG.TIDREG ~
AUTOMREG.RESARB AUTOMREG.TYPLAGBAS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DIALOG-3 AUTOMREG.PRISTYP ~
AUTOMREG.PRIORITET AUTOMREG.PRISA AUTOMREG.NORMALTID AUTOMREG.RESTIDREG ~
AUTOMREG.TIDREG AUTOMREG.RESARB AUTOMREG.TYPLAGBAS 
&Scoped-define ENABLED-TABLES-IN-QUERY-DIALOG-3 AUTOMREG
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DIALOG-3 AUTOMREG
&Scoped-define QUERY-STRING-DIALOG-3 FOR EACH AUTOMREG SHARE-LOCK
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH AUTOMREG SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 AUTOMREG
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 AUTOMREG


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS AUTOMREG.PRISTYP AUTOMREG.PRIORITET ~
AUTOMREG.PRISA AUTOMREG.NORMALTID AUTOMREG.RESTIDREG AUTOMREG.TIDREG ~
AUTOMREG.RESARB AUTOMREG.TYPLAGBAS 
&Scoped-define ENABLED-TABLES AUTOMREG
&Scoped-define FIRST-ENABLED-TABLE AUTOMREG
&Scoped-Define ENABLED-OBJECTS BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-FIELDS AUTOMREG.PRISTYP AUTOMREG.PRIORITET ~
AUTOMREG.PRISA AUTOMREG.NORMALTID AUTOMREG.RESTIDREG AUTOMREG.TIDREG ~
AUTOMREG.RESARB AUTOMREG.TYPLAGBAS 
&Scoped-define DISPLAYED-TABLES AUTOMREG
&Scoped-define FIRST-DISPLAYED-TABLE AUTOMREG


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT AUTO-END-KEY 
     LABEL "AVBRYT":L 
     SIZE 11 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "OK":L 
     SIZE 10.5 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-3 FOR 
      AUTOMREG SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     AUTOMREG.PRISTYP AT ROW 3 COL 17.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     AUTOMREG.PRIORITET AT ROW 4 COL 17.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 2 BY 1
     AUTOMREG.PRISA AT ROW 5 COL 17.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     AUTOMREG.NORMALTID AT ROW 6 COL 17.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     AUTOMREG.RESTIDREG AT ROW 7 COL 17.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     AUTOMREG.TIDREG AT ROW 8 COL 17.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     AUTOMREG.RESARB AT ROW 9 COL 17.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     AUTOMREG.TYPLAGBAS AT ROW 10 COL 17.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     BTN_OK AT ROW 13.5 COL 11.5
     BTN_AVBRYT AT ROW 13.5 COL 26
     SPACE(15.24) SKIP(1.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Automreg":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
                                                                        */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "rt9.AUTOMREG"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT DIALOG-3
ON CHOOSE OF BTN_AVBRYT IN FRAME DIALOG-3 /* AVBRYT */
DO:
  {muswait.i}
  musz = TRUE.
  IF bestreco = ? THEN DO:
    FIND FIRST AUTOMREG WHERE RECID(AUTOMREG) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE AUTOMREG THEN DELETE AUTOMREG.
  END.   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* OK */
DO:
   {muswait.i}
   DO: 
      DO TRANSACTION:
         ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}}.
         APPLY "GO" TO FRAME {&FRAME-NAME}.    
      END.   
   END.
   IF BESTRECO = ? THEN DO:
      MESSAGE "OM JA NORMALTID SÄTTES PÅ NÅGON NY MÅSTE DICTONARY TEXT OCH
      DEFAULTVÄRDEN ÄNDRAS FÖR AONRTAB ,TIMKOSTNADSTAB, TIDREGITAB".
      MESSAGE AUTOMREG.PRISTYP AUTOMREG.PRISA.
      OPEN QUERY pq FOR EACH PERSONALTAB USE-INDEX PERSONALKOD NO-LOCK.
      GET FIRST pq NO-LOCK.   
      DO WHILE AVAILABLE(PERSONALTAB):                
         DO TRANSACTION:
            CREATE TIMKOSTNADSTAB.
            ASSIGN
            TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            TIMKOSTNADSTAB.PRISTYP = AUTOMREG.PRISTYP
            TIMKOSTNADSTAB.PRISA = AUTOMREG.PRISA.
         END.
         GET NEXT pq NO-LOCK. 
      END.   
   END.                                            
   ELSE DO:                                     
      IF gamlaprist NE AUTOMREG.PRISTYP THEN DO:
         OPEN QUERY timq FOR EACH TIMKOSTNADSTAB WHERE TIMKOSTNADSTAB.PRISTYP = gamlaprist 
         USE-INDEX PRISTYP NO-LOCK.
         DO TRANSACTION:
            GET FIRST timq EXCLUSIVE-LOCK.
            IF AVAILABLE TIMKOSTNADSTAB THEN DO:
               ASSIGN TIMKOSTNADSTAB.PRISTYP = AUTOMREG.PRISTYP.
            END.
         END.   
         DO WHILE AVAILABLE(TIMKOSTNADSTAB):
            DO TRANSACTION:
               GET NEXT timq EXCLUSIVE-LOCK.
               IF AVAILABLE TIMKOSTNADSTAB THEN DO:
                  ASSIGN TIMKOSTNADSTAB.PRISTYP = AUTOMREG.PRISTYP.
               END.
            END.
         END.
      END.  
      /*SKALL AONR UPPDATERAS ? EJ AVSLUTADE AONR?*/
      IF gamlaprist NE AUTOMREG.PRISTYP THEN DO:
         OPEN QUERY aq FOR EACH AONRTAB WHERE AONRTAB.PRISTYP = gamlaprist 
         USE-INDEX PRISTYP NO-LOCK.
         DO TRANSACTION:
            GET FIRST aq EXCLUSIVE-LOCK.
            IF AVAILABLE AONRTAB THEN DO:
               ASSIGN AONRTAB.PRISTYP = AUTOMREG.PRISTYP.
            END.
         END.   
         DO WHILE AVAILABLE(AONRTAB):
            DO TRANSACTION:
               GET NEXT aq EXCLUSIVE-LOCK.
               IF AVAILABLE AONRTAB THEN DO:
                  ASSIGN AONRTAB.PRISTYP = AUTOMREG.PRISTYP.
               END.
            END.
         END.        
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
   IF BESTRECO = ? THEN DO TRANSACTION:
     CREATE AUTOMREG.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "NY".
     brec = RECID(AUTOMREG).
   END.
   ELSE DO TRANSACTION:
     FIND AUTOMREG WHERE RECID(AUTOMREG) = BESTRECO EXCLUSIVE-LOCK.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "ÄNDRA-" + AUTOMREG.PRISTYP.
     brec = RECID(AUTOMREG).
     gamlaprist = AUTOMREG.PRISTYP.
   END.    
  RUN enable_UI.
  {musarrow.i}
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
  IF AVAILABLE AUTOMREG THEN 
    DISPLAY AUTOMREG.PRISTYP AUTOMREG.PRIORITET AUTOMREG.PRISA AUTOMREG.NORMALTID 
          AUTOMREG.RESTIDREG AUTOMREG.TIDREG AUTOMREG.RESARB AUTOMREG.TYPLAGBAS 
      WITH FRAME DIALOG-3.
  ENABLE AUTOMREG.PRISTYP AUTOMREG.PRIORITET AUTOMREG.PRISA AUTOMREG.NORMALTID 
         AUTOMREG.RESTIDREG AUTOMREG.TIDREG AUTOMREG.RESARB AUTOMREG.TYPLAGBAS 
         BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

