&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
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

  Created: 12/18/97 - 11:17 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{SOKDEF.I}
&Scoped-define SHARED SHARED
{SCADMIN.I}
DEFINE VARIABLE forlanyapph AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER forid AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR eforlaggtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-FORLAGG FILL-IN-BENAMNING ~
FILL-IN-DJUP FILL-IN-SAM FILL-IN-ROR FILL-IN-MARK FILL-IN-FAKTOR ~
FILL-IN-BREDD FILL-IN-TILLAGG BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-FORLAGG FILL-IN-BENAMNING ~
FILL-IN-DJUP FILL-IN-SAM FILL-IN-ROR FILL-IN-MARK FILL-IN-FAKTOR ~
FILL-IN-BREDD FILL-IN-TILLAGG 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-BENAMNING AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BREDD AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Bredd (i cm)" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DJUP AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Djup (i cm)" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FAKTOR AS DECIMAL FORMAT "9.99":U INITIAL 0 
     LABEL "Faktor" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FORLAGG AS CHARACTER FORMAT "X(256)":U 
     LABEL "Förläggningssätt" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MARK AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Svår mark" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ROR AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Rör" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SAM AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Samförläggning" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TILLAGG AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Tillägg (i cm)" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-FORLAGG AT ROW 2 COL 17 COLON-ALIGNED
     FILL-IN-BENAMNING AT ROW 3.75 COL 17 COLON-ALIGNED
     FILL-IN-DJUP AT ROW 5.5 COL 17 COLON-ALIGNED
     FILL-IN-SAM AT ROW 7.25 COL 17 COLON-ALIGNED
     FILL-IN-ROR AT ROW 9 COL 17 COLON-ALIGNED
     FILL-IN-MARK AT ROW 10.75 COL 17 COLON-ALIGNED
     FILL-IN-FAKTOR AT ROW 12.5 COL 17 COLON-ALIGNED
     FILL-IN-BREDD AT ROW 14.25 COL 17 COLON-ALIGNED
     FILL-IN-TILLAGG AT ROW 16 COL 17 COLON-ALIGNED
     BTN_OK AT ROW 17 COL 60.5
     BTN_AVB AT ROW 17 COL 75.5
     SPACE(6.24) SKIP(0.78)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Förläggningssätt".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Förläggningssätt */
DO:
   IF VALID-HANDLE(forlanyapph) THEN DELETE PROCEDURE forlanyapph.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Förläggningssätt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:  
   EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
   ASSIGN   
   FILL-IN-BREDD =       INPUT FILL-IN-BREDD
   FILL-IN-FORLAGG =    INPUT FILL-IN-FORLAGG
   FILL-IN-BENAMNING =  INPUT FILL-IN-BENAMNING   
   FILL-IN-DJUP =       INPUT FILL-IN-DJUP
   FILL-IN-SAM =        INPUT FILL-IN-SAM
   FILL-IN-ROR =        INPUT FILL-IN-ROR
   FILL-IN-MARK =       INPUT FILL-IN-MARK
   FILL-IN-FAKTOR =     INPUT FILL-IN-FAKTOR
   FILL-IN-TILLAGG =    INPUT FILL-IN-TILLAGG.
   CREATE valsoktemp.
   ASSIGN 
   valsoktemp.SOKCHAR[1] = FILL-IN-FORLAGG  
   valsoktemp.SOKCHAR[2] = FILL-IN-BENAMNING
   valsoktemp.SOKINT[1] = FILL-IN-DJUP     
   valsoktemp.SOKINT[2] = FILL-IN-TILLAGG
   valsoktemp.SOKLOG[1] = FILL-IN-SAM      
   valsoktemp.SOKLOG[2] = FILL-IN-ROR      
   valsoktemp.SOKLOG[3] = FILL-IN-MARK     
   valsoktemp.SOKDEC[1] = FILL-IN-FAKTOR
   valsoktemp.SOKDEC[2] = FILL-IN-BREDD / 100.   
   IF forid = 0 THEN DO:
      FIND LAST tempforlagg USE-INDEX ORDNING NO-ERROR .
      IF AVAILABLE tempforlagg THEN valsoktemp.SOKINT[4] = tempforlagg.ORDNING.
      valsoktemp.SOKINT[4] = valsoktemp.SOKINT[4] + 1.
   END.   
   felmedd = "".
   RUN btnok_UI IN forlanyapph (INPUT forid, OUTPUT felmedd,INPUT TABLE valsoktemp,OUTPUT TABLE eforlaggtemp).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
      felmedd = "".
      APPLY "ENTRY" TO FILL-IN-FORLAGG IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON GO OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:  
   IF VALID-HANDLE(forlanyapph) THEN DELETE PROCEDURE forlanyapph.
   musz = FALSE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FORLAGG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FORLAGG DIALOG-1
ON LEAVE OF FILL-IN-FORLAGG IN FRAME DIALOG-1 /* Förläggningssätt */
DO:
   FILL-IN-FORLAGG = INPUT FILL-IN-FORLAGG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MARK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MARK DIALOG-1
ON LEAVE OF FILL-IN-MARK IN FRAME DIALOG-1 /* Svår mark */
DO:
   FILL-IN-MARK = INPUT FILL-IN-MARK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MARK DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-MARK IN FRAME DIALOG-1 /* Svår mark */
DO:
   IF INPUT FILL-IN-MARK = FALSE THEN FILL-IN-MARK = TRUE. 
   IF INPUT FILL-IN-MARK = TRUE THEN FILL-IN-MARK = FALSE. 
   DISPLAY FILL-IN-MARK WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ROR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ROR DIALOG-1
ON LEAVE OF FILL-IN-ROR IN FRAME DIALOG-1 /* Rör */
DO:
   FILL-IN-ROR = INPUT FILL-IN-ROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ROR DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-ROR IN FRAME DIALOG-1 /* Rör */
DO:
   IF INPUT FILL-IN-ROR = FALSE THEN FILL-IN-ROR = TRUE. 
   IF INPUT FILL-IN-ROR = TRUE THEN FILL-IN-ROR = FALSE. 
   DISPLAY FILL-IN-ROR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SAM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SAM DIALOG-1
ON LEAVE OF FILL-IN-SAM IN FRAME DIALOG-1 /* Samförläggning */
DO:
   FILL-IN-SAM = INPUT FILL-IN-SAM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SAM DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-SAM IN FRAME DIALOG-1 /* Samförläggning */
DO:
   IF INPUT FILL-IN-SAM = FALSE THEN FILL-IN-SAM = TRUE. 
   IF INPUT FILL-IN-SAM = TRUE THEN FILL-IN-SAM = FALSE. 
   DISPLAY FILL-IN-SAM WITH FRAME {&FRAME-NAME}.  
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
   IF forid > 0 THEN DO:       
      FIND FIRST tempforlagg WHERE tempforlagg.ID = forid NO-ERROR.
      IF AVAILABLE tempforlagg THEN DO:
         ASSIGN
         FILL-IN-FORLAGG = tempforlagg.FORLAGG
         FILL-IN-BENAMNING = tempforlagg.BENAMNING
         FILL-IN-DJUP = tempforlagg.DJUP
         FILL-IN-SAM = tempforlagg.SAM
         FILL-IN-ROR = tempforlagg.ROR
         FILL-IN-MARK = tempforlagg.MARK
         FILL-IN-FAKTOR = tempforlagg.FAKTOR
         FILL-IN-TILLAGG = tempforlagg.TILLAGG
         FILL-IN-BREDD   = tempforlagg.BREDD * 100.
      END.
   END.      
   ELSE DO:   
      ASSIGN
      FILL-IN-BREDD = 50
      FILL-IN-DJUP = 45
      FILL-IN-SAM = FALSE
      FILL-IN-ROR = FALSE
      FILL-IN-MARK = FALSE
      FILL-IN-FAKTOR = 2
      FILL-IN-TILLAGG = 0.
   END.
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
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/      
   IF Guru.Konstanter:appcon THEN DO:
      RUN FORLANYAPP.P PERSISTENT SET forlanyapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN FORLANYAPP.P PERSISTENT SET forlanyapph.
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
  DISPLAY FILL-IN-FORLAGG FILL-IN-BENAMNING FILL-IN-DJUP FILL-IN-SAM FILL-IN-ROR 
          FILL-IN-MARK FILL-IN-FAKTOR FILL-IN-BREDD FILL-IN-TILLAGG 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-FORLAGG FILL-IN-BENAMNING FILL-IN-DJUP FILL-IN-SAM FILL-IN-ROR 
         FILL-IN-MARK FILL-IN-FAKTOR FILL-IN-BREDD FILL-IN-TILLAGG BTN_OK 
         BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

