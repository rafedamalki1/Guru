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

  Created: 95/10/22 -  2:57 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
 DEFINE INPUT PARAMETER mtrl_rowid AS ROWID.

/* Local Variable Definitions ---                                       */
 {ALLDEF.I}
 {GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE reprowid AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE fackvar AS CHARACTER NO-UNDO.  
DEFINE VARIABLE var1 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE var2 AS CHARACTER FORMAT "X(6)" NO-UNDO.   
DEFINE VARIABLE fackid AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO. 
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{SPECMTRLTEMP.I}
{MTRLTEMP.I}
DEFINE SHARED TEMP-TABLE aspec_mtrl NO-UNDO LIKE spec_mtrl
   FIELD ny AS LOGICAL
   FIELD andrad AS LOGICAL
   FIELD bort AS LOGICAL.

    


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-BESTP FILL-IN-BESTKV FILL-IN-FACKID1 ~
FILL-IN-FACKID2 FILL-IN-ARSOM btn_ok BTN_AVB FILL-IN-ENR FILL-IN-BEN ~
FILL-IN-ENHET 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-BESTP FILL-IN-BESTKV ~
FILL-IN-FACKID1 FILL-IN-FACKID2 FILL-IN-ARSOM FILL-IN-ENR FILL-IN-BEN ~
FILL-IN-ENHET 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY DEFAULT 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON btn_ok 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-ARSOM AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Omsättning/år" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 33 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-BESTKV AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Best-antal" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BESTP AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Best-punkt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID1 AS CHARACTER FORMAT "X(2)":U 
     LABEL "Fack-id (aa)" 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID2 AS CHARACTER FORMAT "X(10)":U 
     LABEL "+ (999999)" 
     VIEW-AS FILL-IN 
     SIZE 12.75 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-BESTP AT ROW 6.17 COL 14.25 COLON-ALIGNED
     FILL-IN-BESTKV AT ROW 7.67 COL 14.25 COLON-ALIGNED
     FILL-IN-FACKID1 AT ROW 9.17 COL 14.25 COLON-ALIGNED
     FILL-IN-FACKID2 AT ROW 9.17 COL 30.75 COLON-ALIGNED
     FILL-IN-ARSOM AT ROW 10.67 COL 14.25 COLON-ALIGNED
     btn_ok AT ROW 13.04 COL 20.5
     BTN_AVB AT ROW 13.04 COL 35.25
     FILL-IN-ENR AT ROW 1.67 COL 14.25 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.17 COL 14.25 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 4.67 COL 14.25 COLON-ALIGNED
     SPACE(19.74) SKIP(8.98)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Depå information":L
         CANCEL-BUTTON BTN_AVB.


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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Depå information */
DO:
   APPLY "CHOOSE" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Depå information */
DO:
   APPLY "CHOOSE" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* Ok */
DO: 
   ASSIGN 
   FILL-IN-BESTP = INPUT FILL-IN-BESTP
   FILL-IN-BESTKV = INPUT FILL-IN-BESTKV
   fackid = var1 + var2
   FILL-IN-ARSOM = INPUT FILL-IN-ARSOM.
   IF INPUT FILL-IN-BESTKV = 0 THEN DO:
      MESSAGE "Best-antal får inte vara 0."  VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BESTKV IN FRAME {&FRAME-NAME}. 
      APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.   
   ELSE DO:      
      IF musz = TRUE THEN DO:
         FIND FIRST mtrltemp WHERE ROWID(mtrltemp) = mtrl_rowid NO-LOCK NO-ERROR.
         FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrltemp.ENR 
         NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE spec_mtrl THEN DO:            
            IF fackid NE " " THEN  
            FIND spec_mtrl WHERE spec_mtrl.FACKID = fackid NO-LOCK NO-ERROR.
            IF AVAILABLE spec_mtrl THEN DO:
               MESSAGE "Det finns redan ett " + Guru.Konstanter:genk + " kopplat till detta Fack-id. Ange ett nytt." 
               VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO FILL-IN-FACKID1 IN FRAME {&FRAME-NAME}.
               APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
            END.
            ELSE DO:                 
               CREATE spec_mtrl.
               ASSIGN 
               spec_mtrl.DEPNR = vald_depa
               spec_mtrl.ENR = mtrltemp.ENR
               spec_mtrl.BENAMNING = mtrltemp.BENAMNING
               spec_mtrl.ENHET = mtrltemp.ENHET
               spec_mtrl.BESTPUNKT = FILL-IN-BESTP 
               spec_mtrl.BESTKV = FILL-IN-BESTKV
               spec_mtrl.FACKID = fackid
               spec_mtrl.OMSATT = FILL-IN-ARSOM
               spec_mtrl.NPRIS = mtrltemp.NPRIS
               spec_mtrl.BPRIS = mtrltemp.BPRIS  
               spec_mtrl.LAGER = TRUE
               reprowid = ROWID(spec_mtrl).
               /*Niklas personlig spec_mtrl*/
               IF mtrltemp.LEVKOD BEGINS "99" THEN spec_mtrl.LEVKOD = "99".           
               ELSE spec_mtrl.LEVKOD = mtrltemp.LEVKOD. 
               FIND FIRST aspec_mtrl WHERE aspec_mtrl.ENR = spec_mtrl.ENR NO-ERROR.
               IF AVAILABLE aspec_mtrl THEN DO:
                  /*för att få in ett nytt pris i depån gör de först borttag av enr och lägger sedan dit det
                  med rätt pris från katalog*/
                  IF aspec_mtrl.BORT = TRUE  THEN DO:
                     CREATE aspec_mtrl.
                     ASSIGN aspec_mtrl.NY = FALSE
                     aspec_mtrl.ANDRAD = FALSE
                     aspec_mtrl.BORT = FALSE.
                  END.
               END.
               IF NOT AVAILABLE aspec_mtrl THEN DO:   
                  CREATE aspec_mtrl.
               END.
               BUFFER-COPY spec_mtrl TO aspec_mtrl.
               ASSIGN aspec_mtrl.NY = TRUE 
               aspec_mtrl.ANDRAD = FALSE
               aspec_mtrl.BORT = FALSE.
               APPLY "GO" TO FRAME {&FRAME-NAME}.  
            END.   
         END.                          
         ELSE DO: 
            IF fackid NE " " THEN
            FIND FIRST spec_mtrl WHERE spec_mtrl.FACKID = fackid NO-LOCK NO-ERROR.
            IF AVAILABLE spec_mtrl THEN DO:
               MESSAGE "Det finns redan ett " + Guru.Konstanter:genk + " kopplat till detta Fack-id. Ange ett nytt." 
               VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO FILL-IN-FACKID1 IN FRAME {&FRAME-NAME}. 
               APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
            END. 
            ELSE DO:
               ASSIGN         
               spec_mtrl.BESTPUNKT = FILL-IN-BESTP 
               spec_mtrl.BESTKV = FILL-IN-BESTKV
               spec_mtrl.FACKID = fackid
               spec_mtrl.OMSATT = FILL-IN-ARSOM
               reprowid = ROWID(spec_mtrl). 
               FIND FIRST aspec_mtrl WHERE aspec_mtrl.ENR = spec_mtrl.ENR NO-ERROR.
               IF NOT AVAILABLE aspec_mtrl THEN DO:   
                  CREATE aspec_mtrl.
                  ASSIGN aspec_mtrl.NY = FALSE
                  aspec_mtrl.ANDRAD = FALSE
                  aspec_mtrl.BORT = FALSE.
               END.
               BUFFER-COPY spec_mtrl TO aspec_mtrl.
               ASSIGN aspec_mtrl.ANDRAD = TRUE.   
               APPLY "GO" TO FRAME {&FRAME-NAME}. 
            END.   
         END.   
      END.       
      ELSE DO: 
         IF fackvar NE fackid THEN DO:
            IF fackid NE " " THEN DO:
               FIND FIRST spec_mtrl WHERE spec_mtrl.FACKID = fackid NO-LOCK NO-ERROR.
               IF AVAILABLE spec_mtrl THEN DO:
                  MESSAGE "Det finns redan ett " + Guru.Konstanter:genk + " kopplat till detta Fack-id. Ange ett nytt." 
                  VIEW-AS ALERT-BOX.
                  APPLY "ENTRY" TO FILL-IN-FACKID1 IN FRAME {&FRAME-NAME}.
                  APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
               END.
            END.   
         END.                  
         FIND FIRST spec_mtrl WHERE ROWID(spec_mtrl) = mtrl_rowid NO-ERROR.
         ASSIGN
         spec_mtrl.BESTPUNKT = FILL-IN-BESTP 
         spec_mtrl.BESTKV = FILL-IN-BESTKV
         spec_mtrl.FACKID = fackid
         spec_mtrl.OMSATT = FILL-IN-ARSOM
         reprowid = ROWID(spec_mtrl).
         FIND FIRST aspec_mtrl WHERE aspec_mtrl.ENR = spec_mtrl.ENR NO-ERROR.
         IF NOT AVAILABLE aspec_mtrl THEN DO:   
            CREATE aspec_mtrl. 
            ASSIGN aspec_mtrl.NY = FALSE 
            aspec_mtrl.ANDRAD = FALSE
            aspec_mtrl.BORT = FALSE.
         END.
         BUFFER-COPY spec_mtrl TO aspec_mtrl.
         ASSIGN aspec_mtrl.ANDRAD = TRUE.   
         APPLY "GO" TO FRAME {&FRAME-NAME}.          
      END.                
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACKID1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 DIALOG-1
ON LEAVE OF FILL-IN-FACKID1 IN FRAME DIALOG-1 /* Fack-id (aa) */
DO: 
   ASSIGN
   var1 = INPUT FILL-IN-FACKID1  
   var1 = TRIM(var1)
   langd = LENGTH(var1).  
   IF langd = 2 THEN DO:
      FILL-IN-FACKID1 = var1.      
   END.
   ELSE DO:
      DO WHILE langd < 2:
         ASSIGN
         var1 = var1 + " " 
         langd = langd + 1.
      END.  
      FILL-IN-FACKID1 = var1.     
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACKID2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 DIALOG-1
ON LEAVE OF FILL-IN-FACKID2 IN FRAME DIALOG-1 /* + (999999) */
DO:   
   ASSIGN
   var2 = INPUT FILL-IN-FACKID2  
   var2 = TRIM(var2)
   langd = LENGTH(var2).  
   IF langd = 6 THEN DO:
      FILL-IN-FACKID2 = var2.      
   END.
   ELSE DO:
      DO WHILE langd < 6:
         ASSIGN
         var2 = " " + var2
         langd = langd + 1.
      END.  
      FILL-IN-FACKID2 = var2.     
   END.      
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
   IF musz = TRUE THEN DO:      
      FIND mtrltemp WHERE ROWID(mtrltemp) = mtrl_rowid NO-ERROR.
      ASSIGN
      FILL-IN-ENR:LABEL = Guru.Konstanter:genk
      FILL-IN-BEN = mtrltemp.BENAMNING
      FILL-IN-ENR = mtrltemp.ENR
      FILL-IN-ENHET = mtrltemp.ENHET
      FILL-IN-BESTP = 0
      FILL-IN-BESTKV = 1
      FILL-IN-FACKID1 = " "  
      FILL-IN-FACKID2 = " "
      FILL-IN-ARSOM = 0.      
   END. 
   /*ELSE IF sokant = TRUE THEN DO:
      FIND sok_mtrl WHERE ROWID(sok_mtrl) = mtrl_rowid NO-ERROR.
      ASSIGN
      FILL-IN-BEN = sok_mtrl.BENAMNING
      FILL-IN-ENR = sok_mtrl.ENR
      FILL-IN-ENHET = sok_mtrl.ENHET      
      FILL-IN-BESTP = 0
      FILL-IN-BESTKV = 1
      FILL-IN-FACKID1 = " "
      FILL-IN-FACKID2 = " "
      FILL-IN-ARSOM = 0.
   END.               */
   ELSE DO:
      FIND spec_mtrl WHERE ROWID(spec_mtrl) = mtrl_rowid NO-ERROR.
      ASSIGN 
      FILL-IN-BEN = spec_mtrl.BENAMNING
      FILL-IN-ENR = spec_mtrl.ENR
      FILL-IN-ENHET = spec_mtrl.ENHET
      FILL-IN-BESTP = spec_mtrl.BESTPUNKT
      FILL-IN-BESTKV = spec_mtrl.BESTKVANT
      FILL-IN-FACKID1 = SUBSTRING(spec_mtrl.FACKID,1,2)   
      FILL-IN-FACKID2 = SUBSTRING(spec_mtrl.FACKID,3)
      FILL-IN-ARSOM = spec_mtrl.OMSATT
      fackvar = spec_mtrl.FACKID
      var1 = SUBSTRING(spec_mtrl.FACKID,1,2)
      var2 = SUBSTRING(spec_mtrl.FACKID,3).
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
  DISPLAY FILL-IN-BESTP FILL-IN-BESTKV FILL-IN-FACKID1 FILL-IN-FACKID2 
          FILL-IN-ARSOM FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-BESTP FILL-IN-BESTKV FILL-IN-FACKID1 FILL-IN-FACKID2 
         FILL-IN-ARSOM btn_ok BTN_AVB FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

