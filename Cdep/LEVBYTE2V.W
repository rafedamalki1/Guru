&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 12/12/96 -  9:29 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER mtrl_rowid AS ROWID.
/* Local Variable Definitions ---                                       */ 
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE lev1 AS CHARACTER NO-UNDO.    
DEFINE NEW SHARED VARIABLE lev2 AS CHARACTER NO-UNDO.  
  
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE vald_lev  AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE datvar AS DATE NO-UNDO.
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.

DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
{ALLDEF.I}

&Scoped-define NEW 
&Scoped-define SHARED SHARED
{SPECMTRLTEMP.I}
{MTRLTEMP.I}
{LEVTEMP.I}
    
DEFINE BUFFER bestbuff FOR spec_mtrl.
DEFINE TEMP-TABLE emtrltemp2 NO-UNDO LIKE emtrltemp.




/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_LEVTILL BTN_BEST BTN_AVSL FILL-IN-ENR ~
FILL-IN-BEN FILL-IN-ENHET 
&Scoped-Define DISPLAYED-OBJECTS CMB_LEVFR CMB_LEVTILL FILL-IN-ENR ~
FILL-IN-BEN FILL-IN-ENHET 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BEST 
     LABEL "Ok" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_LEVFR AS CHARACTER FORMAT "X(15)":U 
     LABEL "Från leverantör" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LEVTILL AS CHARACTER FORMAT "X(15)":U 
     LABEL "Till leverantör" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 35.63 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 27.5 BY .67 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     CMB_LEVFR AT ROW 8 COL 19 COLON-ALIGNED
     CMB_LEVTILL AT ROW 10 COL 19 COLON-ALIGNED
     BTN_BEST AT ROW 11.75 COL 27.63
     BTN_AVSL AT ROW 11.75 COL 42.63
     FILL-IN-ENR AT ROW 2 COL 19 COLON-ALIGNED
     FILL-IN-BEN AT ROW 4 COL 19 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 6 COL 19 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 56.25 BY 12.13.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Byt leverantör"
         HEIGHT             = 12.21
         WIDTH              = 56.63
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 82
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 82
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
/* SETTINGS FOR COMBO-BOX CMB_LEVFR IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       CMB_LEVFR:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       CMB_LEVTILL:HIDDEN IN FRAME FRAME-B           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL WINDOW-2
ON CHOOSE OF BTN_AVSL IN FRAME FRAME-B /* Avbryt */
DO:       
   APPLY "CLOSE":U TO THIS-PROCEDURE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BEST WINDOW-2
ON CHOOSE OF BTN_BEST IN FRAME FRAME-B /* Ok */
DO:   
   
   IF mtrl_rowid = ? THEN DO:
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev2 NO-LOCK NO-ERROR.
      MESSAGE "Är det korrekt att Ni vill byta till " + levtemp.LEVNAMN + " på alla artiklar?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.
      IF svar THEN DO:
         FOR EACH spec_mtrl:            
            spec_mtrl.LEVKOD = levtemp.LEVKOD.
         END.
         APPLY "GO" TO FRAME {&FRAME-NAME}.              
         APPLY "CLOSE":U TO THIS-PROCEDURE. 
      END.
      ELSE DO:      
         RETURN NO-APPLY.
      END.
   END.
   ELSE DO:   
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev2 NO-LOCK NO-ERROR.      

      EMPTY TEMP-TABLE emtrltemp2 NO-ERROR.
      RUN mtrllevhmtkoll_UI IN mtrlbapph (INPUT levtemp.LEVKOD ,OUTPUT TABLE emtrltemp2).             
      FIND FIRST emtrltemp2 WHERE emtrltemp2.LEVKOD = levtemp.LEVKOD AND   
      emtrltemp2.KALKNR = 0 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE emtrltemp2 THEN DO:
         MESSAGE "Det finns ingen materielkatalog upplagd för " + levtemp.LEVNAMN
         VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      ELSE DO: 
         {muswait.i} 
         FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev2 NO-LOCK NO-ERROR.      
         FIND FIRST emtrltemp WHERE emtrltemp.LEVKOD = levtemp.LEVKOD AND   
         emtrltemp.KALKNR = 0 AND emtrltemp.ENR = spec_mtrl.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
         IF NOT AVAILABLE emtrltemp THEN DO:     
            IF SUBSTRING(spec_mtrl.ENR,1,1) = "E" THEN DO:
               FIND FIRST emtrltemp WHERE emtrltemp.LEVKOD = levtemp.LEVKOD AND   
               emtrltemp.KALKNR = 0 AND emtrltemp.ENR = SUBSTRING(spec_mtrl.ENR,2,11) USE-INDEX LEV NO-LOCK NO-ERROR.
               IF NOT AVAILABLE emtrltemp THEN DO:
                  MESSAGE "Det finns ingen artikel hos " + levtemp.LEVNAMN + " som matchar " + spec_mtrl.ENR +
                  ". Vill Ni göra bytet ändå?"
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.
                  IF svar THEN DO:
                     FIND FIRST bestbuff WHERE bestbuff.ENR = spec_mtrl.ENR AND
                     bestbuff.LEVKOD = levtemp.LEVKOD NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE bestbuff THEN DO:                     
                        spec_mtrl.LEVKOD = levtemp.LEVKOD.
                     END. 
                     ELSE DO:
                        bestbuff.BERKVANT = bestbuff.BERKVANT + spec_mtrl.BERKVANT.
                        DELETE spec_mtrl.
                     END.
                  END.  
                  APPLY "GO" TO FRAME {&FRAME-NAME}.              
                  APPLY "CLOSE":U TO THIS-PROCEDURE. 
               END.
               ELSE DO:
                  MESSAGE "Ni har möjlighet att byta leverantör till " + levtemp.LEVNAMN + ". Den artikel som då kommer att beställas är " + LC(Guru.Konstanter:genk) + ":"
                  + emtrltemp.ENR + " benämning:" + SUBSTRING(emtrltemp.BENAMNING,1,20) + " enhet:" +
                  emtrltemp.ENHET + " pris/enhet:" + STRING(emtrltemp.NPRIS) + ". Är detta korrekt?"
                  VIEW-AS ALERT-BOX
                  QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
                  IF svar THEN DO:
                     FIND FIRST bestbuff WHERE bestbuff.ENR = emtrltemp.ENR AND
                     bestbuff.LEVKOD = emtrltemp.LEVKOD 
                     NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE bestbuff THEN DO:
                        ASSIGN
                        spec_mtrl.ENR = emtrltemp.ENR
                        spec_mtrl.BENAMNING = emtrltemp.BENAMNING
                        spec_mtrl.ENHET = emtrltemp.ENHET
                        spec_mtrl.NPRIS = emtrltemp.NPRIS
                        spec_mtrl.BPRIS = emtrltemp.BPRIS
                        spec_mtrl.LEVKOD = emtrltemp.LEVKOD.
                     END.
                     ELSE DO:
                        bestbuff.BERKVANT = bestbuff.BERKVANT + spec_mtrl.BERKVANT.
                        DELETE spec_mtrl.
                     END.   
                     APPLY "GO" TO FRAME {&FRAME-NAME}.              
                     APPLY "CLOSE":U TO THIS-PROCEDURE.
                  END.
               END.
            END.
            ELSE DO:
               FIND FIRST emtrltemp WHERE emtrltemp.LEVKOD = levtemp.LEVKOD AND   
               emtrltemp.KALKNR = 0 AND emtrltemp.ENR = "E" + spec_mtrl.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
               IF NOT AVAILABLE emtrltemp THEN DO:
                  MESSAGE "Det finns ingen artikel hos " + levtemp.LEVNAMN + " som matchar " + spec_mtrl.ENR +
                  ". Vill Ni göra bytet ändå?"
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.
                  IF svar THEN DO:                  
                     FIND FIRST bestbuff WHERE bestbuff.ENR = spec_mtrl.ENR AND
                     bestbuff.LEVKOD = levtemp.LEVKOD NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE bestbuff THEN DO:                     
                        spec_mtrl.LEVKOD = levtemp.LEVKOD.
                     END. 
                     ELSE DO:
                        bestbuff.BERKVANT = bestbuff.BERKVANT + spec_mtrl.BERKVANT.
                        DELETE spec_mtrl.
                     END.
                  END.
                  APPLY "GO" TO FRAME {&FRAME-NAME}.              
                  APPLY "CLOSE":U TO THIS-PROCEDURE.
               END.
               ELSE DO:
                  MESSAGE "Ni har möjlighet att byta leverantör till " + levtemp.LEVNAMN + ". Den artikel som då kommer att beställas är " + LC(Guru.Konstanter:genk) + ":"
                  + emtrltemp.ENR + " benämning:" + SUBSTRING(emtrltemp.BENAMNING,1,20) + " enhet:" +
                  emtrltemp.ENHET + " pris/enhet:" + STRING(emtrltemp.NPRIS) + ". Är detta korrekt?"
                  VIEW-AS ALERT-BOX
                  QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
                  IF svar THEN DO:
                     FIND FIRST bestbuff WHERE bestbuff.ENR = emtrltemp.ENR AND
                     bestbuff.LEVKOD = emtrltemp.LEVKOD NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE bestbuff THEN DO:
                        ASSIGN
                        spec_mtrl.ENR = emtrltemp.ENR
                        spec_mtrl.BENAMNING = emtrltemp.BENAMNING
                        spec_mtrl.ENHET = emtrltemp.ENHET
                        spec_mtrl.NPRIS = emtrltemp.NPRIS
                        spec_mtrl.BPRIS = emtrltemp.BPRIS
                        spec_mtrl.LEVKOD = emtrltemp.LEVKOD.
                     END. 
                     ELSE DO:
                        bestbuff.BERKVANT = bestbuff.BERKVANT + spec_mtrl.BERKVANT.
                        DELETE spec_mtrl.
                     END.  
                     APPLY "GO" TO FRAME {&FRAME-NAME}.              
                     APPLY "CLOSE":U TO THIS-PROCEDURE.
                  END.
               END.
            END.
         END.
         ELSE DO:
            MESSAGE "Ni har möjlighet att byta leverantör till " + levtemp.LEVNAMN + ". Den artikel som då kommer att beställas är " + LC(Guru.Konstanter:genk) + ":"
            + emtrltemp.ENR + " benämning:" + SUBSTRING(emtrltemp.BENAMNING,1,20) + " enhet:" +
            emtrltemp.ENHET + " pris/enhet:" + STRING(emtrltemp.NPRIS) + ". Är detta korrekt?"
            VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL.         
            IF svar THEN DO:
               FIND FIRST bestbuff WHERE bestbuff.ENR = emtrltemp.ENR AND
               bestbuff.LEVKOD = emtrltemp.LEVKOD 
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE bestbuff THEN DO:
                  ASSIGN
                  spec_mtrl.ENR = emtrltemp.ENR
                  spec_mtrl.BENAMNING = emtrltemp.BENAMNING
                  spec_mtrl.ENHET = emtrltemp.ENHET
                  spec_mtrl.NPRIS = emtrltemp.NPRIS
                  spec_mtrl.BPRIS = emtrltemp.BPRIS
                  spec_mtrl.LEVKOD = emtrltemp.LEVKOD.
               END.
               ELSE DO:
                  bestbuff.BERKVANT = bestbuff.BERKVANT + spec_mtrl.BERKVANT.
                  DELETE spec_mtrl.
               END.   
               APPLY "GO" TO FRAME {&FRAME-NAME}.              
               APPLY "CLOSE":U TO THIS-PROCEDURE.
            END.
         END.      
      END. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEVFR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEVFR WINDOW-2
ON VALUE-CHANGED OF CMB_LEVFR IN FRAME FRAME-B /* Från leverantör */
DO:                                 
   lev1 = INPUT CMB_LEVFR.   
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev1 
   USE-INDEX LEV NO-LOCK NO-ERROR.       
   EMPTY TEMP-TABLE emtrltemp NO-ERROR.
   RUN mtrllevhmt_UI IN mtrlbapph (INPUT levtemp.LEVKOD ,INPUT TABLE spec_mtrl ,OUTPUT TABLE emtrltemp).             
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEVTILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEVTILL WINDOW-2
ON VALUE-CHANGED OF CMB_LEVTILL IN FRAME FRAME-B /* Till leverantör */
DO:                                 
   lev2 = INPUT CMB_LEVTILL.   
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev2
   USE-INDEX LEV NO-LOCK NO-ERROR.       
   EMPTY TEMP-TABLE emtrltemp NO-ERROR. 
   RUN mtrllevhmt_UI IN mtrlbapph (INPUT levtemp.LEVKOD ,INPUT TABLE spec_mtrl, OUTPUT TABLE emtrltemp).             

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
   RUN disable_UI.
END.
/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:  
   {WIN_M_START.I}
   {ALLSTARTDYN.I}
   {muswait.i}   
   EMPTY TEMP-TABLE emtrltemp NO-ERROR.
   IF mtrl_rowid = ? THEN DO:
      FOR EACH levtemp WHERE levtemp.LEVKOD NE "0" AND 
      levtemp.LEVKOD NE "99" AND 
      levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
         ASSIGN
         status-ok = CMB_LEVTILL:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
      END. 
      IF Guru.Konstanter:globforetag = "GKAL" THEN DO:         
         FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev  NO-LOCK NO-ERROR.
         CMB_LEVTILL:SCREEN-VALUE = levtemp.LEVNAMN.
      END.   
      ELSE DO:   
         FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0" AND 
         levtemp.LEVKOD NE "99" AND 
         levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
         CMB_LEVTILL:SCREEN-VALUE = levtemp.LEVNAMN.
      END.   
   END.
   ELSE DO:   
      FIND spec_mtrl WHERE ROWID(spec_mtrl) = mtrl_rowid NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN-ENR:LABEL = Guru.Konstanter:genk
      FILL-IN-ENR = spec_mtrl.ENR
      FILL-IN-BEN = spec_mtrl.BENAMNING
      FILL-IN-ENHET = spec_mtrl.ENHET.      
      FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0" AND 
      levtemp.LEVKOD NE "99" AND levtemp.LEVKOD NE spec_mtrl.LEVKOD AND
      levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
      vald_kundlev = levtemp.LEVKOD.  
      FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD
      USE-INDEX LEV NO-LOCK NO-ERROR.       
      ASSIGN
      status-ok = CMB_LEVFR:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
      CMB_LEVFR:SCREEN-VALUE = levtemp.LEVNAMN.        
      
      FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
      USE-INDEX LEV NO-LOCK NO-ERROR.
      ASSIGN
      status-ok = CMB_LEVTILL:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}    
      CMB_LEVTILL:SCREEN-VALUE = levtemp.LEVNAMN.           
      RUN mtrllevhmt_UI IN mtrlbapph (INPUT levtemp.LEVKOD ,INPUT TABLE spec_mtrl, OUTPUT TABLE emtrltemp).             
      FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
      levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99" AND 
      levtemp.LEVKOD NE spec_mtrl.LEVKOD AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
         status-ok = CMB_LEVTILL:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
      END.  
   END.
   ASSIGN         
   {&WINDOW-NAME}:TITLE = "Byte av leverantör"      
   status-ok = CMB_LEVFR:DELETE("0")
   status-ok = CMB_LEVTILL:DELETE("0"). 
                
   ASSIGN     
   CMB_LEVFR = INPUT CMB_LEVFR
   CMB_LEVTILL = INPUT CMB_LEVTILL
   lev1 = INPUT CMB_LEVFR
   lev2 = INPUT CMB_LEVTILL.
   RUN enable_UI.   
   IF mtrl_rowid = ? THEN DO:      
      ASSIGN
      FILL-IN-ENR:HIDDEN = TRUE
      FILL-IN-BEN:HIDDEN = TRUE
      FILL-IN-ENHET:HIDDEN = TRUE
      CMB_LEVFR:HIDDEN = TRUE.
   END.
   {FRMSIZE.I}  
   {musarrow.i}
   {WIN_M_SLUT.I}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  DISPLAY CMB_LEVFR CMB_LEVTILL FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE CMB_LEVTILL BTN_BEST BTN_AVSL FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

