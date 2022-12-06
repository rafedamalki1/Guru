&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
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

  Created: 08/20/96 - 11:13 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE rabatt AS DECIMAL FORMAT "->9.99" NO-UNDO. 
DEFINE VARIABLE tot AS DECIMAL FORMAT "->9.99" NO-UNDO. 
DEFINE VARIABLE sum AS DECIMAL FORMAT "->9.99" NO-UNDO.    
DEFINE VARIABLE nettooff AS DECIMAL FORMAT "->9.99" NO-UNDO.   
DEFINE VARIABLE brutto AS DECIMAL FORMAT "->9.99" NO-UNDO. 
DEFINE VARIABLE offe AS LOGICAL NO-UNDO. 
DEFINE VARIABLE ett AS DECIMAL NO-UNDO. 
DEFINE VARIABLE tva AS DECIMAL NO-UNDO. 
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED
{DEPATEMP.I}
{SPECMTRLTEMP.I}
/*DEFINE SHARED TEMP-TABLE spec_mtrl    
    FIELD ENR LIKE MTRLDEP.ENR
    FIELD BENAMNING LIKE MTRLDEP.BENAMNING
    FIELD ENHET LIKE MTRLDEP.ENHET   
    FIELD BPRIS LIKE MTRLDEP.BPRIS       
    FIELD NPRIS LIKE MTRLDEP.NPRIS        
    FIELD LEVKOD LIKE MTRLDEP.LEVKOD
    FIELD MARK AS LOGICAL       
    FIELD SALDO LIKE MTRLDEP.SALDO.*/
       

DEFINE SHARED TEMP-TABLE off_mtrl NO-UNDO    
    FIELD TOTALT AS DECIMAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-2

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-RABATT FILL-IN-SUMMA BTN_OK ~
BTN_TABORT BTN_AVB FILL-IN-PRIS FILL-IN-NPRIS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-RABATT FILL-IN-SUMMA FILL-IN-PRIS ~
FILL-IN-NPRIS 

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

DEFINE BUTTON BTN_OK 
     LABEL "Skapa":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_TABORT 
     LABEL "Ta bort":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-NPRIS AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Nettopris" 
      VIEW-AS TEXT 
     SIZE 11.88 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Listpris" 
      VIEW-AS TEXT 
     SIZE 11.5 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-RABATT AS DECIMAL FORMAT "->>>9.99":U INITIAL 0 
     LABEL "Rabatt i %" 
     VIEW-AS FILL-IN 
     SIZE 12.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SUMMA AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Totalt" 
     VIEW-AS FILL-IN 
     SIZE 12.38 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     FILL-IN-RABATT AT ROW 3.96 COL 12.5 COLON-ALIGNED
     FILL-IN-SUMMA AT ROW 5.46 COL 12.5 COLON-ALIGNED
     BTN_OK AT ROW 7.75 COL 7.25
     BTN_TABORT AT ROW 7.75 COL 22.25
     BTN_AVB AT ROW 7.75 COL 37.25
     FILL-IN-PRIS AT ROW 2.46 COL 12.5 COLON-ALIGNED
     FILL-IN-NPRIS AT ROW 2.46 COL 35.63 COLON-ALIGNED
     SPACE(2.11) SKIP(6.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Totalt pris".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-2
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-2:SCROLLABLE       = FALSE
       FRAME DIALOG-2:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN-NPRIS:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-2
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-2 /* Avbryt */
DO:
   IF offe = FALSE THEN DO:
      offe = offe.
   END.
   ELSE DO:   
      ASSIGN
      off_mtrl.TOTALT = ett.
   END.   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-2
ON CHOOSE OF BTN_OK IN FRAME DIALOG-2 /* Skapa */
DO:     
   FIND FIRST off_mtrl EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO:
      off_mtrl.TOTALT = INPUT FILL-IN-SUMMA.
   END.
   ELSE DO:     
      CREATE off_mtrl.
      ASSIGN                             
      off_mtrl.TOTALT = INPUT FILL-IN-SUMMA.            
   END.   
   APPLY "GO" TO FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TABORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TABORT DIALOG-2
ON CHOOSE OF BTN_TABORT IN FRAME DIALOG-2 /* Ta bort */
DO:  
   EMPTY TEMP-TABLE off_mtrl NO-ERROR.   
   APPLY "GO" TO FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-RABATT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-RABATT DIALOG-2
ON LEAVE OF FILL-IN-RABATT IN FRAME DIALOG-2 /* Rabatt i % */
DO:   
   ASSIGN 
   rabatt = INPUT FILL-IN-RABATT
   tot =  INPUT FILL-IN-PRIS * ((100 - rabatt) / 100)
   FILL-IN-RABATT = rabatt
   FILL-IN-SUMMA = tot.     
   DISPLAY FILL-IN-SUMMA WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SUMMA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SUMMA DIALOG-2
ON LEAVE OF FILL-IN-SUMMA IN FRAME DIALOG-2 /* Totalt */
DO:    
   ASSIGN 
   tot = INPUT FILL-IN-SUMMA
   rabatt = 100 - ((tot / INPUT FILL-IN-PRIS) * 100)
   FILL-IN-RABATT = rabatt
   FILL-IN-SUMMA = tot.  
   DISPLAY FILL-IN-RABATT WITH FRAME {&FRAME-NAME}.
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
   ASSIGN  
   nettooff = 0      
   sum = 0
   tot = 0.                      
   FOR EACH spec_mtrl:                                   
      ASSIGN
      nettooff = nettooff + (spec_mtrl.NPRIS * spec_mtrl.SALDO)
      brutto = brutto + (spec_mtrl.BPRIS * spec_mtrl.SALDO).
   END. 
   FIND FIRST off_mtrl NO-LOCK NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO: 
      ASSIGN       
      sum = off_mtrl.TOTALT
      tot = brutto.       
      ASSIGN   
      FILL-IN-PRIS = tot     
      FILL-IN-SUMMA = sum            
      FILL-IN-RABATT = 100 - ((sum / tot) * 100).      
      FILL-IN-NPRIS = nettooff.      
   END.
   ELSE DO:  
      ASSIGN   
      sum = brutto
      tot = brutto.       
      ASSIGN   
      FILL-IN-PRIS = tot    
      FILL-IN-SUMMA = sum
      FILL-IN-RABATT = 100 - ((sum / tot) * 100).      
      FILL-IN-NPRIS = nettooff.                 
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
  DISPLAY FILL-IN-RABATT FILL-IN-SUMMA FILL-IN-PRIS FILL-IN-NPRIS 
      WITH FRAME DIALOG-2.
  ENABLE FILL-IN-RABATT FILL-IN-SUMMA BTN_OK BTN_TABORT BTN_AVB FILL-IN-PRIS 
         FILL-IN-NPRIS 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

