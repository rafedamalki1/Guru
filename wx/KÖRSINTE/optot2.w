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
DEFINE INPUT PARAMETER mtrl_recid AS ROWID NO-UNDO.
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}

DEFINE SHARED VARIABLE kundoffproch AS HANDLE NO-UNDO. /* KUNDOFFAPP.P */
DEFINE SHARED VARIABLE levapph AS HANDLE NO-UNDO.     /*LEVAPP.P*/
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE valkalknr AS INTEGER  NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER  NO-UNDO.
DEFINE SHARED VARIABLE offe AS LOGICAL NO-UNDO.
DEFINE VARIABLE rabatt AS DECIMAL FORMAT "->9.99" NO-UNDO. 
DEFINE VARIABLE tot AS DECIMAL FORMAT "->9.99" NO-UNDO. 
DEFINE VARIABLE sum AS DECIMAL FORMAT "->9.99" NO-UNDO.    
DEFINE VARIABLE nettooff AS DECIMAL FORMAT "->9.99" NO-UNDO.   
DEFINE VARIABLE offlev AS CHARACTER  NO-UNDO. 
DEFINE VARIABLE ett AS DECIMAL NO-UNDO. 
DEFINE VARIABLE tva AS DECIMAL NO-UNDO. 
{SOKMTRL.I}
&Scoped-define SHARED SHARED
{LEVTEMP.I}

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
BTN_TABORT BTN_AVB FILL-IN-LEV FILL-IN-BEN FILL-IN-PRIS FILL-IN-NPRIS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-RABATT FILL-IN-SUMMA FILL-IN-LEV ~
FILL-IN-BEN FILL-IN-PRIS FILL-IN-NPRIS 

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
     SIZE 12 BY 1.

DEFINE BUTTON BTN_TABORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(25)":U 
     LABEL "Leverantör" 
      VIEW-AS TEXT 
     SIZE 33 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEV AS CHARACTER FORMAT "X(4)":U 
     LABEL "Lev-id" 
      VIEW-AS TEXT 
     SIZE 8.88 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-NPRIS AS DECIMAL FORMAT "->>>>>9.99":U INITIAL 0 
     LABEL "Nettopris" 
      VIEW-AS TEXT 
     SIZE 12.38 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT "->>>>>9.99":U INITIAL 0 
     LABEL "Listpris" 
      VIEW-AS TEXT 
     SIZE 12.38 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-RABATT AS DECIMAL FORMAT "->9.99":U INITIAL 0 
     LABEL "Rabatt i %" 
     VIEW-AS FILL-IN 
     SIZE 12.38 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-SUMMA AS DECIMAL FORMAT "->>>>>9.99":U INITIAL 0 
     LABEL "Totalt" 
     VIEW-AS FILL-IN 
     SIZE 12.38 BY .75 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     FILL-IN-RABATT AT ROW 6.13 COL 12.5 COLON-ALIGNED
     FILL-IN-SUMMA AT ROW 7.63 COL 12.5 COLON-ALIGNED
     BTN_OK AT ROW 9.04 COL 4.25
     BTN_TABORT AT ROW 9.04 COL 18.63
     BTN_AVB AT ROW 9.04 COL 36.5
     FILL-IN-LEV AT ROW 1.63 COL 12.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.13 COL 12.5 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 4.63 COL 12.5 COLON-ALIGNED
     FILL-IN-NPRIS AT ROW 4.63 COL 36.13 COLON-ALIGNED
     SPACE(0.73) SKIP(5.07)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Offertpris totalt".


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
       FILL-IN-BEN:HIDDEN IN FRAME DIALOG-2           = TRUE.

ASSIGN 
       FILL-IN-LEV:HIDDEN IN FRAME DIALOG-2           = TRUE.

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
      off_mtrl.TOTALT = ett 
      off_mtrl.RABTOT = tva.
   END.   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-2
ON CHOOSE OF BTN_OK IN FRAME DIALOG-2 /* Skapa */
DO: 
   IF offe = FALSE THEN DO: 
      IF musz = TRUE THEN DO:
         CREATE off_mtrl.
         ASSIGN
         off_mtrl.LEVKOD = vald_lev 
         off_mtrl.KALKNR = valkalknr                     
         off_mtrl.TOTALT = INPUT FILL-IN-PRIS
         off_mtrl.RABTOT = INPUT FILL-IN-SUMMA.
      END.
      ELSE DO:
         CREATE off_mtrl.
         ASSIGN        
         off_mtrl.KALKNR = valkalknr                     
         off_mtrl.TOTALT = INPUT FILL-IN-PRIS
         off_mtrl.RABTOT = INPUT FILL-IN-SUMMA. 
      END.     
   END.
   ELSE DO:
      ASSIGN
      off_mtrl.TOTALT = INPUT FILL-IN-PRIS
      off_mtrl.RABTOT = INPUT FILL-IN-SUMMA. 
   END.                                        
            
   APPLY "GO" TO FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TABORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TABORT DIALOG-2
ON CHOOSE OF BTN_TABORT IN FRAME DIALOG-2 /* Ta bort */
DO:
   IF musz = TRUE THEN DO: 
      FOR EACH off_mtrl WHERE off_mtrl.LEVKOD = vald_lev.
         DELETE off_mtrl.
      END.
   END.   
   ELSE DO:
      EMPTY TEMP-TABLE off_mtrl NO-ERROR.       
   END.               
   APPLY "GO" TO FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-RABATT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-RABATT DIALOG-2
ON LEAVE OF FILL-IN-RABATT IN FRAME DIALOG-2 /* Rabatt i % */
DO: 
   IF offe = FALSE THEN DO:
      ASSIGN 
      rabatt = INPUT FILL-IN-RABATT
      tot =  INPUT FILL-IN-PRIS * ((100 - rabatt) / 100)
      FILL-IN-RABATT = rabatt
      FILL-IN-SUMMA = tot.
   END.
   ELSE DO:
      ASSIGN 
      rabatt = INPUT FILL-IN-RABATT
      off_mtrl.RABTOT =  off_mtrl.TOTALT * ((100 - rabatt) / 100)
      FILL-IN-RABATT = rabatt
      FILL-IN-SUMMA = off_mtrl.RABTOT.
   END.      
   DISPLAY FILL-IN-SUMMA WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SUMMA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SUMMA DIALOG-2
ON LEAVE OF FILL-IN-SUMMA IN FRAME DIALOG-2 /* Totalt */
DO: 
   IF offe = FALSE THEN DO:
      ASSIGN 
      tot = INPUT FILL-IN-SUMMA
      rabatt = 100 - ((tot / INPUT FILL-IN-PRIS) * 100)
      FILL-IN-RABATT = rabatt
      FILL-IN-SUMMA = tot.
   END.
   ELSE DO:
      ASSIGN 
      off_mtrl.RABTOT = INPUT FILL-IN-SUMMA
      rabatt = 100 - ((off_mtrl.RABTOT / off_mtrl.TOTALT) * 100)
      FILL-IN-RABATT = rabatt
      FILL-IN-SUMMA = off_mtrl.RABTOT.
   END.      
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
   FIND FIRST levtemp NO-ERROR.
   IF NOT AVAILABLE levtemp THEN DO:
      RUN levhmt_UI IN levapph (OUTPUT TABLE levtemp).             
   END.
   IF offe = FALSE THEN DO:
      IF musz = TRUE THEN DO:
         FIND bmtrl_mtrl WHERE ROWID(bmtrl_mtrl) = mtrl_recid NO-ERROR.        
         FIND FIRST levtemp WHERE levtemp.LEVKOD = bmtrl_mtrl.LEVKOD NO-LOCK NO-ERROR. 
         ASSIGN
         FILL-IN-LEV = bmtrl_mtrl.LEV
         FILL-IN-BEN = levtemp.LEVNAMN       
         sum = 0
         tot = 0.
         FOR EACH bmtrl_mtrl WHERE bmtrl_mtrl.LEVKOD = vald_lev.
            ASSIGN
            sum = sum + (bmtrl_mtrl.BPRIS * bmtrl_mtrl.BERKVANT)
            tot = tot + bmtrl_mtrl.SUMMA.
         END.         
         ASSIGN   
         FILL-IN-PRIS = sum      
         FILL-IN-SUMMA = tot
         FILL-IN-RABATT = 100 - ((tot / sum) * 100).
      END.
      ELSE DO:                                      
         FIND kund_mtrl WHERE ROWID(kund_mtrl) = mtrl_recid NO-ERROR. 
         ASSIGN  
         nettooff = 0      
         sum = 0
         tot = 0.
         FOR EACH kund_mtrl.
            ASSIGN
            sum = sum + (kund_mtrl.KPRIS * kund_mtrl.BERKVANT)
            tot = tot + kund_mtrl.SUMMA.
         END.         
         ASSIGN   
         FILL-IN-PRIS = sum      
         FILL-IN-SUMMA = tot
         FILL-IN-RABATT = 100 - ((tot / sum) * 100).
         EMPTY TEMP-TABLE off_sum NO-ERROR.                  
         RUN offsum_UI IN kundoffproch (INPUT valkalknr,OUTPUT nettooff,OUTPUT TABLE off_sum).                         
         FOR EACH off_sum.
            nettooff = nettooff + off_sum.SUMMA.
         END.
         FILL-IN-NPRIS = nettooff.          
      END.
   END.   
   ELSE DO:   
      IF musz = TRUE THEN DO:
         FIND off_mtrl WHERE ROWID(off_mtrl) = mtrl_recid NO-ERROR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = off_mtrl.LEVKOD NO-LOCK NO-ERROR. 
         ASSIGN
         FILL-IN-LEV = off_mtrl.LEV
         FILL-IN-BEN = levtemp.LEVNAMN 
         tot = 0.      
         ASSIGN   
         FILL-IN-PRIS = off_mtrl.TOTALT
         FILL-IN-SUMMA = off_mtrl.RABTOT
         FILL-IN-RABATT = 100 - ((off_mtrl.RABTOT / off_mtrl.TOTALT) * 100)
         ett = off_mtrl.TOTALT
         tva = off_mtrl.RABTOT.
      END.
      ELSE DO:
         FIND off_mtrl WHERE ROWID(off_mtrl) = mtrl_recid NO-ERROR.         
         tot = 0.      
         ASSIGN   
         FILL-IN-PRIS = off_mtrl.TOTALT
         FILL-IN-SUMMA = off_mtrl.RABTOT
         FILL-IN-RABATT = 100 - ((off_mtrl.RABTOT / off_mtrl.TOTALT) * 100)
         ett = off_mtrl.TOTALT
         tva = off_mtrl.RABTOT.
         EMPTY TEMP-TABLE off_sum NO-ERROR.                  
         RUN offsum_UI IN kundoffproch (INPUT valkalknr,OUTPUT nettooff,OUTPUT TABLE off_sum).                         
         FOR EACH off_sum.
            nettooff = nettooff + off_sum.SUMMA.
         END.
         FILL-IN-NPRIS = nettooff.          
      END.
   END.         
  RUN enable_UI.       
   {FRMSIZED.I}
  IF musz = TRUE THEN DO:
     ASSIGN       
     FILL-IN-BEN:HIDDEN = FALSE
     FILL-IN-LEV:HIDDEN = FALSE
     FILL-IN-NPRIS:HIDDEN = TRUE.
  END.
  ELSE DO: 
     ASSIGN
     FILL-IN-BEN:HIDDEN = TRUE
     FILL-IN-LEV:HIDDEN = TRUE
     FILL-IN-NPRIS:HIDDEN = FALSE.
  END.           
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
  DISPLAY FILL-IN-RABATT FILL-IN-SUMMA FILL-IN-LEV FILL-IN-BEN FILL-IN-PRIS 
          FILL-IN-NPRIS 
      WITH FRAME DIALOG-2.
  ENABLE FILL-IN-RABATT FILL-IN-SUMMA BTN_OK BTN_TABORT BTN_AVB FILL-IN-LEV 
         FILL-IN-BEN FILL-IN-PRIS FILL-IN-NPRIS 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

