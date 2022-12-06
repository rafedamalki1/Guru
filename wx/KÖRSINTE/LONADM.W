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

  Created: 96/01/28 -  6:17 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-25 SEL_UPP BTN_KOR BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS SEL_UPP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KOR 
     LABEL "Kör funktion" 
     SIZE 14 BY 1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 14.75
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 14.75
     BGCOLOR 8 .

DEFINE VARIABLE SEL_UPP AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 45 BY 12.88
     BGCOLOR 7 FGCOLOR 15 FONT 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     SEL_UPP AT ROW 1.5 COL 3 NO-LABEL
     BTN_KOR AT ROW 3 COL 52.13
     BTN_AVB AT ROW 12.96 COL 52.13
     RECT-2 AT ROW 1 COL 1
     RECT-25 AT ROW 1 COL 50
     SPACE(0.12) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Löneadministration".


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

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOR DIALOG-1
ON CHOOSE OF BTN_KOR IN FRAME DIALOG-1 /* Kör funktion */
DO:
   SEL_UPP = INPUT SEL_UPP.   
   RUN vart_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF SEL_UPP IN FRAME DIALOG-1
DO:
   SEL_UPP = INPUT SEL_UPP.     
   RUN vart_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP DIALOG-1
ON VALUE-CHANGED OF SEL_UPP IN FRAME DIALOG-1
DO:
   SEL_UPP = INPUT SEL_UPP. 
   IF SEL_UPP = "Leverantörsbyte på alla konstruktioner" THEN SEL_UPP:TOOLTIP = "Byte från en leverantör till en annan. Lista på artiklar som ej får träff hos den nya leverantören.".                
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
   ASSIGN
   RECT-2:BGCOLOR = ?
   RECT-25:BGCOLOR = ?  
   FRAME {&FRAME-NAME}:BGCOLOR = ? 
   SEL_UPP:FONT = Guru.Konstanter:varforetypval[20] 
   SEL_UPP:BGCOLOR = 15
   SEL_UPP:FGCOLOR = ?.

   status-ok = SEL_UPP:ADD-LAST("Deltidsfrånvaro") IN FRAME {&FRAME-NAME}.         
   IF Guru.Konstanter:globforetag = "GKAL" THEN.
   /*ELSE  status-ok = SEL_UPP:ADD-LAST("Senaste lönefil") IN FRAME {&FRAME-NAME}.*/    
   /*IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA"  THEN status-ok = SEL_UPP:ADD-LAST("Filöverföring") IN FRAME {&FRAME-NAME}.*/
      
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globforetag = "LULE" THEN status-ok = SEL_UPP:ADD-LAST("Stansunderlag") IN FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "GKAL" THEN status-ok = SEL_UPP:ADD-LAST("Årsuppdatering ATK och friskvårdsval") IN FRAME {&FRAME-NAME}.
     
    RUN enable_UI.       
      

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
  DISPLAY SEL_UPP 
      WITH FRAME DIALOG-1.
  ENABLE RECT-2 RECT-25 SEL_UPP BTN_KOR BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vart_UI DIALOG-1 
PROCEDURE vart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{muswait.i}
   IF SEL_UPP = "Skapa lönefil" THEN DO:
      IF Guru.Konstanter:globforetag = "NORD" OR Guru.Konstanter:globforetag = "ETA" OR Guru.Konstanter:globforetag = "ESMA" OR Guru.Konstanter:globforetag = "ESAN" OR  Guru.Konstanter:globforetag = "GKAL" THEN DO:
         MESSAGE "Denna funktion används ej." VIEW-AS ALERT-BOX.
      END.  
      ELSE IF Guru.Konstanter:globforetag = "GRAN"       THEN DO:
           RUN MANAD.W.
      END.   
   END.   
   IF SEL_UPP = "Deltidsfrånvaro" THEN DO:      
      RUN FRDEL.W.
   END.       
   IF SEL_UPP = "Fellista frånvaro" THEN DO:
      RUN FRFEL.W.
   END.   
  
   IF SEL_UPP = "Förskottsemester" THEN DO:
       RUN FRFOR.W.
   END.           
   /*IF SEL_UPP = "Senaste lönefil" THEN DO:
      IF Guru.Konstanter:globforetag = "SUND"  OR Guru.Konstanter:globforetag = "SNAT" THEN DO:    
         MESSAGE "Vill du se ELNÄT (JA) eller SEAB (NEJ) ?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val14 AS LOGICAL.        
         IF val14 = TRUE THEN RUN SELON.W.                  
         ELSE RUN SELONT.W.                      
      END.      
      ELSE IF Guru.Konstanter:globforetag = "LULE" THEN DO:    
         MESSAGE "Vill du se ELNÄT (JA) eller LEAB (NEJ) ?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val15 AS LOGICAL.        
         IF val15 = TRUE THEN RUN SELON.W.               
         ELSE RUN SELONT.W.                       
      END.              
      ELSE IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         MESSAGE "Vill du se Sydkraft Nät AB Syd (JA) eller Sydkraft Nät AB Norr-filen (NEJ) ?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val5 AS LOGICAL.        
         IF val5 = TRUE THEN  RUN SELONV.W.                 
         ELSE RUN SELONT.W.              
      END.        
      ELSE IF Guru.Konstanter:globforetag = "cGKAL" THEN DO:         
         MESSAGE "Vill du se Bruttoersättningar (JA) eller Frånvaro (NEJ) ?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val11 AS LOGICAL.        
         IF val11 = TRUE THEN DO:     
            MESSAGE "Vill se Bruttoersättningar för GKEAB (JA) eller för GSEAB (NEJ) ?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val12 AS LOGICAL.        
            IF val12 = TRUE THEN RUN SELON.W.                    
            ELSE RUN SELONT.W.                   
         END.   
         ELSE DO:  
            MESSAGE "Vill du se Frånvaro för GKEAB (JA) eller FÖR GSEAB (NEJ) ?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val13 AS LOGICAL.        
            IF val13 = TRUE THEN RUN SELONV.W.                    
            ELSE RUN SELONF.W.                   
         END.   
      END.                    
      ELSE IF Guru.Konstanter:globforetag = "VATT"  OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
         MESSAGE "Vill du se tidfilen (JA) eller tilläggsfilen (NEJ) ?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val4 AS LOGICAL.        
         IF val4 = TRUE THEN  RUN SELON.W.             
         ELSE RUN SELONT.W.           
      END.
      ELSE IF Guru.Konstanter:globforetag = "ETA"  OR Guru.Konstanter:globforetag = "ESMA" OR Guru.Konstanter:globforetag = "ESAN" THEN DO:
         MESSAGE "Vill du se ej överförd lönefil (JA) eller senaste redan överförda lönefil (NEJ) ?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val7 AS LOGICAL.        
         IF val7 = TRUE THEN RUN SELON.W.             
         ELSE RUN SELONT.W.           
      END.
      ELSE RUN SELON.W.             
   END.    
   IF SEL_UPP = "Visa sjukfrånvaro" THEN DO:
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         MESSAGE "Vill du se Nordfil (JA) eller Sydfilen (NEJ) ?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val20 AS LOGICAL.        
         IF val20 = TRUE THEN DO:  
            MESSAGE "Vill du se kollektivfilen (JA) eller tjänstemannafilen (NEJ) ?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.        
            IF val = TRUE THEN RUN SESJUK.W.                     
            ELSE RUN SESJUKT.W.                    
         END.
         ELSE RUN SESJUKS.W.               
      END.
      ELSE IF Guru.Konstanter:globforetag = "GADM"   THEN DO:
         MESSAGE "Vill du se kollektivfilen (JA) eller tjänstemannafilen (NEJ) ?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val21 AS LOGICAL.        
         IF val21 = TRUE THEN RUN SESJUK.W.                 
         ELSE  RUN SESJUKT.W.                    
      END.
      ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         MESSAGE "Vill du se kalmarfilen (JA) eller småland-filen (NEJ) ?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val6 AS LOGICAL.        
         IF val6 = TRUE THEN RUN SESJUK.W.               
         ELSE RUN SESJUKT.W.                
      END.                                      
      ELSE RUN SESJUK.W.        
   END.     
   IF SEL_UPP = "Gammal frånvaro" THEN DO:
      RUN GFRAN.W.
   END.
        
   IF SEL_UPP = "Stansunderlag" THEN DO:     
      RUN STANSFIL.W.           
   END.   
   */
   IF SEL_UPP = "Årsuppdatering ATK och friskvårdsval" THEN DO:   
   /*   IF MONTH(TODAY) GE 10 THEN DO:*/
  
      RUN GODATK.W (INPUT 2).
   
  /*END.   */
   END.
   {musarrow.i} 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

