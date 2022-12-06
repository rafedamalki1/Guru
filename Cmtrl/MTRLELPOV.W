&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 03/30/97 -  8:41 pm

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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}

DEFINE NEW SHARED VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE leverant AS CHARACTER NO-UNDO.


DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL INITIAL TRUE NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE tvar AS CHARACTER EXTENT 50 NO-UNDO.
DEFINE VARIABLE vad AS INTEGER NO-UNDO.
DEFINE VARIABLE sokfil AS CHARACTER.
DEFINE TEMP-TABLE felmex   
   FIELD ENR                AS CHARACTER 
   FIELD BENAMNING          AS CHARACTER 
   FIELD ENHET              AS CHARACTER
   FIELD BPRIS              AS DECIMAL
   FIELD NPRIS              AS DECIMAL
   INDEX ENR IS PRIMARY ENR.
   
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}
IF Guru.Konstanter:appcon THEN DO:
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
END.
ELSE DO:
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
END.
   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-C

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 SEL_UPP BTN_KOR BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS SEL_UPP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-3 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY DEFAULT 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_KOR 
     LABEL "Kör funktion" 
     SIZE 14 BY 1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 18.75
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 18.75
     BGCOLOR 8 .

DEFINE VARIABLE SEL_UPP AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 62.38 BY 17.46
     BGCOLOR 7 FGCOLOR 15 FONT 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-C
     SEL_UPP AT ROW 1.5 COL 5.38 NO-LABEL
     BTN_KOR AT ROW 3 COL 73.88
     BTN_AVB AT ROW 17.25 COL 73.88
     RECT-2 AT ROW 1 COL 1
     RECT-3 AT ROW 1 COL 72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.13 BY 18.88
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = "Specialprogram"
         HEIGHT             = 18.88
         WIDTH              = 89.5
         MAX-HEIGHT         = 24.42
         MAX-WIDTH          = 108.13
         VIRTUAL-HEIGHT     = 24.42
         VIRTUAL-WIDTH      = 108.13
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
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
/* SETTINGS FOR WINDOW WINDOW-3
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-C
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_KOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOR WINDOW-3
ON CHOOSE OF BTN_KOR IN FRAME FRAME-C /* Kör funktion */
DO:
   SEL_UPP = INPUT SEL_UPP.   
   RUN vart_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF SEL_UPP IN FRAME FRAME-C
DO:
   SEL_UPP = INPUT SEL_UPP.     
   RUN vart_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WINDOW-3
ON VALUE-CHANGED OF SEL_UPP IN FRAME FRAME-C
DO:
   SEL_UPP = INPUT SEL_UPP.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-3 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   
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
   ASSIGN
   RECT-2:BGCOLOR = ?
   RECT-3:BGCOLOR = ?  
   FRAME FRAME-C:BGCOLOR = ? 
   SEL_UPP:FONT = Guru.Konstanter:varforetypval[20] 
   SEL_UPP:BGCOLOR = 15
   SEL_UPP:FGCOLOR = ?.

   {muswait.i}
   {ALLSTARTDYN.I}
      
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globniv = 0 THEN DO:         
      status-ok = SEL_UPP:ADD-LAST("*******************Ny katalog*******************").
      status-ok = SEL_UPP:ADD-LAST("Dumpa materiel till .d fil").
      status-ok = SEL_UPP:ADD-LAST("Ta bort allt materiel för leverantör").
      status-ok = SEL_UPP:ADD-LAST("Läsa in materiel från .d fil").            
      /*status-ok = SEL_UPP:ADD-LAST("Uppdatera priser i adm.bered. för vald lev.").*/
      status-ok = SEL_UPP:ADD-LAST("Uppdatera priser i gjorda bered. för vald lev.").
      status-ok = SEL_UPP:ADD-LAST("Uppdatera satsers priser i bered. för vald lev.").    
      status-ok = SEL_UPP:ADD-LAST("Koll av upplägg efter ny katalog").
      status-ok = SEL_UPP:ADD-LAST("Koll av depåer efter ny katalog").      
      status-ok = SEL_UPP:ADD-LAST("Nattkörning av materielkatalog Elektroskandia").
      /*status-ok = SEL_UPP:ADD-LAST("Nattkörning av materielkatalog Onninen").  */    
      status-ok = SEL_UPP:ADD-LAST("*******************Materiel*********************").
      
      /*status-ok = SEL_UPP:ADD-LAST("Läsa in nettopriser Onninen").*/
      status-ok = SEL_UPP:ADD-LAST("Ta bort satsregister för leverantör").
      status-ok = SEL_UPP:ADD-LAST("Läsa in satser från .d fil").
      status-ok = SEL_UPP:ADD-LAST("Dumpa satser till .d fil").                      
      status-ok = SEL_UPP:ADD-LAST("Sätt alla priser = 0 för vald leverantör.").
      status-ok = SEL_UPP:ADD-LAST("Lägga till E i ENR-upplägg").
      status-ok = SEL_UPP:ADD-LAST("Lägga till E i ENR-katalog").
      status-ok = SEL_UPP:ADD-LAST("Ta bort E i ENR-upplägg").
      status-ok = SEL_UPP:ADD-LAST("Ta bort E i ENR-katalog").
      status-ok = SEL_UPP:ADD-LAST("Ta bort E i alla beredningar").                                                    
      
      status-ok = SEL_UPP:ADD-LAST("*******************Beredning********************").
      
      status-ok = SEL_UPP:ADD-LAST("Dumpa konstruktionsgrupp").
      status-ok = SEL_UPP:ADD-LAST("Ladda konstruktionsgrupp").
      status-ok = SEL_UPP:ADD-LAST("Ladda nya konstruktioner till befintlig konstruktionsgrupp").      
      status-ok = SEL_UPP:ADD-LAST("Dumpa konstruktion").
      status-ok = SEL_UPP:ADD-LAST("Ladda konstruktion").      
      status-ok = SEL_UPP:ADD-LAST("Ordning på konstval").
      /*
      status-ok = SEL_UPP:ADD-LAST("Jämförelse databaser").
      */            
                  
      status-ok = SEL_UPP:ADD-LAST("*******************Blandat**********************").                      
                              
      status-ok = SEL_UPP:ADD-LAST("Störningstab").            
      status-ok = SEL_UPP:ADD-LAST("******************* NY DATABAS*************").                      
      status-ok = SEL_UPP:ADD-LAST("Ta bort alla beredningar").    
      status-ok = SEL_UPP:ADD-LAST("Ta bort alla markvärderingar").
      status-ok = SEL_UPP:ADD-LAST("Ta bort alla depåer"). 
      status-ok = SEL_UPP:ADD-LAST("Ta bort alla kalkyler").      
   END.   
   
   RUN enable_UI.   
    {FRMSIZE.I} 
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-3  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
  THEN DELETE WIDGET WINDOW-3.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-3  _DEFAULT-ENABLE
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
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  ENABLE RECT-2 RECT-3 SEL_UPP BTN_KOR BTN_AVB 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inlas_UI WINDOW-3 
PROCEDURE inlas_UI :

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vart_UI WINDOW-3 
PROCEDURE vart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
  {AMERICANEUROPEAN.I}
   {muswait.i}
   
   
   IF SEL_UPP = "Läsa in materiel från .d fil" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN LADDMTRL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globforetag, INPUT leverant).
      END.
      ELSE DO:
         RUN LADDMTRL.P 
         (INPUT Guru.Konstanter:globforetag, INPUT leverant).
      END.  
                                                                           
   END.   
   ELSE IF SEL_UPP = "Dumpa materiel till .d fil" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:  
         RUN DUMPMTRL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globforetag, INPUT leverant).
      END.
      ELSE DO:
         RUN DUMPMTRL.P 
         (INPUT Guru.Konstanter:globforetag, INPUT leverant).
      END.                                                                          
   END.
   ELSE IF SEL_UPP = "Ta bort satsregister för leverantör" THEN DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN DELSATS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT Guru.Konstanter:globforetag, INPUT leverant).                                                                           
      END.
      ELSE DO:      
         RUN DELSATS.P 
         (INPUT Guru.Konstanter:globforetag, INPUT leverant).                                                                           
      END.
   END.
   ELSE IF SEL_UPP = "Läsa in satser från .d fil" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN LADDSATS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globforetag, INPUT leverant).
      END.
      ELSE DO:
         RUN LADDSATS.P 
         (INPUT Guru.Konstanter:globforetag, INPUT leverant).
      END.                                                                          
   END.
   ELSE IF SEL_UPP = "Dumpa satser till .d fil" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN DUMPSATS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globforetag, INPUT leverant).
      END.
      ELSE DO:
         RUN DUMPSATS.P 
         (INPUT Guru.Konstanter:globforetag, INPUT leverant).
      END.                                                                          
   END.
   ELSE IF SEL_UPP = "Ta bort allt materiel för leverantör" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN DELMTRL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT leverant).
      END.
      ELSE DO:
         RUN DELMTRL.P 
         (INPUT leverant).
      END.                                                                          
   END.
   ELSE IF SEL_UPP = "Uppdatera priser i adm.bered. för vald lev." THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN PRISUPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT leverant).
      END.
      ELSE DO:
         RUN PRISUPP.P 
         (INPUT leverant).
      END.                                                                          
   END.
   ELSE IF SEL_UPP = "Uppdatera priser i gjorda bered. för vald lev." THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN PRISUBER.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT leverant).
      END.
      ELSE DO:
         RUN PRISUBER.P 
         (INPUT leverant).
      END.                                                                          
   END.
   ELSE IF SEL_UPP = "Uppdatera satsers priser i bered. för vald lev." THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN PRISUPP6.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT leverant, INPUT Guru.Konstanter:globforetag).
      END.
      ELSE DO:
         RUN PRISUPP6.P 
         (INPUT leverant, INPUT Guru.Konstanter:globforetag).
      END.                                                                          
   END.   
   
   
   ELSE IF SEL_UPP = "Sätt alla priser = 0 för vald leverantör." THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN PRISUPP5.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT leverant).
      END.
      ELSE DO:
         RUN PRISUPP5.P 
         (INPUT leverant).
      END.                                                                          
   END.
   ELSE IF SEL_UPP = "Lägga till E i ENR-upplägg" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ENRELEF.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT leverant).
      END.
      ELSE DO:
         RUN ENRELEF.P 
         (INPUT leverant).
      END.                                                                          
   END.
   ELSE IF SEL_UPP = "Lägga till E i ENR-katalog" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ENRELEF2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT leverant).
      END.
      ELSE DO:
         RUN ENRELEF2.P 
         (INPUT leverant).
      END.                                                                          
   END.
   ELSE IF SEL_UPP = "Ta bort E i ENR-upplägg" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ENRELEFB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT leverant).
      END.
      ELSE DO:
         RUN ENRELEFB.P 
         (INPUT leverant).
      END.                                                                          
   END.
   ELSE IF SEL_UPP = "Ta bort E i ENR-katalog" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ENRELEFB2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT leverant).
      END.
      ELSE DO:
         RUN ENRELEFB2.P 
         (INPUT leverant).
      END.                                                                          
   END.
   ELSE IF SEL_UPP = "Ta bort E i alla beredningar" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ENRELEFB3.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT leverant).
      END.
      ELSE DO:
         RUN ENRELEFB3.P 
         (INPUT leverant).
      END.                                                                          
   END.

   
   ELSE IF SEL_UPP = "Koll av depåer efter ny katalog" THEN DO:       
      RUN KATKOLLV.W.
   END.
   
   ELSE IF SEL_UPP = "Dumpa konstruktionsgrupp" THEN DO:             
      RUN DUMPGRUPP1V.W (INPUT SEL_UPP).      
   END.
   ELSE IF SEL_UPP = "Ladda konstruktionsgrupp" THEN DO:       
      RUN DUMPGRUPP1V.W (INPUT SEL_UPP).
   END.
   ELSE IF SEL_UPP = "Ladda nya konstruktioner till befintlig konstruktionsgrupp" THEN DO:       
      RUN DUMPGRUPP1V.W (INPUT SEL_UPP).
   END.
   ELSE IF SEL_UPP = "Dumpa konstruktion" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN DUMPKON.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
         RUN DUMPKON.P.
      END.                                                                          
   END.
   ELSE IF SEL_UPP = "Ladda konstruktion" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN LADDKON.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globforetag).
      END.
      ELSE DO:
         RUN LADDKON.P 
         (INPUT Guru.Konstanter:globforetag).
      END.                                                                          
   END.   
     
  
   ELSE IF SEL_UPP = "Störningstab" THEN DO:             
      RUN XINLASTV.W.      
   END.
   
   ELSE IF SEL_UPP = "Ta bort alla beredningar" THEN DO: 
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN DELALLBER.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
         RUN DELALLBER.P.
      END.    
      MESSAGE "Klart!"
      VIEW-AS ALERT-BOX.         
   END.
    ELSE IF SEL_UPP = "Ta bort alla kalkyler" THEN DO: 
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN DELALLKALK.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
         RUN DELALLKALK.P.
      END.     
      MESSAGE "Klart!"
      VIEW-AS ALERT-BOX.       
   END.
   
   
   
   ELSE IF SEL_UPP = "Ta bort alla markvärderingar" THEN DO: 
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN DELALLMARK.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
         RUN DELALLMARK.P.
      END.  
      MESSAGE "Klart!"
      VIEW-AS ALERT-BOX.           
   END.
   ELSE IF SEL_UPP = "Ta bort alla depåer" THEN DO: 
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN DELALLDEP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
         RUN DELALLDEP.P.
      END.
      MESSAGE "Klart!"
      VIEW-AS ALERT-BOX.             
   END.
   
   
   ELSE IF SEL_UPP = "Ordning på konstval" THEN DO: 
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN XUPPBER.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
         RUN XUPPBER.P.
      END.            
   END.   
   ELSE IF SEL_UPP = "Nattkörning av materielkatalog Elektroskandia" THEN DO:
   /* 1 0 Onninen , 1 1 Elektroskandia*/
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN GRANAHLS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT 1, INPUT 1).
      END.
      ELSE DO:
         RUN GRANAHLS.P (INPUT 1, INPUT 1).
      END.
   END.
   /*ELSE IF SEL_UPP = "Nattkörning av materielkatalog Onninen" THEN DO:         
      /* 1 0 Onninen , 1 1 Elektroskandia*/
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN GRANAHLS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT 1, INPUT 0).
      END.
      ELSE DO:
         RUN GRANAHLS.P (INPUT 1, INPUT 0).
      END.
                        
   END.*/   
   
   /*ELSE IF SEL_UPP = "Läsa in nettopriser Onninen" THEN DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ELEF.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT leverant).
      END.
      ELSE DO:
         RUN ELEF.P 
         (INPUT leverant).
      END.   
        
   END.*/
   {EUROPEANAMERICAN.I}
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

