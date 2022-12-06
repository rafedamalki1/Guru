&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER filnamn AS CHARACTER NO-UNDO.   
DEFINE INPUT PARAMETER globforetag AS CHARACTER NO-UNDO.   
/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE TEMP-TABLE vartemp
   FIELD VKATALOG  AS LOGICAL
   FIELD VTYPVAR   AS INTEGER
   FIELD VARVAR    AS INTEGER
   FIELD VEBRMONT  AS DECIMAL
   FIELD VEBRMASK1 AS DECIMAL
   FIELD VEBRMASK2 AS DECIMAL
   FIELD VEBRMASK3 AS DECIMAL
   FIELD VRORLIG   AS DECIMAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-VITKAT CMB_KTYP FILL-IN-AR ~
FILL-IN-EBRMONT FILL-IN-MASK1 FILL-IN-MASK2 FILL-IN-MASK3 FILL-IN-ROR ~
Btn_OK Btn_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-VITKAT CMB_KTYP FILL-IN-AR ~
FILL-IN-EBRMONT FILL-IN-MASK1 FILL-IN-MASK2 FILL-IN-MASK3 FILL-IN-ROR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE CMB_KTYP AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Katalog typ" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1","2","3" 
     DROP-DOWN-LIST
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Katalog år" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-EBRMONT AS DECIMAL FORMAT "->>>>>9.99":U INITIAL 0 
     LABEL "Montage pris" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MASK1 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Maskin 1 pris" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MASK2 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Maskin 2 pris" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MASK3 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Maskin 3 pris" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ROR AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Rörlig kostnad" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VITKAT AS LOGICAL FORMAT "Vita/Gröna":U INITIAL NO 
     LABEL "Gröna eller vita katalogen" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-VITKAT AT ROW 3.75 COL 27.13 COLON-ALIGNED
     CMB_KTYP AT ROW 5.25 COL 27.13 COLON-ALIGNED
     FILL-IN-AR AT ROW 6.88 COL 27.13 COLON-ALIGNED
     FILL-IN-EBRMONT AT ROW 8.42 COL 27.13 COLON-ALIGNED
     FILL-IN-MASK1 AT ROW 9.96 COL 27.13 COLON-ALIGNED
     FILL-IN-MASK2 AT ROW 11.5 COL 27.13 COLON-ALIGNED
     FILL-IN-MASK3 AT ROW 12.75 COL 27.13 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-ROR AT ROW 13.92 COL 27.13 COLON-ALIGNED
     Btn_OK AT ROW 16.08 COL 13.13
     Btn_AVB AT ROW 16.08 COL 28.13
     "Ange följande värden:" VIEW-AS TEXT
          SIZE 27.5 BY 1.63 AT ROW 1.54 COL 1.5
          FONT 17
     SPACE(18.87) SKIP(15.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Inläsning av kataloger frå EBR"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Inläsning av kataloger frå EBR */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Ok */
DO:
   ASSIGN
   CMB_KTYP         = INPUT CMB_KTYP       
   FILL-IN-AR       = INPUT FILL-IN-AR     
   FILL-IN-EBRMONT  = INPUT FILL-IN-EBRMONT     
   FILL-IN-MASK1    = INPUT FILL-IN-MASK1  
   FILL-IN-MASK2    = INPUT FILL-IN-MASK2
   FILL-IN-MASK3    = INPUT FILL-IN-MASK3  
   FILL-IN-ROR      = INPUT FILL-IN-ROR    
   FILL-IN-VITKAT   = INPUT FILL-IN-VITKAT .
   CREATE vartemp.
   ASSIGN
   vartemp.VKATALOG   =   FILL-IN-VITKAT       
   vartemp.VTYPVAR    =   CMB_KTYP      
   vartemp.VARVAR     =   FILL-IN-AR 
   vartemp.VEBRMONT   =   FILL-IN-EBRMONT   
   vartemp.VEBRMASK1  =   FILL-IN-MASK1   
   vartemp.VEBRMASK2  =   FILL-IN-MASK2
   vartemp.VEBRMASK3  =   FILL-IN-MASK3     
   vartemp.VRORLIG    =   FILL-IN-ROR.  
   {muswait.i}
   RUN KALKINVN.P 
   (INPUT filnamn, INPUT Guru.Konstanter:globforetag,INPUT TABLE vartemp).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-VITKAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-VITKAT Dialog-Frame
ON LEAVE OF FILL-IN-VITKAT IN FRAME Dialog-Frame /* Gröna eller vita katalogen */
DO:
   FILL-IN-VITKAT = INPUT FILL-IN-VITKAT.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-VITKAT Dialog-Frame
ON MOUSE-SELECT-CLICK OF FILL-IN-VITKAT IN FRAME Dialog-Frame /* Gröna eller vita katalogen */
DO:
   IF INPUT FILL-IN-VITKAT = TRUE THEN FILL-IN-VITKAT = FALSE.
   IF INPUT FILL-IN-VITKAT = FALSE THEN FILL-IN-VITKAT = TRUE.
   DISPLAY FILL-IN-VITKAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   ASSIGN         
   CMB_KTYP   = 1       
   FILL-IN-AR = YEAR(TODAY) + 1    
   /*FILL-IN-EBRMONT  = 574.56    
   FILL-IN-MASK1    = 575
   FILL-IN-MASK2    = 535
   FILL-IN-ROR      = 307.8
   FILL-IN-EBRMONT  = 592.20    
   FILL-IN-MASK1    = 594
   FILL-IN-MASK2    = 553
   FILL-IN-ROR      = 317.2
   FILL-IN-EBRMONT  = 609.84    
   FILL-IN-MASK1    = 655
   FILL-IN-MASK2    = 582
   FILL-IN-ROR      = 326.7*/

   /*FILL-IN-EBRMONT  = 622.44    
   FILL-IN-MASK1    = 665
   FILL-IN-MASK2    = 601
   FILL-IN-ROR      = 333.4
   FILL-IN-VITKAT   = FALSE.
   FILL-IN-EBRMONT  = 637.56    
   FILL-IN-MASK1    = 684
   FILL-IN-MASK2    = 631
   FILL-IN-MASK3    = 663
   FILL-IN-ROR      = 341.55
   FILL-IN-EBRMONT  = 662.76    
   FILL-IN-MASK1    = 689
   FILL-IN-MASK2    = 634
   FILL-IN-MASK3    = 671
   FILL-IN-ROR      = 351.80*/
   
   /*FILL-IN-EBRMONT  = 685.44    
   FILL-IN-MASK1    = 716
   FILL-IN-MASK2    = 647
   FILL-IN-MASK3    = 676
   FILL-IN-ROR      = 367.2*/
   /*FILL-IN-EBRMONT  = 703.53    
   FILL-IN-MASK1    = 723
   FILL-IN-MASK2    = 712
   FILL-IN-MASK3    = 744
   FILL-IN-ROR      = 376.89*/
   /*FILL-IN-EBRMONT  = 713.16    
   FILL-IN-MASK1    = 726
   FILL-IN-MASK2    = 715
   FILL-IN-MASK3    = 747
   FILL-IN-ROR      = 382.05
   FILL-IN-EBRMONT  = 725.76    
   FILL-IN-MASK1    = 726
   FILL-IN-MASK2    = 715
   FILL-IN-MASK3    = 747
   FILL-IN-ROR      = 388.8*/
   
   FILL-IN-EBRMONT  = 756    
   FILL-IN-MASK1    = 748
   FILL-IN-MASK2    = 736
   FILL-IN-MASK3    = 769
   FILL-IN-ROR      = 405.
   
     
   /*ebrber  = 800.4
   ebrbervit  = 908.5      
   ebrmask1vit    = 858.7.*/
   
   FILL-IN-VITKAT   = FALSE.
   RUN enable_UI.       
   {FRMSIZED.I}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-VITKAT CMB_KTYP FILL-IN-AR FILL-IN-EBRMONT FILL-IN-MASK1 
          FILL-IN-MASK2 FILL-IN-MASK3 FILL-IN-ROR 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-VITKAT CMB_KTYP FILL-IN-AR FILL-IN-EBRMONT FILL-IN-MASK1 
         FILL-IN-MASK2 FILL-IN-MASK3 FILL-IN-ROR Btn_OK Btn_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

