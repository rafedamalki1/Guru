&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE SHARED VARIABLE tanint AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE tanval AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE tanlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE del AS INTEGER NO-UNDO.
DEFINE VARIABLE fel AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 FILL-IN-1 BTN_DEL BUTTON-0 ~
BUTTON-1 BUTTON-2 BUTTON-3 BUTTON-4 BUTTON-5 BUTTON-6 BUTTON-7 BUTTON-8 ~
BUTTON-9 BUTTON-XXX Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 TOG_VAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_DEL 
     IMAGE-UP FILE "bilder\pvback":U
     LABEL "Delete" 
     SIZE 5 BY 1.13.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_SPACE 
     LABEL "Mellanslag" 
     SIZE 36.25 BY 1.

DEFINE BUTTON BUTTON-0 
     LABEL "0" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-1 
     LABEL "1" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-2 
     LABEL "2" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-3 
     LABEL "3" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-4 
     LABEL "4" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-5 
     LABEL "5" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-6 
     LABEL "6" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-7 
     LABEL "7" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-8 
     LABEL "8" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-9 
     LABEL "9" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-A 
     LABEL "A" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-AA 
     LABEL "Å" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-AAA 
     LABEL "Ä" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-B 
     LABEL "B" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-C 
     LABEL "C" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-D 
     LABEL "D" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-E 
     LABEL "E" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-F 
     LABEL "F" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-G 
     LABEL "G" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-H 
     LABEL "H" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-I 
     LABEL "I" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-J 
     LABEL "J" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-K 
     LABEL "K" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-L 
     LABEL "L" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-M 
     LABEL "M" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-N 
     LABEL "N" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-O 
     LABEL "O" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-OO 
     LABEL "Ö" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-P 
     LABEL "P" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-Q 
     LABEL "Q" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-R 
     LABEL "R" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-S 
     LABEL "S" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-T 
     LABEL "T" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-U 
     LABEL "U" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-V 
     LABEL "V" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-W 
     LABEL "W" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-X 
     LABEL "X" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-XX 
     LABEL "," 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-XXX 
     LABEL "." 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-XXXX 
     LABEL "-" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-Y 
     LABEL "Y" 
     SIZE 3 BY 1.

DEFINE BUTTON BUTTON-Z 
     LABEL "Z" 
     SIZE 3 BY 1.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39.88 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 2.17
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 1.92
     BGCOLOR 8 .

DEFINE VARIABLE TOG_VAL AS LOGICAL INITIAL no 
     LABEL "Versaler" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-1 AT ROW 1.92 COL 1.88 COLON-ALIGNED NO-LABEL
     TOG_VAL AT ROW 2.13 COL 46.88
     BTN_DEL AT ROW 3.83 COL 54.5
     BUTTON-0 AT ROW 3.96 COL 4.5
     BUTTON-1 AT ROW 3.96 COL 9.5
     BUTTON-2 AT ROW 3.96 COL 14.5
     BUTTON-3 AT ROW 3.96 COL 19.5
     BUTTON-4 AT ROW 3.96 COL 24.5
     BUTTON-5 AT ROW 3.96 COL 29.5
     BUTTON-6 AT ROW 3.96 COL 34.5
     BUTTON-7 AT ROW 3.96 COL 39.5
     BUTTON-8 AT ROW 3.96 COL 44.5
     BUTTON-9 AT ROW 3.96 COL 49.5
     BUTTON-A AT ROW 6.04 COL 4.5
     BUTTON-B AT ROW 6.04 COL 9.5
     BUTTON-C AT ROW 6.04 COL 14.5
     BUTTON-D AT ROW 6.04 COL 19.5
     BUTTON-E AT ROW 6.04 COL 24.5
     BUTTON-F AT ROW 6.04 COL 29.5
     BUTTON-G AT ROW 6.04 COL 34.5
     BUTTON-H AT ROW 6.04 COL 39.5
     BUTTON-I AT ROW 6.04 COL 44.5
     BUTTON-J AT ROW 6.04 COL 49.5
     BUTTON-K AT ROW 6.04 COL 54.5
     BUTTON-L AT ROW 7.83 COL 4.5
     BUTTON-M AT ROW 7.83 COL 9.5
     BUTTON-N AT ROW 7.83 COL 14.5
     BUTTON-O AT ROW 7.83 COL 19.5
     BUTTON-P AT ROW 7.83 COL 24.5
     BUTTON-Q AT ROW 7.83 COL 29.5
     BUTTON-R AT ROW 7.83 COL 34.5
     BUTTON-S AT ROW 7.83 COL 39.5
     BUTTON-T AT ROW 7.83 COL 44.5
     BUTTON-U AT ROW 7.83 COL 49.5
     BUTTON-V AT ROW 7.83 COL 54.5
     BUTTON-W AT ROW 9.54 COL 4.5
     BUTTON-X AT ROW 9.54 COL 9.5
     BUTTON-Y AT ROW 9.54 COL 14.5
     BUTTON-Z AT ROW 9.54 COL 19.5
     BUTTON-AA AT ROW 9.54 COL 24.5
     BUTTON-AAA AT ROW 9.54 COL 29.5
     BUTTON-OO AT ROW 9.54 COL 34.5
     BUTTON-XX AT ROW 9.54 COL 39.5
     BUTTON-XXXX AT ROW 9.54 COL 49.5
     BUTTON-XXX AT ROW 9.58 COL 44.5
     BTN_SPACE AT ROW 11.25 COL 12.63
     Btn_OK AT ROW 13.25 COL 46.75
     RECT-1 AT ROW 1.25 COL 1.75
     RECT-2 AT ROW 3.46 COL 1.75
     SPACE(0.99) SKIP(9.15)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Tangentbord"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BTN_SPACE IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-A IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-AA IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-AAA IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-B IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-C IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-D IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-E IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-F IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-G IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-H IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-I IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-J IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-K IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-L IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-M IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-N IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-O IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-OO IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-P IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Q IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-R IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-S IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-T IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-U IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-V IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-W IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-X IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-XX IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-XXXX IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Y IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Z IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOG_VAL IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Tangentbord */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_DEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_DEL Dialog-Frame
ON CHOOSE OF BTN_DEL IN FRAME Dialog-Frame /* Delete */
DO:   
   del = LENGTH(FILL-IN-1).
   IF del > 0 THEN DO:
      IF langd > 0 THEN
      langd = langd - 1.
      FILL-IN-1 = SUBSTRING(FILL-IN-1,1,del - 1).
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   tanval = INPUT FILL-IN-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SPACE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SPACE Dialog-Frame
ON CHOOSE OF BTN_SPACE IN FRAME Dialog-Frame /* Mellanslag */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      FILL-IN-1 = FILL-IN-1 + " ".            
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-0 Dialog-Frame
ON CHOOSE OF BUTTON-0 IN FRAME Dialog-Frame /* 0 */
DO:
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      FILL-IN-1 = FILL-IN-1 + "0".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 Dialog-Frame
ON CHOOSE OF BUTTON-1 IN FRAME Dialog-Frame /* 1 */
DO:
   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      FILL-IN-1 = FILL-IN-1 + "1".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 Dialog-Frame
ON CHOOSE OF BUTTON-2 IN FRAME Dialog-Frame /* 2 */
DO:
   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      FILL-IN-1 = FILL-IN-1 + "2".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 Dialog-Frame
ON CHOOSE OF BUTTON-3 IN FRAME Dialog-Frame /* 3 */
DO:
   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      FILL-IN-1 = FILL-IN-1 + "3".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 Dialog-Frame
ON CHOOSE OF BUTTON-4 IN FRAME Dialog-Frame /* 4 */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      FILL-IN-1 = FILL-IN-1 + "4".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 Dialog-Frame
ON CHOOSE OF BUTTON-5 IN FRAME Dialog-Frame /* 5 */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      FILL-IN-1 = FILL-IN-1 + "5".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 Dialog-Frame
ON CHOOSE OF BUTTON-6 IN FRAME Dialog-Frame /* 6 */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      FILL-IN-1 = FILL-IN-1 + "6".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 Dialog-Frame
ON CHOOSE OF BUTTON-7 IN FRAME Dialog-Frame /* 7 */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      FILL-IN-1 = FILL-IN-1 + "7".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 Dialog-Frame
ON CHOOSE OF BUTTON-8 IN FRAME Dialog-Frame /* 8 */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      FILL-IN-1 = FILL-IN-1 + "8".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 Dialog-Frame
ON CHOOSE OF BUTTON-9 IN FRAME Dialog-Frame /* 9 */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      FILL-IN-1 = FILL-IN-1 + "9".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-A Dialog-Frame
ON CHOOSE OF BUTTON-A IN FRAME Dialog-Frame /* A */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "A".
      ELSE FILL-IN-1 = FILL-IN-1 + "a".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-AA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-AA Dialog-Frame
ON CHOOSE OF BUTTON-AA IN FRAME Dialog-Frame /* Å */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "Å".
      ELSE FILL-IN-1 = FILL-IN-1 + "å".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-AAA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-AAA Dialog-Frame
ON CHOOSE OF BUTTON-AAA IN FRAME Dialog-Frame /* Ä */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "Ä".
      ELSE FILL-IN-1 = FILL-IN-1 + "ä".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-B
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-B Dialog-Frame
ON CHOOSE OF BUTTON-B IN FRAME Dialog-Frame /* B */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "B".
      ELSE FILL-IN-1 = FILL-IN-1 + "b".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-C
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-C Dialog-Frame
ON CHOOSE OF BUTTON-C IN FRAME Dialog-Frame /* C */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "C".
      ELSE FILL-IN-1 = FILL-IN-1 + "c".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-D
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-D Dialog-Frame
ON CHOOSE OF BUTTON-D IN FRAME Dialog-Frame /* D */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "D".
      ELSE FILL-IN-1 = FILL-IN-1 + "d".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-E
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-E Dialog-Frame
ON CHOOSE OF BUTTON-E IN FRAME Dialog-Frame /* E */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "E".
      ELSE FILL-IN-1 = FILL-IN-1 + "e".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-F
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-F Dialog-Frame
ON CHOOSE OF BUTTON-F IN FRAME Dialog-Frame /* F */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "F".
      ELSE FILL-IN-1 = FILL-IN-1 + "f".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-G
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-G Dialog-Frame
ON CHOOSE OF BUTTON-G IN FRAME Dialog-Frame /* G */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "G".
      ELSE FILL-IN-1 = FILL-IN-1 + "g".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-H
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-H Dialog-Frame
ON CHOOSE OF BUTTON-H IN FRAME Dialog-Frame /* H */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "H".
      ELSE FILL-IN-1 = FILL-IN-1 + "h".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-I
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-I Dialog-Frame
ON CHOOSE OF BUTTON-I IN FRAME Dialog-Frame /* I */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "I".
      ELSE FILL-IN-1 = FILL-IN-1 + "i".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-J
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-J Dialog-Frame
ON CHOOSE OF BUTTON-J IN FRAME Dialog-Frame /* J */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "J".
      ELSE FILL-IN-1 = FILL-IN-1 + "j".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-K
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-K Dialog-Frame
ON CHOOSE OF BUTTON-K IN FRAME Dialog-Frame /* K */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "K".
      ELSE FILL-IN-1 = FILL-IN-1 + "k".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-L
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-L Dialog-Frame
ON CHOOSE OF BUTTON-L IN FRAME Dialog-Frame /* L */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "L".
      ELSE FILL-IN-1 = FILL-IN-1 + "l".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-M
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-M Dialog-Frame
ON CHOOSE OF BUTTON-M IN FRAME Dialog-Frame /* M */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "M".
      ELSE FILL-IN-1 = FILL-IN-1 + "m".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-N
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-N Dialog-Frame
ON CHOOSE OF BUTTON-N IN FRAME Dialog-Frame /* N */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "N".
      ELSE FILL-IN-1 = FILL-IN-1 + "n".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-O
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-O Dialog-Frame
ON CHOOSE OF BUTTON-O IN FRAME Dialog-Frame /* O */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "O".
      ELSE FILL-IN-1 = FILL-IN-1 + "o".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-OO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-OO Dialog-Frame
ON CHOOSE OF BUTTON-OO IN FRAME Dialog-Frame /* Ö */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:   
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "Ö".
      ELSE FILL-IN-1 = FILL-IN-1 + "ö".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-P
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-P Dialog-Frame
ON CHOOSE OF BUTTON-P IN FRAME Dialog-Frame /* P */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "P".
      ELSE FILL-IN-1 = FILL-IN-1 + "p".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Q
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Q Dialog-Frame
ON CHOOSE OF BUTTON-Q IN FRAME Dialog-Frame /* Q */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "Q".
      ELSE FILL-IN-1 = FILL-IN-1 + "q".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-R
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-R Dialog-Frame
ON CHOOSE OF BUTTON-R IN FRAME Dialog-Frame /* R */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "R".
      ELSE FILL-IN-1 = FILL-IN-1 + "r".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-S
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-S Dialog-Frame
ON CHOOSE OF BUTTON-S IN FRAME Dialog-Frame /* S */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "S".
      ELSE FILL-IN-1 = FILL-IN-1 + "s".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-T
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-T Dialog-Frame
ON CHOOSE OF BUTTON-T IN FRAME Dialog-Frame /* T */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "T".
      ELSE FILL-IN-1 = FILL-IN-1 + "t".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-U
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-U Dialog-Frame
ON CHOOSE OF BUTTON-U IN FRAME Dialog-Frame /* U */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "U".
      ELSE FILL-IN-1 = FILL-IN-1 + "u".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-V
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-V Dialog-Frame
ON CHOOSE OF BUTTON-V IN FRAME Dialog-Frame /* V */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "V".
      ELSE FILL-IN-1 = FILL-IN-1 + "v".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-W
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-W Dialog-Frame
ON CHOOSE OF BUTTON-W IN FRAME Dialog-Frame /* W */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "W".
      ELSE FILL-IN-1 = FILL-IN-1 + "w".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-X
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-X Dialog-Frame
ON CHOOSE OF BUTTON-X IN FRAME Dialog-Frame /* X */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "X".
      ELSE FILL-IN-1 = FILL-IN-1 + "x".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-XX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-XX Dialog-Frame
ON CHOOSE OF BUTTON-XX IN FRAME Dialog-Frame /* , */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + ",".
      ELSE FILL-IN-1 = FILL-IN-1 + ",".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-XXX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-XXX Dialog-Frame
ON CHOOSE OF BUTTON-XXX IN FRAME Dialog-Frame /* . */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + ".".
      ELSE FILL-IN-1 = FILL-IN-1 + ".".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-XXXX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-XXXX Dialog-Frame
ON CHOOSE OF BUTTON-XXXX IN FRAME Dialog-Frame /* - */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "-".
      ELSE FILL-IN-1 = FILL-IN-1 + "-".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Y
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Y Dialog-Frame
ON CHOOSE OF BUTTON-Y IN FRAME Dialog-Frame /* Y */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "Y".
      ELSE FILL-IN-1 = FILL-IN-1 + "y".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Z
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Z Dialog-Frame
ON CHOOSE OF BUTTON-Z IN FRAME Dialog-Frame /* Z */
DO:   
   RUN koll_UI.
   IF fel = FALSE THEN DO:  
      langd = langd + 1.    
      IF TOG_VAL = TRUE THEN
      FILL-IN-1 = FILL-IN-1 + "Z".
      ELSE FILL-IN-1 = FILL-IN-1 + "z".
      DISPLAY FILL-IN-1 WITH FRAME {&FRAME-NAME}.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VAL Dialog-Frame
ON VALUE-CHANGED OF TOG_VAL IN FRAME Dialog-Frame /* Versaler */
DO:
   TOG_VAL = INPUT TOG_VAL.
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
   langd = 0.
   RUN enable_UI.       
   {FRMSIZED.I}
   IF tanint = FALSE THEN DO:
      ENABLE BUTTON-A BUTTON-B BUTTON-C BUTTON-D BUTTON-E 
      BUTTON-F BUTTON-G BUTTON-H BUTTON-I BUTTON-J BUTTON-K
      BUTTON-L BUTTON-M BUTTON-N BUTTON-O BUTTON-P BUTTON-Q
      BUTTON-R BUTTON-S BUTTON-T BUTTON-U BUTTON-V BUTTON-W
      BUTTON-X BUTTON-Y BUTTON-Z BUTTON-AA BUTTON-AAA BUTTON-OO
      BUTTON-XX BUTTON-XXXX BTN_SPACE TOG_VAL WITH FRAME {&FRAME-NAME}.
   END.
   {musarrow.i}
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
  DISPLAY FILL-IN-1 TOG_VAL 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 RECT-2 FILL-IN-1 BTN_DEL BUTTON-0 BUTTON-1 BUTTON-2 BUTTON-3 
         BUTTON-4 BUTTON-5 BUTTON-6 BUTTON-7 BUTTON-8 BUTTON-9 BUTTON-XXX 
         Btn_OK 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE koll_UI Dialog-Frame 
PROCEDURE koll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   fel = FALSE. 
   IF langd = tanlangd + 1 THEN DO:
      MESSAGE "Max antal tecken är " + STRING(tanlangd) + " st."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      fel = TRUE.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

