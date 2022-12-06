&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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
DEFINE OUTPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER aonrrec AS RECID NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{NRSERIETEMPN.I}
{GLOBVAR2DEL1.I}
{OMRTEMPW.I}

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE nrvalapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_PRE

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES nrprefistemp

/* Definitions for BROWSE BRW_PRE                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_PRE nrprefistemp.PREFIX ~
nrprefistemp.NRTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PRE nrprefistemp.PREFIX 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PRE nrprefistemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PRE nrprefistemp
&Scoped-define QUERY-STRING-BRW_PRE FOR EACH nrprefistemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PRE OPEN QUERY BRW_PRE FOR EACH nrprefistemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PRE nrprefistemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PRE nrprefistemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_FAST BRW_PRE BTN_KLAR BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_FAST 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_PRE 
       MENU-ITEM m_Avmarkera    LABEL "Avmarkera"     .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_KLAR AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(256)":U 
     LABEL "AONR" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 40 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_PRE FOR 
      nrprefistemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_PRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PRE Dialog-Frame _STRUCTURED
  QUERY BRW_PRE NO-LOCK DISPLAY
      nrprefistemp.PREFIX COLUMN-LABEL "Prefix" FORMAT "X(2)":U
      nrprefistemp.NRTEXT COLUMN-LABEL "Text för prefix" FORMAT "X(256)":U
            WIDTH 32.88
  ENABLE
      nrprefistemp.PREFIX
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 44 BY 14.13
         TITLE "Prefix".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     RAD_FAST AT ROW 1.25 COL 1.5 NO-LABEL
     BRW_PRE AT ROW 2.63 COL 1.5
     FILL-IN-AONR AT ROW 17.17 COL 1.5
     BTN_KLAR AT ROW 18.71 COL 16.5
     BTN_AVB AT ROW 18.71 COL 31.5
     SPACE(0.62) SKIP(0.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Välj typ av arbetsorder serie".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: nrprefistemp T "?" NO-UNDO temp-db nrprefistemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_PRE RAD_FAST Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_PRE:POPUP-MENU IN FRAME Dialog-Frame             = MENU POPUP-MENU-BRW_PRE:HANDLE.

/* SETTINGS FOR FILL-IN FILL-IN-AONR IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       FILL-IN-AONR:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME Dialog-Frame
   SHARED                                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PRE
/* Query rebuild information for BROWSE BRW_PRE
     _TblList          = "Temp-Tables.nrprefistemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.nrprefistemp.PREFIX
"nrprefistemp.PREFIX" "Prefix" "X(2)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.nrprefistemp.NRTEXT
"nrprefistemp.NRTEXT" "Text för prefix" "X(256)" "character" ? ? ? ? ? ? no ? no no "32.88" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PRE */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Välj typ av arbetsorder serie */
DO:
   musz = TRUE.
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nrvalapph) THEN DELETE PROCEDURE nrvalapph.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Välj typ av arbetsorder serie */
DO:
    APPLY "END-ERROR" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Välj typ av arbetsorder serie */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PRE
&Scoped-define SELF-NAME BRW_PRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PRE Dialog-Frame
ON LEAVE OF BRW_PRE IN FRAME Dialog-Frame /* Prefix */
DO:
   IF AVAILABLE nrprefistemp THEN 
   DISPLAY nrprefistemp.NRTEXT WITH BROWSE BRW_PRE.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PRE Dialog-Frame
ON MOUSE-SELECT-CLICK OF BRW_PRE IN FRAME Dialog-Frame /* Prefix */
DO:
   APPLY "VALUE-CHANGED" TO BRW_PRE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PRE Dialog-Frame
ON VALUE-CHANGED OF BRW_PRE IN FRAME Dialog-Frame /* Prefix */
DO:
   RUN fastnr_UI (INPUT 1). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KLAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KLAR Dialog-Frame
ON CHOOSE OF BTN_KLAR IN FRAME Dialog-Frame /* Ok */
DO:
   /*IF NOT AVAILABLE nrprefistemp THEN DO:
      MESSAGE "Du måste välja ett prefix!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.*/
   FILL-IN-AONR = INPUT FILL-IN-AONR.
   aonrvar = "".
   IF AVAILABLE nrprefistemp THEN aonrvar = nrprefistemp.PREFIX.
   IF FILL-IN-AONR NE "" THEN DO:
      IF FILL-IN-AONR NE aonrvar THEN aonrvar = "F" + FILL-IN-AONR.
   END.      
   IF aonrvar = "" OR aonrvar = "F" THEN DO:
      MESSAGE "Du måste välja något!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "elpa" THEN DO: 
      IF AVAILABLE nrprefistemp THEN DO:         
         MESSAGE "Du har valt prefix" nrprefistemp.PREFIX + " " + nrprefistemp.NRTEXT +
         " är detta korrekt?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
         CASE val:         
            WHEN FALSE THEN DO:
               RETURN NO-APPLY.
            END.      
         END CASE.   
      END.

   END.  
   RUN AONRNAMNKOLL.P (INPUT 1, INPUT FILL-IN-AONR,OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
    RUN AONRNAMNKOLL.P (INPUT 2, INPUT FILL-IN-AONR,OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END. 
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "elpa" THEN DO: 
      IF AVAILABLE nrprefistemp THEN DO:
         IF Guru.Konstanter:globforetag = "elpa" THEN DO: 
            IF nrprefistemp.JUDID = "BYGG" THEN DO:
               aonrvar = "S" + aonrvar.
            END.
         END.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO: 
            IF nrprefistemp.JUDID = "SEAB" THEN DO:
               aonrvar = "S" + aonrvar.
            END.
         END.
      END.         
   END.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN NRAOAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT globomr,INPUT RAD_FAST,INPUT-OUTPUT aonrvar,OUTPUT aonrrec).
   END.
   ELSE DO:
      RUN NRAOAPP.P 
      (INPUT globomr,INPUT RAD_FAST,INPUT-OUTPUT aonrvar,OUTPUT aonrrec).
   END.       
   IF aonrvar = "XXXXXX" THEN DO:
      MESSAGE "Nummerserien är full eller nummret är redan upplagt! Välj ett annat nummer!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   APPLY "GO" TO BTN_KLAR IN FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KLAR Dialog-Frame
ON GO OF BTN_KLAR IN FRAME Dialog-Frame /* Ok */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nrvalapph) THEN DELETE PROCEDURE nrvalapph.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Avmarkera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Avmarkera Dialog-Frame
ON CHOOSE OF MENU-ITEM m_Avmarkera /* Avmarkera */
DO:
    status-ok = BRW_PRE:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
    RELEASE  nrprefistemp NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST Dialog-Frame
ON VALUE-CHANGED OF RAD_FAST IN FRAME Dialog-Frame
DO:
   RAD_FAST = INPUT RAD_FAST.
   RUN fastnr_UI (INPUT 0).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   /*RUN nrseriehmt IN nrvalapph (OUTPUT TABLE nrprefistemp,
                               OUTPUT TABLE nrstyrtemp).*/
   DEBUGGER:SET-BREAK().                            
   RUN nrseriejurhmt IN nrvalapph (INPUT Guru.Konstanter:globanv , OUTPUT TABLE nrprefistemp,OUTPUT TABLE nrstyrtemp).
   FIND FIRST nrstyrtemp WHERE NO-LOCK NO-ERROR.
   FIND FIRST nrprefistemp WHERE NO-LOCK NO-ERROR.
   {TILLFAST.I}
   IF Guru.Konstanter:globanv = "elpa" THEN.
   ELSE DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   IF RAD_FAST = ? THEN RAD_FAST = FALSE.
   RUN enable_UI.       
   APPLY "VALUE-CHANGED" TO RAD_FAST.   
   ASSIGN FRAME {&FRAME-NAME}:TITLE = "Välj typ av " + LC(Guru.Konstanter:gaol) + "-serie.".         
   /*SEJFK och SEUJO skall bara kunna lägga upp fasta aonr för SEAB
   Borttaget 2008-05-15 eftersom fasta projekt inte finns längre */
   IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      IF Guru.Konstanter:globanv = "SEIEN" OR Guru.Konstanter:globanv = QUOTER({setuser.I}) OR Guru.Konstanter:globanv = "SELNG"  THEN DO:
         FILL-IN-AONR:HIDDEN = FALSE.
         DISPLAY FILL-IN-AONR WITH FRAME {&FRAME-NAME}.
         FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.
         ENABLE FILL-IN-AONR WITH FRAME {&FRAME-NAME}.
      END.           
   END.         
   IF Guru.Konstanter:globforetag = "SUND" THEN DO:
      FILL-IN-AONR:HIDDEN = FALSE.
      DISPLAY FILL-IN-AONR WITH FRAME {&FRAME-NAME}.
      FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.
      ENABLE FILL-IN-AONR WITH FRAME {&FRAME-NAME}.
   END.
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN Getfirst_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1]. 
    
     
   {FRMSIZED.I}   
   {DIA_M_SLUT.I}
   /*
   RELEASE nrprefistemp NO-ERROR.
   */
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   nrprefistemp.PREFIX:READ-ONLY IN BROWSE BRW_PRE = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_PRE:HANDLE IN FRAME {&FRAME-NAME}).  
   IF Guru.Konstanter:appcon THEN DO:
      RUN NRVALAPPN.P PERSISTENT SET nrvalapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NRVALAPPN.P PERSISTENT SET nrvalapph.
   END.  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY RAD_FAST 
      WITH FRAME Dialog-Frame.
  ENABLE RAD_FAST BRW_PRE BTN_KLAR BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fastnr_UI Dialog-Frame 
PROCEDURE fastnr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   FILL-IN-AONR = INPUT FRAME {&FRAME-NAME} FILL-IN-AONR.
   IF vad = 1 THEN DO:
      IF NOT AVAILABLE nrprefistemp THEN DO:
         RUN Getfirst_UI IN brwproc[1].
      END. 
      FILL-IN-AONR = nrprefistemp.PREFIX + SUBSTRING(FILL-IN-AONR,LENGTH(nrprefistemp.PREFIX) + 1).
   END.
   IF RAD_FAST = TRUE THEN DO:
      DISPLAY FILL-IN-AONR WITH FRAME {&FRAME-NAME}.
      FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.
      ENABLE FILL-IN-AONR WITH FRAME {&FRAME-NAME}.
   END.   
   ELSE IF AVAILABLE nrprefistemp AND  RAD_FAST = FALSE AND nrprefistemp.PREFIX = "00"  THEN DO:
      DISPLAY FILL-IN-AONR WITH FRAME {&FRAME-NAME}.
      FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.
      ENABLE FILL-IN-AONR WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      FILL-IN-AONR:HIDDEN = TRUE.
   END.
   /*IF Guru.Konstanter:globanv = "SEIEN" OR Guru.Konstanter:globanv = "SEBEN" OR Guru.Konstanter:globanv = "SESST" OR Guru.Konstanter:globanv = "SEJFK" OR Guru.Konstanter:globanv = "SEUJO" OR Guru.Konstanter:globanv = "elpa" THEN DO:*/
      FILL-IN-AONR:HIDDEN = FALSE.
      DISPLAY FILL-IN-AONR WITH FRAME {&FRAME-NAME}.
      FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.
      ENABLE FILL-IN-AONR WITH FRAME {&FRAME-NAME}.
   /*END.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

