&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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

  Created: 95/09/15 -  2:57 pm

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
&Scoped-define NEW                                
{GLOBVAR2DEL1.I}
/*{SPECMTRLG1.I}*/
  {SMTRL.I}
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE EDITOR_MEDD AS CHARACTER  VIEW-AS EDITOR SCROLLBAR-VERTICAL SIZE 53 BY 9 FONT 25 NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.        
DEFINE VARIABLE str AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str1 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str0 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
/*DEFINE VARIABLE sumpris LIKE BERMTRL.PRIS NO-UNDO.    
DEFINE VARIABLE totalsum LIKE BERMTRL.PRIS NO-UNDO.
DEFINE VARIABLE sumantal LIKE BERMTRL.ANTAL NO-UNDO.
DEFINE VARIABLE xkordvar LIKE BERID.XKORD NO-UNDO.  
DEFINE VARIABLE stopvar AS LOGICAL NO-UNDO.         
DEFINE VARIABLE prisvar AS INTEGER NO-UNDO.         
DEFINE VARIABLE antvar LIKE MTRLBER.ANTAL NO-UNDO. 
DEFINE VARIABLE antvar2 LIKE MTRLBER.ANTAL NO-UNDO. 
DEFINE VARIABLE firstlev LIKE MTRL.LEVKOD NO-UNDO.  */ 
DEFINE VARIABLE mtrlrak AS INTEGER NO-UNDO.  
DEFINE VARIABLE mtrlrak2 AS INTEGER NO-UNDO.
DEFINE VARIABLE radrak AS INTEGER NO-UNDO.   
/*DEFINE VARIABLE langd AS INTEGER NO-UNDO.  
DEFINE VARIABLE upp AS LOGICAL NO-UNDO.            
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE prognamn2 AS CHARACTER FORMAT "X(41)" NO-UNDO.*/
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
/*DEFINE VARIABLE bytgrupp AS CHARACTER NO-UNDO. 
DEFINE VARIABLE enrvar LIKE MTRL.ENR NO-UNDO.    */
DEFINE INPUT  PARAMETER bestoff  AS CHARACTER.
/*DEFINE VARIABLE utrec AS RECID NO-UNDO.
DEFINE VARIABLE utrec2 AS RECID NO-UNDO.
DEFINE VARIABLE kant AS INTEGER NO-UNDO.
DEFINE VARIABLE tant AS INTEGER NO-UNDO.*/
/*DEFINE VARIABLE uppvar AS LOGICAL NO-UNDO.*/
/*DEFINE VARIABLE sidlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE ediraknare AS INTEGER NO-UNDO.*/
/*DEFINE VARIABLE anmlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE idvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE numsok AS LOGICAL NO-UNDO.    */
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{SKAPAMTRL.I}


{TIDUTTT.I}

DEFINE VARIABLE MED_EDITOR AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 11.73
     BGCOLOR 8  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT FOR 
      tidut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(132)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 81 BY 20.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_AVS AT ROW 22.25 COL 68.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 83.75 BY 22.73.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: tidut T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Specifikation av beställning"
         HEIGHT             = 22.83
         WIDTH              = 83.75
         MAX-HEIGHT         = 25.13
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 25.13
         VIRTUAL-WIDTH      = 100
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
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
/* SETTINGS FOR FRAME FRAME-VINST
                                                                        */
/* BROWSE-TAB BRW_UT 1 FRAME-VINST */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST                = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "temp-db.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = temp-db.tidut.ut
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UT
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

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
   {muswait.i} 
   {ALLSTARTDYN.I} 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   
   str=
"=============================================================================". 
   str1 =
"*****************************************************************************".  
   str0 = 
"-----------------------------------------------------------------------------".     
   RUN huvud_UI.
   IF musz = FALSE THEN RUN klar_UI.                                
   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.                 
   RUN ut_UI.  
   status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
   APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
   IF status-mus2 = status-mus2 THEN LEAVE MAIN-BLOCK.       
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
  
      
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
  ENABLE BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-2 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/ 
   DO TRANSACTION:         
      IF musz = TRUE THEN musz = musz.   
      ELSE DO: 
         FIND FIRST skapa_mtrl NO-LOCK NO-ERROR.
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,1) = "<Order>".
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "<SystemID>GURU</SystemID>".
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,1) = "<Orderhuvud>".
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,1) = "<Dokumenttyp>Offert</Dokumenttyp>".
         CREATE tidut. 
         ASSIGN SUBSTRING(tidut.UT,1) = "<Datum>"
         SUBSTRING(tidut.UT,8) = STRING(TODAY,"9999-99-99")
         SUBSTRING(tidut.UT,18) = "</Datum>".
         CREATE tidut. 
         ASSIGN SUBSTRING(tidut.UT,1) = "<Tid>"        
         SUBSTRING(tidut.UT,6) = STRING(TIME,"HH:MM:SS"). 
         SUBSTRING(tidut.UT,14) = "</Tid>".         
         CREATE tidut.          
         ASSIGN SUBSTRING(tidut.UT,9) = "<Kundnummer>"        
         SUBSTRING(tidut.UT,21) = STRING(skapa_mtrl.KUNDNR,"99999") + "</Kundnummer>".
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Firma>"        
         SUBSTRING(tidut.UT,16) = SUBSTRING(skapa_mtrl.FORE,1,35) + "</Firma>".
         IF skapa_mtrl.KIKONTAKT NE "" THEN DO:
            CREATE tidut.
            ASSIGN SUBSTRING(tidut.UT,9) = "<Kontaktperson>"        
            SUBSTRING(tidut.UT,24) = SUBSTRING(skapa_mtrl.KIKONTAKT,1,35) + "</Kontaktperson>".
         END.
         IF skapa_mtrl.KITELE NE "" THEN DO:   
            CREATE tidut.
            ASSIGN SUBSTRING(tidut.UT,9) = "<Telefon>"        
            SUBSTRING(tidut.UT,18) = STRING(skapa_mtrl.KITELE) + "</Telefon>".
         END.
         IF skapa_mtrl.FAX NE "" THEN DO:    
            CREATE tidut.
            ASSIGN SUBSTRING(tidut.UT,9) = "<Fax>"        
            SUBSTRING(tidut.UT,14) = STRING(skapa_mtrl.FAX) + "</Fax>".
         END.   
         IF skapa_mtrl.KIMOBIL NE "" THEN DO:   
            CREATE tidut.
            ASSIGN SUBSTRING(tidut.UT,9) = "<Mobil>"        
            SUBSTRING(tidut.UT,16) = STRING(skapa_mtrl.KIMOBIL) + "</Mobil>".
         END.   
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Gatuadress>"        
         SUBSTRING(tidut.UT,21) = SUBSTRING(skapa_mtrl.KADR,1,35) + "</Gatuadress>".
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Postnummer>".
         IF LENGTH(skapa_mtrl.L2) = 5 THEN        
         SUBSTRING(tidut.UT,21) = STRING(skapa_mtrl.KPNR,"999 99") + "</Postnummer>".
         ELSE SUBSTRING(tidut.UT,21) = STRING(skapa_mtrl.KPNR) + "</Postnummer>".
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Ort>"        
         SUBSTRING(tidut.UT,14) = SUBSTRING(skapa_mtrl.KORT,1,35) + "</Ort>".
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,1) = "</Orderhuvud>".                      
      END.
   END.                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar_UI WINDOW-2 
PROCEDURE klar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "<Orderdata>"        
   mtrlrak = 1.
   mtrlrak2 = 0.
   radrak = 0.
   FOR EACH spec_mtrl WHERE spec_mtrl.BERKVANT > 0:       
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "<Post " + STRING(mtrlrak) + ">".
      radrak = radrak + 1.      
      CREATE tidut.
      ASSIGN 
      SUBSTRING(tidut.UT,9) = "<Artikelnr>" + STRING(RIGHT-TRIM(spec_mtrl.ENR)) + "</Artikelnr>".               
      radrak = radrak + 1.
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,9) = "<Beskrivning>" +        
      STRING(RIGHT-TRIM(spec_mtrl.BENAMNING)) + "</Beskrivning>". 
      radrak = radrak + 1.     
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,9) = "<Enhet>" + STRING(RIGHT-TRIM(spec_mtrl.ENHET)) + "</Enhet>". 
      radrak = radrak + 1.
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,9) = "<Antal>" + TRIM(STRING(spec_mtrl.BERKVANT))
      + "</Antal>".
      mtrlrak2 = mtrlrak2 + spec_mtrl.BERKVANT.
      radrak = radrak + 1.
      /*CREATE tidut.
 *       ASSIGN SUBSTRING(tidut.UT,9) = "<Pris>" + RIGHT-TRIM(STRING(spec_mtrl.NPRIS))
 *       + "</Pris>".*/
      radrak = radrak + 1.         
      
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "</Post " + STRING(mtrlrak) + ">".
      radrak = radrak + 1.
      radrak = radrak + 1.
      mtrlrak = mtrlrak + 1.
   END.   
   mtrlrak = mtrlrak - 1.  
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "</Orderdata>".
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "<Checksumma>" + STRING(mtrlrak + mtrlrak2) 
   + "</Checksumma>".
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "</Order>".  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-2 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
/*UT*/         
   
   EDITOR_MEDD = "".
  
   IF Guru.Konstanter:globforetag = "ELPA"  THEN DO:
      RUN EPOST2.P (INPUT "",INPUT "elpool.ume@elpool.se",INPUT bestoff,INPUT TABLE tidut,      
      INPUT "",INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT skick,OUTPUT efel).
      IF efel NE "" THEN MESSAGE efel VIEW-AS ALERT-BOX.          
      IF skick = TRUE THEN RETURN NO-APPLY.     
   END.
   ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" {GLOBVES.I} THEN DO:                          
      IF bestoff = "beställning" THEN DO:
         /*"order@elef.se"*/
         RUN EPOST2.P (INPUT "",INPUT "order@elef.se",INPUT bestoff,INPUT TABLE tidut,         
         INPUT "",INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT skick,OUTPUT efel).
         IF efel NE "" THEN MESSAGE efel VIEW-AS ALERT-BOX.
         IF skick = TRUE THEN RETURN NO-APPLY.       
         ELSE DO:
            MESSAGE "E-posten är nu skickad till utkorgen" VIEW-AS ALERT-BOX.
         END.
      END.
      IF bestoff = "offert" THEN DO:
         /*"offert@elef.se"*/
         RUN EPOST2.P (INPUT "",INPUT "offert@elef.se",INPUT bestoff,INPUT TABLE tidut,         
         INPUT "",INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT skick,OUTPUT efel).
         IF efel NE "" THEN MESSAGE efel VIEW-AS ALERT-BOX.     
         IF skick = TRUE THEN RETURN NO-APPLY.    
         ELSE DO:
            MESSAGE "E-posten är nu skickad till utkorgen" VIEW-AS ALERT-BOX.
         END.
      END.
   END.

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

