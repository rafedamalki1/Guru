&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
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
{GLOBVAR2DEL1.I}
&Scoped-define NEW                                    
DEFINE SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE nytt_bestnr AS INTEGER NO-UNDO.                                          
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE epostvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE EDITOR_MEDD AS CHARACTER  VIEW-AS EDITOR SCROLLBAR-VERTICAL SIZE 53 BY 9 FONT 25 NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.        
DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE totalt2 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE firstlev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE mtrlrak AS INTEGER NO-UNDO.  
DEFINE VARIABLE radrak AS INTEGER NO-UNDO.   
DEFINE VARIABLE langd AS INTEGER NO-UNDO.     
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(41)" NO-UNDO. 
DEFINE VARIABLE prognamn2 AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE INPUT  PARAMETER bestoff  AS CHARACTER.
DEFINE VARIABLE maildepapph AS HANDLE NO-UNDO.


DEFINE TEMP-TABLE felmeddftptemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.            

&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{SPECMTRLTEMP.I}
{SKAPAMTRL.I}
   
DEFINE SHARED TEMP-TABLE lev_temp NO-UNDO
   FIELD LEVKOD AS CHARACTER.      
   
{TIDUTTTNEW.I}


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 81 BY 26.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_AVS AT ROW 28 COL 68.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 83 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Specifikation av beställning"
         COLUMN             = 11.75
         ROW                = 2.13
         HEIGHT             = 28.42
         WIDTH              = 83.38
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 28.42
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-VINST
                                                                        */
/* BROWSE-TAB BRW_UT 1 FRAME-VINST */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST                = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = no.

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
   FIND FIRST spec_mtrl WHERE spec_mtrl.LEVKOD = vald_lev NO-LOCK NO-ERROR.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.     
   EMPTY TEMP-TABLE tidut NO-ERROR.                                    
str=
"=============================================================================".    
   RUN huvud_UI.     
   RUN klar_UI.      
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
  
   IF Guru.Konstanter:appcon THEN DO:
      RUN MAILDEPAPP.P PERSISTENT SET maildepapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MAILDEPAPP.P PERSISTENT SET maildepapph.
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
  ENABLE BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
  VIEW WINDOW-2.
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
      FIND FIRST skapa_mtrl NO-LOCK NO-ERROR.
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,1) = "<Order>".
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "<SystemID>GURU</SystemID>".
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,1) = "<Orderhuvud>".
      CREATE tidut.         
      ASSIGN SUBSTRING(tidut.UT,1) = "<Dokumenttyp>Beställning</Dokumenttyp>".         
      CREATE tidut. 
      ASSIGN SUBSTRING(tidut.UT,1) = "<Datum>"
      SUBSTRING(tidut.UT,8) = STRING(TODAY,"99-99-99") + "</Datum>".      
      CREATE tidut. 
      ASSIGN SUBSTRING(tidut.UT,1) = "<Tid>"        
      SUBSTRING(tidut.UT,6) = STRING(TIME,"HH:MM:SS") + "</Tid>".       
      FIND levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR. 
      CREATE tidut.          
      ASSIGN 
      SUBSTRING(tidut.UT,9) = "<Kundnummer>".         
      SUBSTRING(tidut.UT,21) = STRING(skapa_mtrl.KUNDNR,"99999") + "</Kundnummer>".                  
      IF Guru.Konstanter:globforetag = "GKAL" THEN DO:         
         CREATE tidut.
         SUBSTRING(tidut.UT,9) = "<Organisationsnr>".
         SUBSTRING(tidut.UT,26) = "5561827543</Organisationsnr>".
      END.
      ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         CREATE tidut.
         SUBSTRING(tidut.UT,9) = "<Organisationsnr>".
         SUBSTRING(tidut.UT,26) = "5565027223</Organisationsnr>".
      END.
      ELSE IF Guru.Konstanter:globforetag = "VAST" THEN DO:
         CREATE tidut.
         SUBSTRING(tidut.UT,9) = "<Organisationsnr>".
         SUBSTRING(tidut.UT,26) = "5564170800</Organisationsnr>".
      END.
      ELSE DO:
         CREATE tidut.
         SUBSTRING(tidut.UT,9) = "<Organisationsnr>".
         SUBSTRING(tidut.UT,26) = "</Organisationsnr>".
      END.
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,9) = "<Firma>"        
      SUBSTRING(tidut.UT,16) = SUBSTRING(skapa_mtrl.FORE,1,25) + "</Firma>".
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,9) = "<Gatuadress>"        
      SUBSTRING(tidut.UT,21) = SUBSTRING(skapa_mtrl.L1,1,25) + "</Gatuadress>".
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,9) = "<Postnummer>".
      IF LENGTH(skapa_mtrl.L2) = 5 THEN        
      SUBSTRING(tidut.UT,21) = STRING(skapa_mtrl.L2,"99999") + "</Postnummer>".
      ELSE DO:
         SUBSTRING(tidut.UT,21) = STRING(REPLACE(skapa_mtrl.L2," ",""),"99999") + "</Postnummer>".            
      END.
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,9) = "<Ort>"        
      SUBSTRING(tidut.UT,14) = SUBSTRING(skapa_mtrl.L3,1,20) + "</Ort>".
      /*
      CREATE tidut. 
      ASSIGN SUBSTRING(tidut.UT,9) = "<Leveranssätt>"
      SUBSTRING(tidut.UT,23) = "01"
                           Leveranssätt (2 N)   01      Onninen distribution system
                           02   Fetching
                           03   Post
                           05   Express delivery
                           06   Van, consignee pay freight
                           08   Bus
                           09   Van, Onninen pay freight
                           11   Air cargo
                           12   Fetching from other stock
                           30   Sawing
                           32   Express-sawing
      SUBSTRING(tidut.UT,9) = "</Leveranssätt>".
      */
      IF skapa_mtrl.KIKONTAKT NE "" THEN DO:
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Kontaktperson>"        
         SUBSTRING(tidut.UT,24) = SUBSTRING(skapa_mtrl.KIKONTAKT,1,20) + "</Kontaktperson>".
      END.
      IF epostvar NE "" THEN DO:         
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<e-mail>".
         SUBSTRING(tidut.UT,17) = SUBSTRING(epostvar,1,80) + "</e-mail>".
      END.         
      CREATE tidut.        
      ASSIGN SUBSTRING(tidut.UT,9) = "<ProjektID>"        
      SUBSTRING(tidut.UT,20) = "VD." + STRING(vald_depa) + "." + STRING(skapa_mtrl.bestnr)
      + "</ProjektID>".      
      IF skapa_mtrl.MARK NE "" THEN DO:   
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Marke>"        
         SUBSTRING(tidut.UT,16) = SUBSTRING(skapa_mtrl.MARK,1,20) + "</Marke>".
      END.   
      IF skapa_mtrl.AVIS = TRUE THEN DO: 
         CREATE tidut.         
         ASSIGN SUBSTRING(tidut.UT,9) = "<Avisering>"        
         SUBSTRING(tidut.UT,20) = STRING(skapa_mtrl.AVISPERS)
         + "</Avisering>".
      END.            
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,9) = "<Leveransdatum>"        
      SUBSTRING(tidut.UT,24) = STRING(skapa_mtrl.DATUM,"99-99-99") + "</Leveransdatum>".         
      CREATE tidut.         
      ASSIGN SUBSTRING(tidut.UT,9) = "<Konto>"        
      SUBSTRING(tidut.UT,16) = " " 
      + "</Konto>".         
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,9) = "<Kommentarorderhuvud>".            
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = SUBSTRING(skapa_mtrl.KOM,1,40).
      IF LENGTH(skapa_mtrl.KOM) > 40 THEN DO:               
         CREATE tidut.         
         ASSIGN SUBSTRING(tidut.UT,1) = SUBSTRING(skapa_mtrl.KOM,41,40).
      END.         
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Kontaktperson inköp:" + STRING(skapa_mtrl.KIKONTAKT).
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Tele:" + STRING(skapa_mtrl.KITELE) +
      " Mobil:" + STRING(skapa_mtrl.KIMOBIL).             
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Kontaktperson teknik:" + STRING(skapa_mtrl.KTKONTAKT).
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Tele:" + STRING(skapa_mtrl.KTTELE) +  
      " Mobil:" + STRING(skapa_mtrl.KTMOBIL).        
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,9) = "</Kommentarorderhuvud>".         
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,1) = "</Orderhuvud>".                            
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
   radrak = 0.
   FOR EACH spec_mtrl WHERE spec_mtrl.LEVKOD = vald_lev AND spec_mtrl.BERKVANT > 0:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "<Post " + STRING(mtrlrak) + ">".      
      CREATE tidut.
      IF SUBSTRING(spec_mtrl.ENR,1,1) = "E" THEN DO:
         ASSIGN 
         SUBSTRING(tidut.UT,9) = "<Artikelnr>" + STRING(RIGHT-TRIM(spec_mtrl.ENR)) + "</Artikelnr>".               
      END.
      ELSE DO:
         ASSIGN 
         SUBSTRING(tidut.UT,9) = "<Artikelnr>" + "E" + STRING(RIGHT-TRIM(spec_mtrl.ENR)) + "</Artikelnr>".               
      END.                               
      CREATE tidut.          
      ASSIGN SUBSTRING(tidut.UT,9) = "<Beskrivning>" +        
      RIGHT-TRIM(STRING(spec_mtrl.BENAMNING)) + "</Beskrivning>".       
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,9) = "<Enhet>" + TRIM(STRING(spec_mtrl.ENHET)) + "</Enhet>".       
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,9) = "<Antal>" + TRIM(STRING(spec_mtrl.BERKVANT))  
      + "</Antal>".      
      radrak = radrak + spec_mtrl.BERKVANT.
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,9) = "<Pris>" + TRIM(STRING(spec_mtrl.NPRIS))
      + "</Pris>".      
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "</Post " + STRING(mtrlrak) + ">".      
      CREATE tidut.      
      mtrlrak = mtrlrak + 1.
   END.      
   mtrlrak = mtrlrak - 1.  
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "</Orderdata>".
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "<Checksumma>" + STRING(radrak + mtrlrak) 
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
   prognamn = SESSION:TEMP-DIRECTORY.
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN prognamn = webclienttempdir.    
   
   IF bestoff = "beställning" THEN DO:
      prognamn = prognamn + "depbest" + STRING(nytt_bestnr) + ".txt".
   END.   
   IF bestoff = "offert" THEN DO:
     prognamn = prognamn + "depoff" + STRING(nytt_bestnr) + ".txt".                        
   END.
   OUTPUT TO VALUE(prognamn).
   FOR EACH tidut:          
      PUT tidut.UT.
      PUT SKIP.
   END.  
   OUTPUT CLOSE.
 /* IF SEARCH(prognamn) = ? THEN DO:
      MESSAGE "Filen finns ej." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.*/ 
   EDITOR_MEDD = "".
   FOR EACH tidut:
      EDITOR_MEDD = EDITOR_MEDD + tidut.UT + CHR(13) + CHR(10).
   END.    
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:  
      IF bestoff = "beställning" THEN DO:
         RUN EPOST.P (INPUT "",INPUT "order@elef.se",INPUT bestoff,INPUT EDITOR_MEDD,
         INPUT "",INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT skick,OUTPUT efel).
         IF efel NE "" THEN MESSAGE efel VIEW-AS ALERT-BOX. 
         ELSE DO:
            MESSAGE "E-posten är nu skickad till utkorgen" VIEW-AS ALERT-BOX.
         END.
         IF skick = TRUE THEN RETURN NO-APPLY.
      END.
      IF bestoff = "offert" THEN DO:
         RUN EPOST.P (INPUT "",INPUT "offert@elef.se",INPUT bestoff,INPUT EDITOR_MEDD,
         INPUT "",INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT skick,OUTPUT efel).
         IF efel NE "" THEN MESSAGE efel VIEW-AS ALERT-BOX.
         ELSE DO:
            MESSAGE "E-posten är nu skickad till utkorgen" VIEW-AS ALERT-BOX.
         END.
         IF skick = TRUE THEN RETURN NO-APPLY.
      END.                     
   END.
   ELSE IF Guru.Konstanter:globforetag = "VORD"  OR
   Guru.Konstanter:globforetag = "VAST"  THEN DO:
      RUN skicka_UI IN maildepapph (INPUT vald_depa, INPUT TABLE skapa_mtrl,INPUT TABLE tidut,
                                    OUTPUT TABLE felmeddftptemp).
      FIND FIRST skapa_mtrl NO-ERROR.
      prognamn = SESSION:TEMP-DIRECTORY.       
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN prognamn = webclienttempdir.    
      prognamn = prognamn + "VD." + STRING(vald_depa) + "." + STRING(skapa_mtrl.bestnr) + ".guru".           
      OUTPUT TO VALUE(prognamn).
      FOR EACH tidut:                                                              
         PUT UNFORMATTED                                                           
         TRIM(tidut.UT) + CHR(10).                                                 
      END.                                                                         
      OUTPUT CLOSE.
    
      FIND FIRST felmeddftptemp NO-LOCK NO-ERROR.
      IF felmeddftptemp.FELMEDD BEGINS "Fil skickad..." THEN DO:
         MESSAGE "Beställningen är nu skickad. Filen finns att se under:" + prognamn VIEW-AS ALERT-BOX TITLE "Meddelande".
         DELETE felmeddftptemp.
      END.
      ELSE DO:
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN prognamn = webclienttempdir + "\ftpfel.txt".
         ELSE prognamn = SESSION:TEMP-DIRECTORY + "\ftpfel.txt".
         
         MESSAGE "Ett fel inträffade. Kontakta Elpool 090 - 18 45 40." VIEW-AS ALERT-BOX TITLE "Meddelande".
         OUTPUT TO VALUE(prognamn).
         FOR EACH felmeddftptemp:
            PUT UNFORMATTED
            TRIM(felmeddftptemp.FELMEDD) + CHR(10).
            DELETE felmeddftptemp.
         END.
         OUTPUT CLOSE.
      END.            
   END.
   ELSE IF Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      /*borlänge depåbeställning*/
      RUN skickabor_UI IN maildepapph (INPUT vald_depa, INPUT TABLE skapa_mtrl,INPUT TABLE tidut,INPUT levtemp.LEVKOD,
                                    OUTPUT TABLE felmeddftptemp).
      FIND FIRST skapa_mtrl NO-ERROR.
      prognamn = SESSION:TEMP-DIRECTORY.             
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN prognamn = webclienttempdir.    
      prognamn = prognamn + "D." + STRING(vald_depa) + "." + STRING(skapa_mtrl.bestnr) + "-" + "STRING(levtemp.LEVKOD)" + ".guru".           
      OUTPUT TO VALUE(prognamn).
      FOR EACH tidut:                                                              
         PUT UNFORMATTED                                                           
         TRIM(tidut.UT) + CHR(10).                                                 
      END.                                                                         
      OUTPUT CLOSE.
    
      FIND FIRST felmeddftptemp NO-LOCK NO-ERROR.
      IF felmeddftptemp.FELMEDD BEGINS "Fil skickad..." THEN DO:
         MESSAGE "Beställningen är nu skickad. Filen finns att se under:" + prognamn VIEW-AS ALERT-BOX TITLE "Meddelande".
         DELETE felmeddftptemp.
      END.
      ELSE DO:
         prognamn = SESSION:TEMP-DIRECTORY.        
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN prognamn = webclienttempdir.
         prognamn = prognamn + "edifel.txt".
         MESSAGE "Ett fel inträffade. Kontakta Elpool 090 - 18 45 40." VIEW-AS ALERT-BOX TITLE "Meddelande".
         OUTPUT TO VALUE(prognamn).
         FOR EACH felmeddftptemp:
            PUT UNFORMATTED
            TRIM(felmeddftptemp.FELMEDD) + CHR(10).
            DELETE felmeddftptemp.
         END.
         OUTPUT CLOSE.
      END.            
   END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

