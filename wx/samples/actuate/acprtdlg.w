&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME actuate_printdlg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS actuate_printdlg 
/*------------------------------------------------------------------------

  File: acprtdlg.w

  Description: Print Actuate report dialog

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Gerry Seidl

  Created: June 30, 1997
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{samples/actuate/acpproc.i} /* Actuate API preprocessor option values */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME actuate_printdlg

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rect_duplex_mode rect_options ~
rect_orientation rect_paper_size rect_print_settings pc_filename btn_Browse ~
tog_launch tog_async tog_instance tog_display tog_print tog_open ~
pl_defaultlpt pc_lptname pi_lptnumcopies pi_lptscale pc_lptformname ~
pi_lptcolor pi_lptcollate pi_lptorientation pi_lptduplex pi_lptpapersize ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS pc_filename tog_launch tog_async ~
tog_instance tog_display tog_print tog_open pl_defaultlpt pc_lptname ~
pi_lptnumcopies pi_lptscale pc_lptformname pi_lptcolor pi_lptcollate ~
pi_lptorientation pi_lptduplex pi_lptpapersize pi_lptPaperlength ~
pi_lptpaperWidth 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_Browse 
     LABEL "Browse..." 
     SIZE 11 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE pi_lptpapersize AS CHARACTER FORMAT "X(256)":U INITIAL "Letter 8.5 x 11in" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(specify)","Letter 8.5 x 11in","Letter small 8.5 x 11in","Tabloid 11 x 17in","Ledger 17 x 11in","Legal 8.5 x 14in","Statement 5.5 x 8.5in","Executive 7.25 x 10.5in","A3 sheet 297 x 420mm","A4 sheet 210 x 297mm","A4 small 210 x 297mm","A5 sheet 148 x 210mm","B4 sheet 250 x 354mm","B5 sheet 182 x 257mm","Folio 8.5 x 13in","Quarto 215 x 275mm","10 x 14 in","11 x 17 in","Note 8.5 x 11in","Envelope #9","Envelope #10","Envelope #11","Envelope #12","Envelope #14","C Sheet","D Sheet","E Sheet","Envelope DL","Envelope C5","Envelope C3","Envelope C4","Envelope C6","Envelope C65","Envelope B4","Envelope B5","Envelope B6","Envelope 110 x 230mm","Envelope Monarch","6 3/4 Envelope","US Std Fanfold","German Std Fanfold","German Legal Fanfold","B4 (ISO)","Japanese Postcard","9 x 11in","10 x 11in","15 x 11in","Envelope Invite","RESERVED--DO NOT USE","RESERVED--DO NOT USE","Letter Extra 9","Legal Extra 9","Tabloid Extra","A4 Extra","Letter Transverse 8","A4 Transverse","Letter Extra Transverse 9 ","Super A","Super B","Letter Plus 8.5 x 12.69in","A4 Plus","A5 Transverse","B5 (JIS) Transverse","A3 Extra","A5 Extra","B5 (ISO)","A2","A3 Transverse","A3 Extra Transverse" 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE pc_filename AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filename" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE pc_lptformname AS CHARACTER FORMAT "X(256)":U 
     LABEL "Form name" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE pc_lptname AS CHARACTER FORMAT "X(256)":U 
     LABEL "Printer" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE pi_lptnumcopies AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "# of copies" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE pi_lptPaperlength AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Length" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE pi_lptpaperWidth AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Width" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE pi_lptscale AS INTEGER FORMAT ">>9%":U INITIAL 100 
     LABEL "Scale" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE pi_lptduplex AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Simplex (no duplex)", 1,
"Horizontal", 2,
"Vertical", 3
     SIZE 28 BY 2.86 NO-UNDO.

DEFINE VARIABLE pi_lptorientation AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Portrait", 1,
"Landscape", 2
     SIZE 16 BY 1.67 NO-UNDO.

DEFINE RECTANGLE rect_duplex_mode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 31 BY 3.57.

DEFINE RECTANGLE rect_options
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 63 BY 3.33 TOOLTIP "Report Options".

DEFINE RECTANGLE rect_orientation
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 25 BY 2.38.

DEFINE RECTANGLE rect_paper_size
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 31 BY 4.52.

DEFINE RECTANGLE rect_print_settings
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 63 BY 11.67 TOOLTIP "Print Settings".

DEFINE VARIABLE pi_lptcollate AS LOGICAL INITIAL yes 
     LABEL "Collate" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE pi_lptcolor AS LOGICAL INITIAL no 
     LABEL "Color" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE pl_defaultlpt AS LOGICAL INITIAL yes 
     LABEL "Use default printer" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .67 NO-UNDO.

DEFINE VARIABLE tog_async AS LOGICAL INITIAL no 
     LABEL "Run report asynchronously" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tog_display AS LOGICAL INITIAL no 
     LABEL "Do not display EUD" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tog_instance AS LOGICAL INITIAL no 
     LABEL "Use current instance of EUD" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE tog_launch AS LOGICAL INITIAL no 
     LABEL "Always launch new EUD" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE tog_open AS LOGICAL INITIAL no 
     LABEL "Keep EUD open" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tog_print AS LOGICAL INITIAL yes 
     LABEL "Print generated report" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME actuate_printdlg
     pc_filename AT ROW 1.24 COL 11 COLON-ALIGNED
     btn_Browse AT ROW 1.24 COL 55
     tog_launch AT ROW 3.38 COL 4
     tog_async AT ROW 3.38 COL 36
     tog_instance AT ROW 4.33 COL 4
     tog_display AT ROW 4.33 COL 36
     tog_print AT ROW 5.33 COL 4
     tog_open AT ROW 5.33 COL 36
     pl_defaultlpt AT ROW 7.19 COL 7
     pc_lptname AT ROW 8.14 COL 15 COLON-ALIGNED
     pi_lptnumcopies AT ROW 9.33 COL 15 COLON-ALIGNED
     pi_lptscale AT ROW 10.52 COL 15 COLON-ALIGNED
     pc_lptformname AT ROW 11.67 COL 15 COLON-ALIGNED
     pi_lptcolor AT ROW 13.14 COL 7
     pi_lptcollate AT ROW 14.33 COL 7
     pi_lptorientation AT ROW 16.24 COL 7 NO-LABEL
     pi_lptduplex AT ROW 10.05 COL 36 NO-LABEL
     pi_lptpapersize AT ROW 14.33 COL 34 COLON-ALIGNED NO-LABEL
     pi_lptPaperlength AT ROW 15.52 COL 42 COLON-ALIGNED
     pi_lptpaperWidth AT ROW 16.71 COL 42 COLON-ALIGNED
     Btn_OK AT ROW 1.24 COL 68
     Btn_Cancel AT ROW 2.67 COL 68
     rect_duplex_mode AT ROW 9.57 COL 34
     rect_options AT ROW 2.91 COL 3
     rect_orientation AT ROW 15.76 COL 6
     rect_paper_size AT ROW 13.62 COL 34
     rect_print_settings AT ROW 6.67 COL 3
     "Report options" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.67 COL 4
     "Print settings" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 6.48 COL 4
     "Duplex mode" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 9.33 COL 36
     "Paper Size" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 13.38 COL 36
     "Orientation" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 15.52 COL 7
     SPACE(65.99) SKIP(2.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Print Actuate Report"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX actuate_printdlg
   Custom                                                               */
ASSIGN 
       FRAME actuate_printdlg:SCROLLABLE       = FALSE
       FRAME actuate_printdlg:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN pi_lptPaperlength IN FRAME actuate_printdlg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN pi_lptpaperWidth IN FRAME actuate_printdlg
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME actuate_printdlg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL actuate_printdlg actuate_printdlg
ON GO OF FRAME actuate_printdlg /* Print Actuate Report */
DO:
  DEFINE VARIABLE pi_Status    AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE pi_options   AS INTEGER   NO-UNDO.
  
  /* Assign fields from the screen */
  ASSIGN pc_filename 
         pc_lptformname 
         pc_lptname 
         tog_async 
         tog_display 
         tog_instance 
         tog_launch 
         tog_open 
         tog_print
         pi_lptcollate 
         pi_lptcolor 
         pi_lptduplex 
         pi_lptPaperlength 
         pi_lptpapersize 
         pi_lptpaperWidth 
         pi_lptscale 
         pi_lptnumcopies 
         pi_lptorientation
         pl_defaultlpt.

  /* Assemble report options */
  IF tog_launch   THEN pi_Options = pi_Options + {&AC_REQ_LAUNCH_ALWAYS}.
  IF tog_instance THEN pi_Options = pi_Options + {&AC_REQ_LOCAL}.
  IF tog_print    THEN pi_Options = pi_Options + {&AC_REQ_PRINT}.
  IF tog_async    THEN pi_Options = pi_Options + {&AC_REQ_ASYNC}.
  IF tog_display  THEN pi_Options = pi_Options + {&AC_REQ_HIDE}.  
  IF tog_open     THEN pi_Options = pi_Options + {&AC_REQ_STAY_OPEN}.

/* DEBUG message
  MESSAGE "pc_filename: " pc_filename skip
          "pi_options: " pi_options skip
          "pc_lptname: " pc_lptname skip
          "pi_lptnumcopies: " pi_lptnumcopies skip
          "pi_lptorientation: " pi_lptorientation skip
          "pi_lptscale: " pi_lptscale skip
          "pi_lptcolor: " pi_lptcolor skip
          "pi_lptduplex: " pi_lptduplex skip
          "pi_lptcollate: " pi_lptcollate skip
          "pc_lptformname: " pc_lptformname skip
          "pi_lptpapersize: " pi_lptpapersize skip
          "pi_lptpaperlength: " pi_lptpaperlength skip
          "pi_lptpaperwidth: " pi_lptpaperwidth skip
          "pl_defaultlpt: " pl_defaultlpt
          VIEW-AS ALERT-BOX INFORMATION.
*/
          
  /* Print the Actuate report */       
  RUN samples/actuate/acprtrpt.p (INPUT pc_filename,
                  INPUT pi_options, 
                  INPUT pc_lptname, 
                  INPUT pi_lptnumcopies, 
                  INPUT pi_lptorientation, 
                  INPUT pi_lptscale,  
                  INPUT (IF pi_lptcolor THEN 1 ELSE 0),
                  INPUT pi_lptduplex, 
                  INPUT (IF pi_lptcollate THEN 1 ELSE 0),
                  INPUT pc_lptFormName,
                  INPUT (IF pi_lptPaperSize:SCREEN-VALUE NE "(specify)" THEN pi_lptPaperSize:LOOKUP(pi_lptPaperSize) - 1 ELSE 0), 
                  INPUT pi_lptPaperLength, 
                  INPUT pi_lptPaperWidth,
                  INPUT (IF pl_defaultlpt THEN 1 ELSE 0),
                  OUTPUT pi_Status).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL actuate_printdlg actuate_printdlg
ON WINDOW-CLOSE OF FRAME actuate_printdlg /* Print Actuate Report */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Browse actuate_printdlg
ON CHOOSE OF btn_Browse IN FRAME actuate_printdlg /* Browse... */
DO:
  DEFINE VAR l_ok              AS LOGICAL   NO-UNDO.
  DEFINE VAR filename          AS CHARACTER NO-UNDO.
  DEFINE VAR Filter_NameString AS CHARACTER NO-UNDO.
  DEFINE VAR Filter_FileSpec   AS CHARACTER NO-UNDO.

  /* Initialize the file filters, for special cases. */
  ASSIGN Filter_NameString = "Report document(*.roi)"
         Filter_FileSpec   = "*.roi".

  /* Ask for a file name. NOTE: File-names to run must exist */                          
  filename = pc_filename:SCREEN-VALUE.
  SYSTEM-DIALOG GET-FILE filename
      TITLE    "Print Actuate report"
      FILTERS  Filter_NameString   Filter_FileSpec             
      MUST-EXIST
      UPDATE   l_ok IN WINDOW {&WINDOW-NAME}.  
  IF l_ok THEN pc_filename:SCREEN-VALUE = filename.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK actuate_printdlg
ON CHOOSE OF Btn_OK IN FRAME actuate_printdlg /* OK */
DO:
  ASSIGN pc_filename.
  IF pc_filename EQ ? OR pc_filename EQ "" THEN DO:
    MESSAGE "Enter the name of a ROI report to print" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY" TO pc_filename.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pi_lptpapersize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pi_lptpapersize actuate_printdlg
ON VALUE-CHANGED OF pi_lptpapersize IN FRAME actuate_printdlg
DO:
  IF SELF:SCREEN-VALUE = "(specify)" THEN
    ASSIGN pi_lptpaperlength:SENSITIVE = TRUE
           pi_lptpaperwidth:SENSITIVE  = TRUE.
  ELSE
    ASSIGN pi_lptpaperlength:SENSITIVE = FALSE
           pi_lptpaperwidth:SENSITIVE  = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK actuate_printdlg 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI actuate_printdlg _DEFAULT-DISABLE
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
  HIDE FRAME actuate_printdlg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI actuate_printdlg _DEFAULT-ENABLE
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
  DISPLAY pc_filename tog_launch tog_async tog_instance tog_display tog_print 
          tog_open pl_defaultlpt pc_lptname pi_lptnumcopies pi_lptscale 
          pc_lptformname pi_lptcolor pi_lptcollate pi_lptorientation 
          pi_lptduplex pi_lptpapersize pi_lptPaperlength pi_lptpaperWidth 
      WITH FRAME actuate_printdlg.
  ENABLE rect_duplex_mode rect_options rect_orientation rect_paper_size 
         rect_print_settings pc_filename btn_Browse tog_launch tog_async 
         tog_instance tog_display tog_print tog_open pl_defaultlpt pc_lptname 
         pi_lptnumcopies pi_lptscale pc_lptformname pi_lptcolor pi_lptcollate 
         pi_lptorientation pi_lptduplex pi_lptpapersize Btn_OK Btn_Cancel 
      WITH FRAME actuate_printdlg.
  VIEW FRAME actuate_printdlg.
  {&OPEN-BROWSERS-IN-QUERY-actuate_printdlg}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


