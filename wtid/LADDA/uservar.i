
/*uservar.i*/

{ladda\adestds.i}

DEFINE {1} SHARED VARIABLE user_dbname   AS CHARACTER           NO-UNDO.
DEFINE {1} SHARED VARIABLE user_dbtype   AS CHARACTER           NO-UNDO.
DEFINE {1} SHARED VARIABLE user_env      AS CHARACTER EXTENT 35 NO-UNDO.
/* NOTE: this variable-definition has to be in sync with adedict/dictvar.i */
DEFINE {1} SHARED VARIABLE user_filename AS CHARACTER           NO-UNDO.
DEFINE {1} SHARED VARIABLE user_hdr      AS CHARACTER           NO-UNDO.
DEFINE {1} SHARED VARIABLE user_menupos  AS INTEGER   INITIAL ? NO-UNDO.
DEFINE {1} SHARED VARIABLE user_path     AS CHARACTER           NO-UNDO.
DEFINE {1} SHARED VARIABLE user_trans    AS CHARACTER           NO-UNDO.
DEFINE {1} SHARED VARIABLE user_status   AS CHARACTER INITIAL ? NO-UNDO.
DEFINE {1} SHARED VARIABLE user_library  AS CHARACTER           NO-UNDO.

DEFINE {1} SHARED VARIABLE index-area-number AS INTEGER INITIAL 6 NO-UNDO.

DEFINE BUTTON btn_Ok     LABEL "OK"     {&STDPH_OKBTN} AUTO-GO.
DEFINE BUTTON btn_Cancel LABEL "Cancel" {&STDPH_OKBTN} AUTO-ENDKEY.

DEFINE {1} SHARED STREAM logfile.
DEFINE {1} SHARED VARIABLE logfile_open AS LOGICAL NO-UNDO INITIAL false.

/*Fernando  20020129-017*/
DEFINE {1} SHARED VARIABLE user_msg_count AS INTEGER NO-UNDO INITIAL 0.

/* kmcintos "Auditing support" */
DEFINE {1} SHARED VARIABLE user_overwrite AS LOGICAL   NO-UNDO.
DEFINE {1} SHARED VARIABLE user_commit    AS LOGICAL   NO-UNDO.
DEFINE {1} SHARED VARIABLE user_excepts   AS CHARACTER NO-UNDO.

/* for bug fix 20050930-006 */
DEFINE {1} SHARED VARIABLE user_longchar  AS LONGCHAR NO-UNDO.

&GLOBAL-DEFINE ERROR_ROLLBACK 151


&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN 
   DEFINE RECTANGLE rect_Btns {&STDPH_OKBOX}.
   DEFINE BUTTON    btn_Help LABEL "&Help" {&STDPH_OKBTN}.

   &GLOBAL-DEFINE   HLP_BTN  &HELP = "btn_Help"
   &GLOBAL-DEFINE   HLP_BTN_NAME btn_Help
   &GLOBAL-DEFINE   CAN_BTN  /* we have one but it's not passed to okrun.i */
   &GLOBAL-DEFINE   WIDG_SKIP SKIP({&VM_WIDG})
&ELSE
   &GLOBAL-DEFINE   HLP_BTN  /* no help for tty */
   &GLOBAL-DEFINE   HLP_BTN_NAME /* no help for tty */
   &GLOBAL-DEFINE   CAN_BTN  &CANCEL = "btn_Cancel" /* so btn can be centered */
   &GLOBAL-DEFINE   WIDG_SKIP SKIP /*Often don't need and can't afford blnklin*/
&ENDIF


/*&IF "{&WINDOW-SYSTEM}" <> "TTY"
 AND "{&DICTG}" <> "dictg"
 &THEN 
   /* Help context defines and the name of the help file */
   {prodict/admnhlp.i}
   &global-define ADM_HELP_FILE "adehelp/admin.hlp"  
&ENDIF*/

/* used to define the display name of the product. It used to be PROGRESS,
and now we are changigng it to OpenEdge. Creating define so that the next time
we change it, we don't have to go through changing all necessary source
files again 
*/
&GLOBAL-DEFINE PRO_DISPLAY_NAME OpenEdge
