/************************************************************************************
        PROCEDURE: mndriver.p

        PURPOSE:   Menu Driver

        SYNTAX:    "RUN mndriver.p"

        REMARKS:   This code handles menu options and selections within the selection
                   lists for topics and procedures.
                   Internal procedure on_off enables and disables widgets to handle
                   execution of modal subroutines.

        PARAMETERS:

        AUTHORS:   Progress Consulting 
        DATE:      March 1993

        LAST INSPECTED:
        INSPECTED BY:

************************************************************************************/
/* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.            */

DEF SHARED TEMP-TABLE mmst
  FIELD menu_nbr   AS integer FORMAT ">9"
  FIELD menu_name  AS character FORMAT "x(36)"
  FIELD menu_total_items AS integer
  FIELD menu_group AS character FORMAT "x(60)"
  INDEX name IS UNIQUE 
     menu_name ASC
  INDEX nbr  IS UNIQUE PRIMARY
     menu_nbr ASC.

DEF SHARED TEMP-TABLE mdet
  FIELD menu_nbr      AS integer FORMAT ">9"
  FIELD menu_name     AS character FORMAT "x(36)"
  FIELD menu_line_nbr AS integer FORMAT ">9"
  FIELD menu_item     AS character  FORMAT "x(36)"
  FIELD menu_mnemonic AS character  FORMAT "x(2)"
  FIELD menu_program  AS character  FORMAT "x(64)"
  FIELD menu_help_program AS character  FORMAT "x(64)"
  FIELD menu_help_text AS character FORMAT "x(36)" EXTENT 14
  FIELD menu_group    AS character  FORMAT "x(60)"
  FIELD program       AS character  FORMAT "x(60)"
  FIELD menu_program_type AS integer  FORMAT ">9"
  FIELD update_pgm    AS character  FORMAT "x(60)"
  FIELD activities    AS character  FORMAT "x(60)"
  INDEX mnem IS UNIQUE
    menu_mnemonic ASC
  INDEX nbr  IS UNIQUE PRIMARY
    menu_nbr      ASC
    menu_line_nbr ASC.
 
DEF SHARED BUFFER sys_mmst   FOR mmst.
DEF SHARED BUFFER sys_mdet   FOR mdet.
DEF        BUFFER x_sys_mdet FOR mdet.
DEF SHARED VAR help_run AS LOGICAL NO-UNDO.

/*************************** Variable Categories ***************************/
/******************************** Character ********************************/
DEF VAR current_menu  AS CHAR FORMAT "x(36)"                     NO-UNDO.
DEF VAR menu_current  AS CHAR FORMAT "x(20)" INITIAL "Main Menu" NO-UNDO.
DEF VAR menu_select   AS CHAR FORMAT "x(37)" EXTENT 10           NO-UNDO.
DEF VAR menu_text     AS CHAR FORMAT "x(36)" EXTENT 15           NO-UNDO.
DEF VAR load_file     AS CHAR                                    NO-UNDO.
DEF VAR temp_progr    LIKE sys_mdet.menu_program                 NO-UNDO.
DEF VAR temp_prog     LIKE sys_mdet.menu_program                 NO-UNDO.

DEF SHARED VAR g_current_rev   AS CHAR                         NO-UNDO.
DEF SHARED VAR g_func          AS CHAR                         NO-UNDO.
DEF SHARED VAR g_group         AS CHAR FORMAT "x(12)"          NO-UNDO.
DEF SHARED VAR g_site#_name    AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF SHARED VAR g_nodename      AS CHAR                         NO-UNDO.
DEF SHARED VAR g_release_level AS CHAR FORMAT "x(15)"          NO-UNDO.
DEF SHARED VAR g_time          AS CHAR FORMAT "X(9)"           NO-UNDO.
DEF SHARED VAR g_screen        AS CHAR FORMAT "x(75)"          NO-UNDO.
DEF SHARED VAR g_userid        AS CHAR                         NO-UNDO.
DEF SHARED VAR g_mesg_upd      AS CHAR FORMAT "x(10)"          NO-UNDO.
DEF SHARED VAR g_save_rev      AS CHAR                         NO-UNDO.
DEF SHARED VAR g_file_del      AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF SHARED VAR g_mnemonic      LIKE sys_mdet.menu_mnemonic FORMAT "xxx"
    LABEL "Enter Expert Code or Browse Key for list of choices" NO-UNDO.

/********************************  Integer  ********************************/
DEF SHARED VAR cntr            AS INT                      NO-UNDO.
DEF SHARED VAR i               AS INT                      NO-UNDO.
DEF SHARED VAR menu_key        AS INT FORMAT ">9" EXTENT 4 NO-UNDO.
DEF SHARED VAR menu_level      AS INT FORMAT ">9"          NO-UNDO.
DEF SHARED VAR g_timeout       AS INT FORMAT ">9"          NO-UNDO.
DEF SHARED VAR g_scrlength     AS INT                      NO-UNDO.
DEF SHARED VAR g_pause         AS INT FORMAT ">9"          NO-UNDO.
DEF        VAR menu_counter    AS INT FORMAT ">9"          NO-UNDO.
DEF        VAR total_items     AS INT                      NO-UNDO.
DEF        VAR temp_length     AS INT                      NO-UNDO.

/********************************   Date    ********************************/
DEF SHARED VAR g_today          AS DATE NO-UNDO.
DEF SHARED VAR g_save_rev_date  AS DATE NO-UNDO.

/********************************  Logical  ********************************/
DEF SHARED VAR g_hilite        AS LOGICAL          NO-UNDO.
DEF SHARED VAR g_hotkey        AS LOGICAL INIT yes NO-UNDO.
DEF SHARED VAR g_menuhelp      AS LOGICAL INIT yes NO-UNDO.
DEF SHARED VAR g_exit          AS LOGICAL          NO-UNDO.
DEF        VAR first_letter    AS LOGICAL INIT YES NO-UNDO.

/***************************************************************************\
\***************************************************************************/

DEF SHARED FRAME menu_header.

FORM  "User ID:"                  AT 3
      g_userid VIEW-AS TEXT
      g_release_level             AT 35 VIEW-AS TEXT
      g_today                     AT 69 VIEW-AS TEXT skip
      "System:"                   AT 3
      g_nodename VIEW-AS TEXT
      g_site#_name                AT 25 VIEW-AS TEXT
      g_time                      AT 68 VIEW-AS TEXT
      WITH FRAME menu_header WIDTH 80 ROW 1 no-labels overlay
      TITLE COLOR normal "Procedure Library".

FORM  menu_text VIEW-AS TEXT SKIP(1)
      WITH FRAME menu_help_screen WIDTH 38 ROW 5 no-labels overlay.

FORM  menu_select 
      WITH FRAME menu_choices WIDTH 41
      NO-LABELS overlay TITLE COLOR normal current_menu
      VIEW-AS DIALOG-BOX.
  
ASSIGN
   FRAME menu_choices:ROW = 6
   FRAME menu_choices:COLUMN = 41.

/***********************************************************************
    Mainloop gets the menu master record and all related detail records
    to set up the selection frame.
***********************************************************************/

mainloop:
DO WHILE TRUE:
  FIND FIRST sys_mmst
       WHERE sys_mmst.menu_nbr = menu_key[menu_level]
       NO-LOCK NO-ERROR.        /* find first menu master record */
       
  IF AVAILABLE sys_mmst THEN DO:
    assign menu_counter = 0
           menu_select = " "
           cntr = 1.
    FOR EACH sys_mdet WHERE sys_mdet.menu_nbr = sys_mmst.menu_nbr NO-LOCK:
       assign
          menu_counter = menu_counter + 1
          menu_select[menu_counter] = sys_mdet.menu_item.
    END.
    assign 
       total_items = menu_counter + 1
       menu_select[menu_counter + cntr] =
           if sys_mmst.menu_nbr > 1 then "Return" else "Quit"
       current_menu = sys_mmst.menu_name.
  END.
  i = 1.
   
/***********************************************************************
    Selection loops the user through selecting menu options and
    sets level variables to keep track of where the user is.
***********************************************************************/
   selection:                                        /* selection loop       */
   DO WHILE TRUE:
      FIND FIRST sys_mdet NO-LOCK
           WHERE sys_mdet.menu_item = menu_select[i] NO-ERROR.  /* find detl */
/**************************************************************************
    There is no need to check for record availability here because the
    menu selection is the first thing checked.  There will be no record if
    they are on "Quit" or "Return".
**************************************************************************/

      IF g_menuhelp THEN                            /* if user wants help text*/
      DO:
        IF CAN-DO("Quit,Return",menu_select[i]) THEN  /* if not quit, retn*/
        DO:
             menu_text = " ".                        /* init                 */
          ASSIGN menu_text[8] = IF menu_select[i] = "Quit" THEN
            ("                  " + menu_select[i]) ELSE
            ("       " + menu_select[i] + " to Previous Menu")
            menu_text[7] = "       ***********************"
            menu_text[9] = "       ***********************".
        END.
        ELSE DO:
          menu_counter = 1.
          DO menu_counter = 1 TO 14:
              menu_text[menu_counter] = sys_mdet.menu_help_text[menu_counter].
          END.
        END.
        DISPLAY menu_text
          WITH FRAME menu_help_screen. /* display help text    */
      END. /* g_menuhelp = YES */

/***********************************************************************
    Timeloop puts the current time on the screen when a key is hit,
    and saves the function key that is hit.
***********************************************************************/

      IF NOT help_run THEN DO: 
         g_time = STRING(TIME,"HH:MM AM").           /* get current time */
         DISPLAY g_time WITH FRAME menu_header.      /* display time     */
      END.

      DISPLAY menu_select WITH FRAME menu_choices.
      IF KEYFUNCTION(LASTKEY) = "cursor-down" AND i <= total_items 
      THEN DO:
        IF i <> 1 THEN 
           DISPLAY menu_select[i - 1] DCOLOR 1 FGCOLOR 1
               WITH FRAME menu_choices.
        DISPLAY menu_select[i] DCOLOR 1 FGCOLOR 2
            WITH FRAME menu_choices.
        NEXT-PROMPT menu_select[i] WITH FRAME menu_choices.
      END.
      ELSE
      IF KEYFUNCTION(LASTKEY) = "cursor-up" AND i >= 1
      THEN DO:
        DISPLAY menu_select[i + 1] DCOLOR 1 FGCOLOR 1
            WITH FRAME menu_choices.
        DISPLAY menu_select[i] DCOLOR 1 FGCOLOR 2
            WITH FRAME menu_choices.
        NEXT-PROMPT menu_select[i] WITH FRAME menu_choices.
      END.
             
      CHOOSE FIELD menu_select  
        GO-ON (CURSOR-DOWN CURSOR-UP CTRL-P RETURN CTRL-G) 
        WITH FRAME menu_choices.

      g_func = KEYFUNCTION(LASTKEY).          /* get key functions      */
      HIDE MESSAGE.                           /* hide any leftover msgs */

/***********************************************************************
    This next section checks for cursor down.  If hilite bar is at the
    bottom, the hilite bar is moved to the top again.
***********************************************************************/

    IF g_func = "cursor-down" THEN
    DO:
      ASSIGN i = IF i < total_items THEN (i + 1) ELSE 1.
      IF SUBSTR(menu_select[i],1,1) = " " THEN i = i + 1.
      NEXT selection.
    END.

/***********************************************************************
    This section checks for cursor up.  If the hilite bar is at the top,
    the hilite bar is moved to the bottom.
***********************************************************************/
    IF g_func = "cursor-up" THEN
    DO:
      i = IF i > 1 THEN i - 1
          ELSE total_items.
      NEXT selection.
    END.

/***********************************************************************
    This section checks for F4 of menu selection of RETURN.  It sets the
    the menu level to to the previous level.
***********************************************************************/
    IF g_func = "END-ERROR" OR FRAME-VALUE = "Return" THEN
    DO:
       IF help_run THEN DO:
          HIDE FRAME menu_choices NO-PAUSE.
          RETURN.
       END.
       IF menu_level > 1 THEN menu_level = (menu_level - 1).
       i = 2.
       LEAVE selection.
    END.

/***********************************************************************
    This section checks for ENTER or GO.  If the the menu selection is
    quit then the user is taken out of the system.  If the program field
    of the current detail is spaces, then the user has chosen another
    menu and placed in that menu if they user group allows entry to
    the chosen menu.  If the program field is not blank, the user group
    is allowed access, and the program can be found, the program is
    executed. This
***********************************************************************/
    IF CAN-DO("return,go",g_func) OR KEYLABEL(LASTKEY) = "CTRL-P"
    THEN DO:

      IF FRAME-VALUE = "quit" THEN DO:
         HIDE FRAME menu_help_screen No-PAUSE.
         HIDE FRAME menu_header NO-PAUSE.
         RETURN.
      END.
      FIND FIRST sys_mdet WHERE 
                 sys_mdet.menu_nbr      = sys_mmst.menu_nbr AND
                 sys_mdet.menu_item     = FRAME-VALUE
                 /*
                 sys_mdet.menu_line_nbr = i
                 */
                 NO-LOCK NO-ERROR.
      IF sys_mdet.menu_program = " " AND KEYLABEL(LASTKEY) <> "CTRL-P"
      THEN DO:
        IF CAN-DO(sys_mdet.menu_group,g_group) THEN
        DO:
          FIND FIRST sys_mmst
            WHERE sys_mmst.menu_name = sys_mdet.menu_item NO-LOCK NO-ERROR.
          IF AVAILABLE sys_mmst THEN
          DO:
            IF CAN-FIND(FIRST sys_mdet WHERE sys_mdet.menu_nbr =
              sys_mmst.menu_nbr USE-INDEX nbr ) THEN
            DO:
              ASSIGN
                 i = 2
                menu_level = menu_level + 1
                menu_key[menu_level] = sys_mmst.menu_nbr.
            END.     /*end can find*/
            ELSE
               MESSAGE "There are no choices available at this time". /* 1 */
            LEAVE selection.
          END.      /*end if avail sys_mmst*/
          ELSE
          DO:
            MESSAGE "There are not choices available at this time". 
            LEAVE selection.
          END.     /*end else do*/
        END.
        ELSE
        DO:
          MESSAGE "You do not have clearance to run this program".
          NEXT selection.
        END.      /* end else do*/
      END.
      ELSE DO:
        /* run menu program*/
        ASSIGN
            g_mnemonic = " ".
        runprogs:
        DO WHILE TRUE:
          IF KEYLABEL(LASTKEY) = "CTRL-P" OR g_mnemonic <> " " THEN
          DO:

            IF g_mnemonic = " " THEN
               UPDATE g_mnemonic LABEL "Enter Fast Key Code" AUTO-RETURN
                    WITH FRAME g_mne OVERLAY VIEW-AS DIALOG-BOX SIDE-LABELS
                    ROW 7 CENTERED.
            IF g_mnemonic = "mm" THEN DO:
              ASSIGN
                menu_level = 1
                i = 2.
              LEAVE selection.
            END.
            ELSE
            IF g_mnemonic = "qt" THEN RETURN.
            FIND sys_mdet WHERE sys_mdet.menu_mnemonic = g_mnemonic
              NO-LOCK NO-ERROR.
            IF NOT AVAILABLE sys_mdet THEN
            DO:
              message "No selections available".
              NEXT selection.
            END.     /*end if avail sys_mdet*/
          END.

          HIDE ALL.
          temp_prog = sys_mdet.menu_program.

          IF CAN-DO(sys_mdet.program,g_userid) THEN
          DO:
            test1:
            DO WHILE TRUE:
              ASSIGN
                    temp_length = LENGTH(temp_prog) - 2
                 temp_progr  = substring(temp_prog,1,temp_length) + ".r".
              IF SEARCH(temp_prog) = ? AND SEARCH(temp_progr) = ? THEN
              DO:
                MESSAGE "Program not found".
                NEXT selection.
              END.   /*end search*/
              ASSIGN
                 g_mnemonic = " "
                 g_screen = sys_mdet.menu_item
                 g_scrlength = length(g_screen)
                 g_exit = no.
              RUN VALUE(temp_prog).
              PUT SCREEN FILL(" ",80) ROW 3 COLUMN 1.
              HIDE ALL.
              HIDE MESSAGE NO-PAUSE.
              IF KEYFUNCTION(LASTKEY) = "END-ERROR" OR g_exit THEN
                NEXT selection.
              IF g_mnemonic <> " " THEN NEXT runprogs.
              NEXT selection.
            END.   /*end test1*/
          END.    /*end can-do*/
          ELSE
          DO:
            MESSAGE "You do not have clearance to run this program".
            NEXT selection.
          END.              /*end do*/
        END.   /*end runprogs*/
      END.
    END.
    MESSAGE "There are no choices available at this time.".
     NEXT selection.
  END.                                          /* end selection            */
  HIDE FRAME menu_help_screen.
  HIDE FRAME menu_choices.
  HIDE FRAME menu_choices_centered.
END.































