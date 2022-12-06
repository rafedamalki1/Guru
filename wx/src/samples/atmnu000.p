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
/* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

/* Code Start */  

DEF NEW SHARED TEMP-TABLE f_help
  FIELD fld_name   AS CHAR
  FIELD pgm_name   AS CHAR
  INDEX fld_nam IS UNIQUE
     fld_name ASC.
     
DEF NEW SHARED TEMP-TABLE mmst
  FIELD menu_nbr   AS integer FORMAT ">9"
  FIELD menu_name  AS character FORMAT "x(36)"
  FIELD menu_total_items AS integer
  FIELD menu_group AS character FORMAT "x(60)"
  INDEX name IS UNIQUE 
     menu_name ASC
  INDEX nbr  IS UNIQUE PRIMARY
     menu_nbr ASC.

DEF NEW SHARED TEMP-TABLE mdet
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

DEF NEW SHARED BUFFER sys_mmst FOR mmst.
DEF NEW SHARED BUFFER sys_mdet FOR mdet.    
DEF NEW SHARED BUFFER fld_help FOR f_help.

DEF NEW SHARED VAR help_run AS LOGICAL NO-UNDO.
DEF NEW SHARED VAR s_cut AS CHAR NO-UNDO.
/*************************** Variable Categories ***************************/
/******************************** Character ********************************/
DEF VAR load_file     AS CHAR                          NO-UNDO.

/********************************  Integer  ********************************/
DEF NEW SHARED VAR cntr            AS INTEGER                      NO-UNDO.
DEF NEW SHARED VAR i               AS INTEGER                      NO-UNDO. /*selection cntr*/
DEF NEW SHARED VAR menu_key        AS INTEGER FORMAT ">9" EXTENT 4 NO-UNDO.
DEF NEW SHARED VAR menu_level      AS INTEGER FORMAT ">9"          NO-UNDO.

/********************************  Logical  ********************************/

/***************************************************************************\
\***************************************************************************/

/*************************** Variable Categories ***************************/
DEF NEW GLOBAL SHARED VAR g_hilite     AS LOGICAL NO-UNDO.

DEF NEW GLOBAL SHARED VAR g_hotkey     AS LOGICAL INIT yes NO-UNDO.

DEF NEW GLOBAL SHARED VAR g_menuhelp   AS LOGICAL INIT yes NO-UNDO.
DEF NEW GLOBAL SHARED VAR g_mnemonic   LIKE sys_mdet.menu_mnemonic FORMAT "xxx"
    LABEL "Enter Expert Code or Browse Key for list of choices" NO-UNDO.

DEF NEW GLOBAL SHARED VAR g_timeout    AS INT FORMAT ">9" NO-UNDO.

/******************************** Character ********************************/
DEF NEW GLOBAL SHARED VAR g_current_rev    AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR g_func           AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR g_group          AS CHAR FORMAT "x(12)" NO-UNDO.
DEF NEW GLOBAL SHARED VAR g_site#_name     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF NEW GLOBAL SHARED VAR g_nodename       AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR g_release_level  AS CHAR FORMAT "x(15)" NO-UNDO.
/* main menu time display */
DEF NEW GLOBAL SHARED VAR g_time           AS CHAR FORMAT "X(9)" NO-UNDO.
/* screen title */
DEF NEW GLOBAL SHARED VAR g_screen         AS CHAR FORMAT "x(75)" NO-UNDO.
/* main menu user id */
DEF NEW GLOBAL SHARED VAR g_userid         AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR g_mesg_upd       AS CHAR FORMAT "x(10)" NO-UNDO.
/* holder of program revision level and date */
DEF NEW GLOBAL SHARED VAR g_save_rev       AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR g_file_del       AS CHAR FORMAT "x(30)" NO-UNDO.

/********************************   Date    ********************************/
DEF NEW GLOBAL SHARED VAR g_today          AS DATE NO-UNDO.
/* holder of program revision date */
DEF NEW GLOBAL SHARED VAR g_save_rev_date  AS DATE NO-UNDO.

/********************************  Decimal  ********************************/

/********************************  Integer  ********************************/
/* screen title length */
DEF NEW GLOBAL SHARED VAR g_scrlength      AS INT                NO-UNDO.
DEF NEW GLOBAL SHARED VAR g_pause          AS INT FORMAT ">9"    NO-UNDO.

/********************************  Logical  ********************************/
/* program exit code */
DEF NEW GLOBAL SHARED VAR g_exit           AS LOGICAL NO-UNDO.
/* program link command forward */
DEF NEW GLOBAL SHARED VAR g_next           AS LOGICAL NO-UNDO.
/* program link command backward */
DEF NEW GLOBAL SHARED VAR g_prev           AS LOGICAL NO-UNDO.

/********************************   Recid   ********************************/
DEF NEW SHARED FRAME menu_header.

FORM        "User ID:"                  AT 3
	    g_userid VIEW-AS TEXT
	    g_release_level             AT 35 VIEW-AS TEXT
	    g_today                     AT 69 VIEW-AS TEXT skip
	    "System:"                   AT 3
	    g_nodename VIEW-AS TEXT
	    g_site#_name                AT 25 VIEW-AS TEXT
	    g_time                      AT 68 VIEW-AS TEXT
  WITH FRAME menu_header WIDTH 80 ROW 1 no-labels overlay
  TITLE COLOR normal "Procedure Library".


/*INPUT FROM TERMINAL MAP upper.*/
/***********************************************************************
    Main program logic begins here.
***********************************************************************/

ASSIGN
  cntr = 0                                          /* init counter         */
  menu_key[1] = 1                                   /* set first menu key   */
  menu_level = 1                                    /* set current menu level*/
  i = 2                                             /* init select counter  */
  g_time = STRING(TIME,"HH:MM AM").                 /* set time             */

DISPLAY g_userid
	g_release_level
	g_nodename
	g_site#_name
	g_time
	g_today
  WITH FRAME menu_header.                           /* display header       */

FOR EACH sys_mmst: DELETE sys_mmst. END.
FOR EACH sys_mdet: DELETE sys_mdet. END.

load_file = SEARCH("samples/sys_mmst.dat").
IF load_file = ? THEN DO:
    MESSAGE "Unable to find 'sys_mmst.dat' in PROPATH".
    STOP.
END.
ELSE DO:   
    INPUT FROM VALUE(load_file) NO-ECHO.
    REPEAT:		
        CREATE sys_mmst.
        IMPORT sys_mmst.
        IF sys_mmst.menu_name = "" THEN DELETE sys_mmst.
    END. /* REPEAT */
    INPUT CLOSE.
END.

load_file = SEARCH("samples/sys_mdet.dat").
IF load_file = ? THEN DO:
    MESSAGE "Unable to find 'sys_mdet.dat' in PROPATH".
    STOP.
END.
ELSE DO:   
    INPUT FROM VALUE(load_file) NO-ECHO.
    REPEAT:		
        CREATE sys_mdet.
        IMPORT sys_mdet.
        IF sys_mdet.menu_line_nbr = 0 THEN DO:
           DELETE sys_mdet.
           LEAVE.
        END.
    END. /* REPEAT */
    INPUT CLOSE.
END.

load_file = SEARCH("samples/help.dat").
IF load_file = ? THEN DO:
    MESSAGE "Unable to find 'help.dat' in PROPATH".
    STOP.
END.
ELSE DO:   
    INPUT FROM VALUE(load_file) NO-ECHO.
    REPEAT:		
        CREATE fld_help.
        IMPORT fld_help.
    END. /* REPEAT */
    INPUT CLOSE.
END.

RUN samples/atmnu001.p.




















