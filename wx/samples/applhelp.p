/************************************************************************************
	PROCEDURE: applhelp.p

	PURPOSE:   Super Help program

	SYNTAX:    RUN samples/applhelp.p

	REMARKS:   This code displays a menu and executes a program or action

	PARAMETERS:

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.         */
 
 
/* Code Start */ 
{samples/helpvar.i}

/*
DEFINE            VAR c               AS CHAR NO-UNDO.
DEFINE            VAR i               AS INT NO-UNDO.
*/

/*.......................................................................*/


DEF SHARED TEMP-TABLE f_help
  FIELD fld_name   AS CHAR
  FIELD pgm_name   AS CHAR
  INDEX fld_nam IS UNIQUE
     fld_name ASC.

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
 
DEF SHARED BUFFER sys_mmst FOR mmst.
DEF SHARED BUFFER sys_mdet FOR mdet.
DEF SHARED BUFFER fld_help FOR f_help.



/*************************** Variable Categories ***************************/
DEF  SHARED VAR g_hilite     AS LOGICAL NO-UNDO.

DEF  SHARED VAR g_hotkey     AS LOGICAL INIT yes NO-UNDO.

DEF  SHARED VAR g_menuhelp   AS LOGICAL INIT yes NO-UNDO.
DEF  SHARED VAR g_mnemonic   AS CHAR FORMAT "XX"
    LABEL "Enter Expert Code or Browse Key for list of choices" NO-UNDO.

DEF  SHARED VAR g_timeout    AS INT FORMAT ">9" NO-UNDO.

/******************************** Character ********************************/
DEF  SHARED VAR g_current_rev    AS CHAR NO-UNDO.
DEF  SHARED VAR g_func           AS CHAR NO-UNDO.
DEF  SHARED VAR g_group          AS CHAR FORMAT "x(12)" NO-UNDO.
DEF  SHARED VAR g_site#_name     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF  SHARED VAR g_nodename       AS CHAR NO-UNDO.
DEF  SHARED VAR g_release_level  AS CHAR FORMAT "x(15)" NO-UNDO.
/* main menu time display */
DEF  SHARED VAR g_time           AS CHAR NO-UNDO.
/* screen title */
DEF  SHARED VAR g_screen         AS CHAR FORMAT "x(75)" NO-UNDO.
/* main menu user id */
DEF  SHARED VAR g_userid         AS CHAR NO-UNDO.
DEF  SHARED VAR g_mesg_upd       AS CHAR FORMAT "x(10)" NO-UNDO.
/* holder of program revision level and date */
DEF  SHARED VAR g_save_rev       AS CHAR NO-UNDO.
DEF  SHARED VAR g_file_del       AS CHAR FORMAT "x(30)" NO-UNDO.

/********************************   Date    ********************************/
DEF  SHARED VAR g_today          AS DATE NO-UNDO.
/* holder of program revision date */
DEF  SHARED VAR g_save_rev_date  AS DATE NO-UNDO.

/********************************  Decimal  ********************************/

/********************************  Integer  ********************************/
/* screen title length */
DEF  SHARED VAR g_scrlength      AS INT                NO-UNDO.
DEF  SHARED VAR g_pause          AS INT FORMAT ">9"    NO-UNDO.

/********************************  Logical  ********************************/
/* program exit code */
DEF  SHARED VAR g_exit           AS LOGICAL NO-UNDO.
/* program link command forward */
DEF  SHARED VAR g_next           AS LOGICAL NO-UNDO.
/* program link command backward */
DEF  SHARED VAR g_prev           AS LOGICAL NO-UNDO.

/********************************   Recid   ********************************/

/***************************************************************************\
\***************************************************************************/


DEFINE NEW        SHARED VARIABLE fnum       AS LOGICAL        NO-UNDO.
DEFINE NEW        SHARED VARIABLE fdb        AS CHAR FORMAT "x(12)" NO-UNDO.
DEFINE NEW        SHARED VARIABLE ffile      AS CHAR FORMAT "x(32)" NO-UNDO.
DEFINE NEW        SHARED VARIABLE ffield     AS CHAR FORMAT "x(32)" NO-UNDO.
DEFINE NEW        SHARED VARIABLE fformat    AS CHAR                NO-UNDO.
DEFINE NEW        SHARED VARIABLE flabel     AS CHAR                NO-UNDO.
DEFINE NEW        SHARED VARIABLE fdesc      AS CHAR                NO-UNDO.
DEFINE NEW        SHARED VARIABLE fdtype     AS CHAR                NO-UNDO.
DEFINE NEW        SHARED VARIABLE dfield     AS CHAR  NO-UNDO.
DEFINE NEW        SHARED VARIABLE xfield     AS CHAR  NO-UNDO.
DEFINE NEW        SHARED VARIABLE xlabel     AS CHAR  NO-UNDO.
DEFINE NEW        SHARED VARIABLE yfield     AS CHAR  NO-UNDO.
DEFINE NEW        SHARED VARIABLE ydtype     AS CHAR  NO-UNDO.

DEFINE NEW        SHARED VARIABLE expr       AS INT   NO-UNDO. 

DEFINE NEW        SHARED VARIABLE help_run   AS LOG   NO-UNDO.

DEF SHARED VAR cntr            AS INTEGER                      NO-UNDO.
DEF SHARED VAR i               AS INTEGER                      NO-UNDO.
DEF SHARED VAR menu_key        AS INTEGER FORMAT ">9" EXTENT 4 NO-UNDO.
DEF SHARED VAR menu_level      AS INTEGER FORMAT ">9"          NO-UNDO.
DEF SHARED VAR s_cut           AS CHAR                         NO-UNDO.
/*
   List of fields that have specific programs to be run to retrieve
   possible values
*/

FIND fld_help WHERE fld_help.fld_name = FRAME-FIELD NO-LOCK NO-ERROR.
IF AVAILABLE fld_help THEN DO:
   RUN VALUE(fld_help.pgm_name).
   RETURN.
END.


ASSIGN
  g_menuhelp = NO
  cntr = 0                                          /* init counter         */
  menu_key[2] = 2                                   /* set first menu key   */
  menu_level = 2                                    /* set current menu level*/
  i = 2                                             /* init select counter  */
  g_time = STRING(TIME,"HH:MM AM")                  /* set time             */
  help_run = YES
   fdb    = FRAME-DB
   ffile  = FRAME-FILE
   ffield = FRAME-FIELD.


RUN samples/atmnu001.p.
g_menuhelp = yes.













































