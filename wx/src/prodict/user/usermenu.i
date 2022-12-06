/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------
File: usermenu.i

Description:
   This include file contains the definition of the Admin menu and the
   Dictionary menu for the TTY version.
 
Arguments:
   {1} - this is either "NEW" or nothing.

Author: Laura Stern

HISTORY
mcmann   06/05/98   DICTDB was being assigned to a foreign database which
                    caused an error message to be display when trying to
                    access DataServer utilities. 98-04-28-005 96-10-17-026
mcmann   06/03/98   Removed MS SQL Server 98-05-21-018 and clean up code.
mcmann   05/12/98   Change Oracle SQL to sqlplus from plus32 as the default.
laurief  04/09/98   Changed user_env[1] from "a" to "s" for the following
                    utilities to allow users to select/deselect multiple
                    tables (BUG 97-07-22-029).  Utilities include Dump 
                    Definitions
mcmann   01/16/98   Change Oracle migration tools menu
hutegger 96/09      removed DB2 and DB2-DRDA again
tomn     07/96      Changed "menu graying" logic (Menu_Walk) so that a menu item
                    will be disabled if all of its children are disabled.  This
                    is an MS-Windows standard, which has a side-effect on TTY
                    since there is no visual cue that a menu item is disabled.
tomn     07/96      Added "Schema Migration Tools" pop-up menu to MS-SQL, ORACLE,
                    and SYBASE sub-menus.  This menu is essentially 3 of the 4
                    individual steps of the "PROTOXXX" utility: Generate DDL,
                    Send SQL, and Adjust Schema (Beautify).  The fourth
                    step is Create Schema Image, which is already on the previous
                    menu.  Also on this menu is the combined "PROTOXXX" utility.
tomn     07/96      Reordered DataServer main menu in alphabetic order.
DLM      06/96      Removed DB2/400 V6 Utilities and Allbase reference
sjp      06/96      Added db type DB2-DRDA. Re-used DB2 for DB2 with ODBC.
                    DB2-DRDA is older, native DB2 dataserver.
DLM      05/96      Changed system check to be begins ms-win instead of =
tomn     03/96      Removed dataserver name from submenu label to shorten length
tomn     03/96      Removed Sybase4 from menu; Changed Sybase10 menu labels to
                    "Sybase" (Sybase4 was "Sybase" and Sybase10 was "Sybase-10")
hutegger 95/11      added MSSQL-menu plus /*###*/ to indicate where
                    changes are needed when adding a new DataServer
                    (Don't forget adecomm/ds_type.i & prodict/dictgate.i!)
hutegger 95/09      removed netisam and ctos-isam, also V5-dump 
trb      05/05/95   Updated AS400 V7 Utilities.
nhorn    11/14/94   Added AS400 V7 Utilities. Took out Allbase Utilities.
gfs      11/01/94   Fixed wording on menu.
gfs      07/22/94   Changed mneumonic on "Change" from "g" to "h" for
                    better readability.

Date Created: 01/04/93 
----------------------------------------------------------------------------*/

/*===========================Variables==================================*/

/*---------------------------------------------------------------------
   This will be filled with the menu handles for each menu-item.  After
   the initial graying phase, elements for menu-items that will NEVER
   be activated for this version of progress will be set to ?.
---------------------------------------------------------------------*/
Define var Menu_Hdl as widget-handle extent /*170*/ 200 NO-UNDO.  /* tsn 7/96 */

/* Indexes into the Menu_Hdl array and the ?_Gray arrays. */
Define var All_Items_ix as integer NO-UNDO init 0. /* idx into Menu_Hdl */
Define var Submenu_ix 	as integer NO-UNDO init 0. /* idx into ?_Gray array */

/* This is used after running another ADE tool */
Define var num_connected as integer NO-UNDO.


/*------------------------------------------------------------------------
   This array is used to determine what should be grayed when we are
   running a particular version of Progress.  There is an array element 
   for each sub-menu.  The value of the array element is a comma 
   delimited list, with a value for each menu item.  (A rule is considered
   a menu item!)  Each of these values is a combination of the letters FCXQRN.
   Each letter represents a configuration of Progress as shown below.  If the 
   letter is present, it means that this menu item should be enabled for 
   this Progress.  Otherwise, it is replaced with a "-".  So FCX--- means 
   that we enable this menu item only for Full, Compile and Compile-Encrypted 
   Progress.

   The code letters are as follows:
      F = FULL PROGRESS
      C = COMPILE-ONLY PROGRESS

      X = COMPILE-ENCRYPT PROGRESS
      Q = QUERY/REPORT PROGRESS
      R = RUN-TIME PROGRESS
      N = PROGRESS w/no database connected 

   *****************************IMPORTANT***************************
   The order of submenus was generated by the same tree walk 
   that will be used to process the list for graying.
   If any menus/menu items are added, removed or re-ordered, this 
   table must be updated.
   ******************************************************************
---------------------------------------------------------------------------*/
Define var Gray_Table as char extent 27 NO-UNDO init /*###*/
[
   /*Database  */ "FCXQR-,FCXQRN,FCXQRN,FCXQRN,FCXQRN",       
   /*Reports   */ "FCXQR-,FCXQR-,FCXQR-,FCXQR-,FCXQR-,FCXQR-,FCXQR-,FCXQ--,FCXQR-",
  &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   /*&Schema   */ "FCXQR-,FCX---,FCX---,FCXQR-,FCX---,FCX---,FCXQR-,FCXQR-", 	
  &ENDIF
   /*Admin     */ "FCX---",
   /*Dump      */ "FCX---,F-----,FCX---,FCX---,FCX---,FCX---,FCX---,F-----,------,FCX---/*,FCX---,FCX---*/",
   /*Load      */ "FCX---,F-----,FCX---,FCX---,F-----,FCX---",  
   /*Security  */ "FCXQ--,FCXQ--,FCXQ--,FCXQ--,FCXQ--,FCXQ--",    	
   /*Export    */ "F-----,F-----,F-----,F-----,F-----,F-----",  	
   /*Import    */ "F-----,F-----,F-----,F-----,F-----,F-----",    	
   /*DataServer*/ "", 
   /*CISAM Util*/ "FCX---,FCX---,/*FCX---,*/FCX---,FCX---,FCX---,FCX---,FCX---",
   /*DB2/400 Util */
                  "FCX---,FCX---,------,FCX---,FCX---,FCX---", 
   /*Odb Util  */ "FCX---,FCX---,FCX---,FCX---,FCX---,FCX---",
   /*ORA Util  */ "FCX---,FCX---,FCX---,FCX---,FCX---,FCX---,FCX---,------,FCX---",
   /*ORA Tools */ "FCXQ--",
   /*RMS Util  */ "FCX---,FCX---,FCX---,FCX---,FCX---,FCX---,FCX---,FCX---,FCX---",
   /*Syb Util  */ "FCX---,FCX---,FCX---,FCX---,FCX---,FCX---,------,FCX---",
   /*Syb Tools */ "FCXQ--,FCX---,FCX---,------,FCX---",
   /*VMS Rdb Util */
                  "FCX---,FCX---,FCX---,FCX---,FCX---,FCX---",
   /*Utilities */ "FCXQ-N,------,FCX---,FCX---,FCX---,------,FCXQ-N",
   /*Quoter    */ "FCXQ-N,FCXQ-N,FCXQ-N,FCXQ-N", 
   /*Incl File */ "FCXQ--,FCXQ--,FCXQ--", 
   /*PRO/SQL   */ "FCXQR-,FCXQ--,FCXQ--"
  &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
   ,
   /*&Help     */ "FCXQRN,FCXQRN,FCXQRN,FCXQRN,FCXQRN,FCXQRN"  
  &ENDIF
   ].


/* These are array indices for the DataServer sub-menus in Gray_Table.
*/
&IF "{&WINDOW-SYSTEM}" = "TTY" /*###*/
 &THEN
  &global-define CISAM_IX       11
  &global-define AS400V7_IX     12
  &global-define ODBC_IX        13
  &global-define ORACLE_IX      14 /* tools 15 */
  &global-define RMS_IX         16
  &global-define SYB10_IX       17 /* tools 18 */
  &global-define RDB_IX         19
 &ELSE                         /*###*/                       
  &global-define CISAM_IX       10
  &global-define AS400V7_IX     11
  &global-define ODBC_IX        12
  &global-define ORACLE_IX      13 /* tools 14 */
  &global-define RMS_IX         15
  &global-define SYB10_IX       16 /* tools 17 */
  &global-define RDB_IX         18
&ENDIF

/*=============================Menu Definitions==========================*/

/* standard tools menu */
&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   {adecomm/toolmenu.i 
      &EXCLUDE_ADMIN = yes
      &EXCLUDE_DICT  = yes
   }
&ELSE 
   {adecomm/toolmenu.i 
      &EXCLUDE_ADMIN = yes
   }
&ENDIF


/*--------------------Database and its Sub-menus-------------------------*/
Define sub-menu mnu_Reports	
   menu-item mi_Rpt_DetTbl    label "&Detailed Table"
   menu-item mi_Rpt_Table     label "Quick &Table"
   menu-item mi_Rpt_Field     label "Quick &Field"
   menu-item mi_Rpt_Index     label "Quick &Index"
   menu-item mi_Rpt_View      label "PRO/SQL &View"
   menu-item mi_Rpt_Sequence  label "&Sequence"
   menu-item mi_Rpt_Trigger   label "Tri&gger"
   menu-item mi_Rpt_User      label "&User"
   menu-item mi_Rpt_TblRel    label "Table &Relations"
   .

Define sub-menu mnu_Database
   menu-item mi_DB_Select     label "&Select Working Database..."
   menu-item mi_DB_Create     label "&Create..."
   menu-item mi_DB_Connect    label "Co&nnect..." 
   menu-item mi_DB_Disconnect label "&Disconnect..."
   sub-menu  mnu_Reports      label "&Reports"   
   menu-item mi_Exit          label "E&xit"
   .

/*---------------------------Schema Menu----------------------------------*/
&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
Define sub-menu mnu_Schema
   menu-item mi_Sch_ModTbl    label "&Modify Table..."
   menu-item mi_Sch_AddTbl    label "&Add New Table..."
   menu-item mi_Sch_DelTbl    label "&Delete Table(s)..."
   menu-item mi_Sch_FldEdit   label "&Field Editor..."
   menu-item mi_Sch_ReordFld  label "&Reorder Fields..."
   menu-item mi_Sch_RenamFld  label "&Global Field Name Change..."
   menu-item mi_Sch_IdxEdit   label "&Index Editor..."
   menu-item mi_Sch_SeqEdit   label "&Sequence Editor..."
   .
&ENDIF
    
/*-----------------Admin Menu and its Sub-menus---------------------------*/

Define sub-menu mnu_Dump
   menu-item mi_Dump_Defs     label "&Data Definitions (.df file)..."
   menu-item mi_Dump_Contents label "&Table Contents (.d file)..."
   menu-item mi_Dump_Views    label "SQL &Views..."
   menu-item mi_Dump_User     label "&User Table Contents..."
   menu-item mi_Dump_AutoConn label "&Auto-Connect Records only..."
   menu-item mi_Dump_CollTran label "&Collation Tables..."
   menu-item mi_Dump_SeqDefs  label "&Sequences Definitions..."
   menu-item mi_Dump_SeqVals  label "Se&quences Current Values..."
   RULE
   menu-item mi_Dump_IncrDF   label "Create &Incremental .df File..."
 /*menu-item mi_Dump_ConvV5   label "Convert V&5 into Current .df Format..."*/
 /*menu-item mi_Dump_DumpV5   label "Dump Data Definitions in &Old V5 Format..."*/
   .

Define sub-menu mnu_Load
   menu-item mi_Load_Defs     label "&Data Definitions (.df file)..."
   menu-item mi_Load_Contents label "&Table Contents (.d file)..."
   menu-item mi_Load_Views    label "SQL &Views..."
   menu-item mi_Load_User     label "&User Table Contents..."
   menu-item mi_Load_SeqVals  label "&Sequences Current Values..."
   menu-item mi_Load_BadRecs  label "&Reconstruct Bad Load Records..."
   .

Define sub-menu mnu_Security
   menu-item mi_Sec_EditUser  label "&Edit User List..."
   menu-item mi_Sec_Password  label "Change Your &Password..."
   menu-item mi_Sec_DataSec   label "Edit Data &Security..."
   menu-item mi_Sec_Adminors  label "Security &Administrators..."
   menu-item mi_Sec_BlankId   label "Disallow &Blank Userid Access..."
   menu-item mi_Sec_UserRpt   label "&User Report..."
   .
	
Define sub-menu mnu_Export
   menu-item mi_Exp_DIF	      label "&DIF..."
   menu-item mi_Exp_SYLK      label "&SYLK..."
   menu-item mi_Exp_ASCII     label "&Text..."
   menu-item mi_Exp_WordStar  label "&WordStar Merge Data..."
   menu-item mi_Exp_MSWord    label "&Microsoft Word Merge Data..."
   menu-item mi_Exp_Perfect   label "Word&Perfect Merge Data..."
   /* menu-item mi_Exp_OfisWrit  label "BTOS &OfisWriter Merge Data..." */
   .

Define sub-menu mnu_Import
   menu-item mi_Imp_DIF	      label "&DIF..."
   menu-item mi_Imp_SYLK      label "&SYLK..."
   menu-item mi_Imp_ASCII     label "Delimited &Text..."
   menu-item mi_Imp_FixedLen  label "&Fixed-Length..."
   menu-item mi_Imp_dBASEDefs label "d&BASE Definitions..."
   menu-item mi_Imp_dBASECont label "dBASE File &Contents..."
   .

Define sub-menu mnu_Admin
   sub-menu  mnu_Dump 	      label "&Dump Data and Definitions"
   sub-menu  mnu_Load 	      label "&Load Data and Definitions"
   sub-menu  mnu_Security     label "&Security"
   sub-menu  mnu_Export	      label "&Export Data"
   sub-menu  mnu_Import	      label "&Import Data"
   menu-item mi_BulkLoad      label "Create &Bulk Loader Description File..."
   .


/*-----------------DataServers Menu and its Sub-menus-----------------------*/
Define sub-menu mnu_ora_tools
  /* menu-item mi_ora_GenDDL    label "&Generate DDL..."
   menu-item mi_ora_SendDDL   label "&Send DDL..."
   menu-item mi_ora_AdjstSI   label "&Adjust Schema..."
   RULE */
   menu-item mi_ora_Migrate   label "&Migrate DB to ORACLE..."
   .

Define sub-menu mnu_syb_tools
   menu-item mi_syb_GenDDL    label "&Generate DDL..."
   menu-item mi_syb_SendDDL   label "&Send DDL..."
   menu-item mi_syb_AdjstSI   label "&Adjust Schema..."
   RULE
   menu-item mi_syb_Migrate   label "&Migrate DB to SYBASE..."
   .
   
/*== Sub-menus ==*/   
Define sub-menu mnu_CISAM
   menu-item mi_CISAM_Create   label "&Create DataServer Schema...          "
   menu-item mi_CISAM_AddFile  label "&Add New Table Definition...          "
   menu-item mi_CISAM_uddt     label "&User-defined Data Types...           "
   menu-item mi_CISAM_DispRec  label "Display Record &Layout...             "
   menu-item mi_CISAM_ConnInfo label "&Edit Connection Information...       "
   menu-item mi_CISAM_ChgCP    label "C&hange DataServer Schema Code Page..."
   menu-item mi_CISAM_Delete   label "&Delete DataServer Schema...          "
   .

Define sub-menu mnu_AS400V7
   menu-item mi_AS4V7_Dict     label "PROGRESS/400 Data Dictionary..."
   menu-item mi_AS4V7_Sync     label "Synchronize PROGRESS/400 Client..."  
   RULE                                                                                                                               
   menu-item mi_AS4V7_ConnInfo label "&Edit Connection Information..."
   menu-item mi_AS4v7_Create   label "&Create DataServer Schema..."
   menu-item mi_AS4V7_Delete   label "&Delete DataServer Schema..." 
   .

Define sub-menu mnu_Odbc
   menu-item mi_Odb_Create    label "&Create DataServer Schema..."
   menu-item mi_Odb_UpdFile   label "&Update/Add Table Definitions..."
   menu-item mi_Odb_VerFile   label "&Verify Table Definition..."
   menu-item mi_Odb_ConnInfo  label "&Edit Connection Information..."
   menu-item mi_odb_ChgCP     label "C&hange DataServer Schema Code Page..."
   menu-item mi_Odb_Delete    label "&Delete DataServer Schema..."
   .

Define sub-menu mnu_ORACLE
   menu-item mi_ORA_Create    label "&Create DataServer Schema..."
   menu-item mi_ORA_UpdFile   label "&Update/Add Table Definitions..."
   menu-item mi_ORA_VerFile   label "&Verify Table Definition..."
   menu-item mi_ORA_ConnInfo  label "&Edit Connection Information..."
   menu-item mi_ORA_ChgCP     label "C&hange DataServer Schema Code Page..."
   menu-item mi_ORA_Delete    label "&Delete DataServer Schema..."
   menu-item mi_ORA_SQLPlus   label "&Run ORACLE SQL*Plus..."
   RULE
   sub-menu mnu_ora_tools     label "Schema &Migration Tools"
   .

Define sub-menu mnu_RMS
   menu-item mi_RMS_Create    label "&Create DataServer Schema..."
   menu-item mi_RMS_AddFile   label "&Add New Table Definition..."
   menu-item mi_RMS_UpdFile   label "&Update Existing Table Definition..."
   menu-item mi_RMS_Import    label "&Import Definitions from CDD/Plus..."
   menu-item mi_RMS_RedIdx    label "&Redefine Index..."
   menu-item mi_RMS_DispRec   label "Display Record &Layout..."
   menu-item mi_RMS_ConnInfo  label "&Edit Connection Information..."
   menu-item mi_RMS_ChgCP     label "C&hange DataServer Schema Code Page..."
   menu-item mi_RMS_Delete    label "&Delete DataServer Schema..."
   .

Define sub-menu mnu_Syb10
   menu-item mi_S10_Create    label "&Create DataServer Schema..."
   menu-item mi_S10_UpdFile   label "&Update/Add Table Definitions..."
   menu-item mi_S10_VerFile   label "&Verify Table Definition..."
   menu-item mi_S10_ConnInfo  label "&Edit Connection Information..."
   menu-item mi_S10_ChgCP     label "C&hange DataServer Schema Code Page..."
   menu-item mi_S10_Delete    label "&Delete DataServer Schema..."
   RULE
   sub-menu mnu_syb_tools     label "Schema &Migration Tools"
   .

Define sub-menu mnu_Rdb
   menu-item mi_Rdb_Create    label "&Create DataServer Schema..."
   menu-item mi_Rdb_UpdFile   label "&Update/Add Table Definitions..."
   menu-item mi_Rdb_VerFile   label "&Verify Table Definition..."
   menu-item mi_Rdb_ConnInfo  label "&Edit Connection Information..."
   menu-item mi_Rdb_ChgCP     label "C&hange DataServer Schema Code Page..."
   menu-item mi_Rdb_Delete    label "&Delete DataServer Schema..."
   .

/*== DataServers Main Menu ==*/     /*###*/ /*###*/
Define sub-menu mnu_Gateway
   sub-menu mnu_CISAM         label "&C-ISAM Utilities"
   sub-menu mnu_AS400V7       label "D&B2/400 Utilities" 
   sub-menu mnu_Odbc          label "O&DBC Utilities"
   sub-menu mnu_ORACLE        label "&ORACLE Utilities"
   sub-menu mnu_RMS           label "&RMS Utilities"
   sub-menu mnu_Syb10         label "&SYBASE Utilities"    
   sub-menu mnu_Rdb           label "&VMS Rdb Utilities"
   .


/*---------------Utilities Menus and its sub-menus--------------------------*/

Define sub-menu mnu_Quoter
   menu-item mi_Quo_EntLines  label "&Entire Lines..."
   menu-item mi_Quo_Delimiter label "By &Delimiter..."
   menu-item mi_Quo_ColRanges label "By &Column Ranges..."
   menu-item mi_Quo_InclFile  label "Quoter &Include File..."
   .

Define sub-menu mnu_GenIncl
   menu-item mi_Incl_Assign   label "&ASSIGN Statement..."
   menu-item mi_Incl_FORM     label "&FORM Statement..."
   menu-item mi_Incl_WorkFile label "&DEFINE WORK-TABLE Statement..."
   .

Define sub-menu mnu_Utilities
   menu-item mi_Util_ParmFile label "Editor for &Parameter Files..."
   sub-menu  mnu_Quoter	      label "&Quoter Functions"
   sub-menu  mnu_GenIncl      label "&Generate Include Files"
   RULE
   menu-item mi_Util_AutoConn label "Edit PROGRESS &Auto-Connect List..."
   menu-item mi_Util_Freeze   label "&Freeze/Unfreeze..."
   menu-item mi_Util_IdxDeact label "Index &Deactivation..."
   RULE
   menu-item mi_Util_Info     label "&Information..."
   .


/*---------------------------Other menus----------------------------------*/

Define sub-menu mnu_SQL
   menu-item mi_SQL_ViewRpt   label "PRO/SQL View &Report..."
   menu-item mi_SQL_DumpView  label "Dump as CREATE &VIEW Statement..."
   menu-item mi_SQL_DumpTable label "Dump as CREATE &TABLE Statement..."
   .

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
Define sub-menu mnu_Help SUB-MENU-HELP
   menu-item mi_Hlp_Topics       label "&Help Topics"
   RULE 
   menu-item mi_Hlp_Messages     label "M&essages..."
   menu-item mi_Hlp_Recent       label "&Recent Messages..."
   RULE
   menu-item mi_Hlp_About        label "&About Database Administration".
&ENDIF


/*----------------------------Menu bar-----------------------------------*/
Define {1} shared menu mnu_Admin_Tool
   MENUBAR
   sub-menu mnu_Database    label "&Database"

   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   sub-menu mnu_Schema	    label "&Schema"
   &ENDIF

   sub-menu mnu_Admin	    label "&Admin"
   sub-menu mnu_Gateway     label "DataSer&ver"
   sub-menu mnu_Utilities   label "&Utilities"
   sub-menu mnu_SQL	    label "&PRO/SQL"
   sub-menu mnu_Tools       label "&Tools"

   &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
   sub-menu mnu_Help	    label "&Help"
   &ENDIF
   .


/*==========================Internal Procedures==========================*/

/*----------------------------------------------------------------------
   Do_Menu_Item_Initial

   This is the function called for each menu item on the initial walk
   through the menus.

   Input Parameter:
      p_hdl    - Handle to a menu item
      p_sub_ix  - Index of the sub-menu being processed (in Gray_Table)
      p_item_ix - Index of the menu item within this sub-menu.
----------------------------------------------------------------------*/
Procedure Do_Menu_Item_Initial:
   Define INPUT parameter p_hdl      as widget-handle NO-UNDO.
   Define INPUT parameter p_sub_ix   as integer       NO-UNDO.
   Define INPUT parameter p_item_ix  as integer       NO-UNDO.

   Define var gray as integer NO-UNDO.  /*  0 = gray, 1 = don't gray */
   Define var code as char    NO-UNDO.  /* FCXQRN entry from gray table */

   code = ENTRY(p_item_ix, Gray_Table[p_sub_ix]).

   case PROGRESS:
      when "Full" then
      	 gray = INDEX(code, "F").
      when "COMPILE" then 
      	 gray = INDEX(code, "C").
      when "COMPILE-ENCRYPT" then 
      	 gray = INDEX(code, "X").
      when "Query" then 
      	 gray = INDEX(code, "Q").
      when "Run-Time" then 
      	 gray = INDEX(code, "R").
     end.

   if gray = 0 then
      /* If this menu item not appropriate for this PROGRESS
         then disable it and set handle to ? so it will never
         get enabled.
      */
      assign
         p_hdl:sensitive = no
         Menu_Hdl[All_Items_ix] = ?. 
   else do:
      Menu_Hdl[All_Items_ix] = p_hdl.  /* store the menu handle */
      /* If we're not connected to a database and this isn't 
         appropriate when no database is connected then disable it 
      */
      if user_dbname = "" AND (INDEX(code, "N") = 0) then
         p_hdl:sensitive = no.
     end.
   
  end.


/*----------------------------------------------------------------------
   Do_Menu_Item_Db_Change

   This is the function called for each menu item on the subsequent
   walks through the menus when the database connection status has
   changed.

   Input Parameter:
      p_hdl    - Handle to a menu item
      p_sub_ix  - Index of the sub-menu being processed (in Gray_Table)
      p_item_ix - Index of the menu item within this sub-menu.      
----------------------------------------------------------------------*/
Procedure Do_Menu_Item_Db_Change:
   Define INPUT parameter p_hdl      as widget-handle NO-UNDO.
   Define INPUT parameter p_sub_ix   as integer       NO-UNDO.
   Define INPUT parameter p_item_ix  as integer       NO-UNDO.

   Define var code as char    NO-UNDO.  /* FCXQRN entry from gray table */

   /* If handle is ?, it's been grayed already and forever 
      because of the PROGRESS version or lack of gateway support.
      So do nothing */
   if NOT Menu_Hdl[All_Items_ix] = ? then
   do:
      /* If the menu item is marked as not appropriate when no database
      	 is connected (a "-" in the "N" slot) gray it or ungray it
      	 depending on if database is connected.  If it IS marked as
      	 appropriate even when there's no db connected (a "N" in the "N"
      	 slot) leave it alone - it will never be grayed.
      */
      code = ENTRY(p_item_ix, Gray_Table[p_sub_ix]).
      if INDEX(code, "N") = 0 then
         p_hdl:sensitive = (if user_dbname = "" then no else yes).
     end.
  end.


/*--------------------------------------------------------------------------
   Menu_Walk

   Walk through the menu tree and do menu graying.  This will be
   called once for initial menu graying and again for each time
   we switch between having a connected database and not having one.
   p_Func will do different things for each case.

   Input Parameter:
      p_hdl      - Handle to either the top level menu or a submenu.
      p_Func     - Function to call to process each menu item. 
      p_sm-alive - Flag to indicate if sub-menu should be disabled.  If any
                   of it's children are enabled, then it is "alive". 
----------------------------------------------------------------------------*/
Procedure Menu_Walk:
   Define INPUT  parameter p_hdl      as widget-handle NO-UNDO.  
   Define INPUT  parameter p_Func     as char          NO-UNDO.
   Define OUTPUT parameter p_sm-alive as logical       NO-UNDO init no.

   /* Submenu index that this recursive level is processing */
   Define var loc_sub_ix as integer NO-UNDO.  
   Define var item_ix    as integer NO-UNDO init 0.  /* index within submenu */
   Define var l_sm-alive as logical NO-UNDO.  /* store return value from   *
                                               * recursive calls to myself */

   loc_sub_ix = Submenu_ix.
   do while p_hdl <> ?:
      if p_hdl:type = "menu-item" then 
      do:
         assign
      	   All_Items_ix = All_Items_ix + 1
      	   item_ix      = item_ix + 1.
      	   
      	 run VALUE(p_Func) (p_hdl, loc_sub_ix, item_ix).
      	 if p_hdl:sensitive then assign p_sm-alive = yes.  /* sub-menu alive; at *
      	                                                    * least one enabled  *
      	                                                    * menu-item          */
        end.
      else do:
      	 /* Don't handle the tools menu - common code deals with this */
      	 if NOT(p_hdl:type = "sub-menu" AND p_hdl:label = "&Tools") then
      	 do:
      	    /* type will either be sub-menu or menu */
	    if p_hdl:type = "sub-menu" then
	       Submenu_ix = Submenu_ix + 1.
	    run Menu_Walk (p_hdl:first-child, p_Func, OUTPUT l_sm-alive).
	    if not l_sm-alive then
	      p_hdl:sensitive = no.  /* disable sub-menu - no enabled children */
	    else assign
	      p_sm-alive = yes        /* at least one enabled sub-menu */
	      p_hdl:sensitive = yes.  /* so enable parent              */
      	   end.
        end.
      if p_hdl:type <> "menu" then  /* if not top-level menu bar */
      	 p_hdl = p_hdl:next-sibling.
      else
      	 p_hdl = ?.
     end.
  end.


/*----------------------------------------------------------------------
   Initial_Menu_Gray

   Gray menu items based on what the current version of PROGRESS 
   supports.  This includes the type of Progress as shown below as well
   as the gateways supported by this Progress.

   This graying only happens once since the executable doesn't change
   within one session.  Also, at this time check to see if there is a
   database connected.  If not, gray based on the "N" array as well.  
   We want to do it in one pass since menu graying can be very slow.
----------------------------------------------------------------------*/
Procedure Initial_Menu_Gray:
   Define var gate       as char NO-UNDO.
   Define var supp_gates as char NO-UNDO.  /* supported gateways */
   Define var all_gates  as char NO-UNDO.

   Define var g_all_ix  as integer NO-UNDO. /* index into all gateways list */
   Define var gate_ix   as integer NO-UNDO. /* index into Gray_Table array */
   Define var gate_list as char    NO-UNDO. /* list of "------"s for gateway */
   Define var junk      as logical no-undo. /* for output parm we don't care about */

   /* Change the Gray_Table based on whether this copy of PROGRESS
      supports certain gateways.  If a gateway is not supported, reset
      the comma delimited list for that gateway menu to all "------"'s so
      that all the menu items will be grayed out.
   */
   ASSIGN
     all_gates  = {adecomm/ds_type.i 
       &direction = "itype"
       &from-type = supp_gates
       }
     supp_gates = GATEWAYS.

   do g_all_ix = 1 to NUM-ENTRIES(all_gates):
      gate = ENTRY(g_all_ix, all_gates).
      if NOT CAN-DO(supp_gates, gate) then
      	 case gate:
            when "CISAM" then
      	       Gray_Table[{&CISAM_IX}] = 
      	       	  "------,------,------,/*------,*/------,------,------,------".
      	    when "AS400" then 
               Gray_Table[{&AS400V7_IX}] = 
                  "------,------,------,------,------,------". 
            when "ODBC" then
               Gray_Table[{&ODBC_IX}] =
               "------,------,------,------,------,------".
            when "ORACLE" then do:
      	       Gray_Table[{&ORACLE_IX}] =      /* ORACLE Menu */
      	       	  "------,------,------,------,------,------,------,------,------".
      	       Gray_Table[{&ORACLE_IX} + 1] =  /* Migrate Menu */
      	       	  "------".
      	    end.
      	    when "RMS" then
      	       Gray_Table[{&RMS_IX}] = 
      	       "------,------,------,------,------,------,------,------,------".
            when "SYB10" then do:
               Gray_Table[{&SYB10_IX}] =      /* SYBASE Menu */
               "------,------,------,------,------,------,------,------".
               Gray_Table[{&SYB10_IX} + 1] =  /* Migrate Menu */
               "------,------,------,------,------".
            end.
      	    when "RDB" then
      	       Gray_Table[{&RDB_IX}] = 
      	       	  "------,------,------,------,------,------".
      	   end case.	  /*###*/
     end.
   
   /* Walk through the menu and do menu graying. */
   assign
      All_Items_ix = 0
      submenu_ix = 0.
   run Menu_Walk (MENU mnu_Admin_Tool:HANDLE, "Do_Menu_Item_Initial", OUTPUT junk).
  end.


/*------------------------------------------------------------------------ 
   Fix up the aliases.
------------------------------------------------------------------------*/
Procedure Reset_Aliases:
   IF LDBNAME("DICTDBG") <> ? THEN DELETE ALIAS "DICTDBG".
   IF user_dbname = "" THEN
      DELETE ALIAS "DICTDB".
   ELSE IF SDBNAME(user_dbname) = ? THEN DO:
      /* this case should never be executed except when running a copy
         of progress with a missing gateway (whatever that means!). */
      DO i = 1 TO cache_db#:
      	 IF cache_db_t[i] = user_dbtype AND cache_db_l[i] = user_dbname THEN 
      	    LEAVE.
        end.
      CREATE ALIAS "DICTDB" FOR DATABASE VALUE(cache_db_s[i]) NO-ERROR.
     end.
   ELSE DO:
      IF    DBTYPE("DICTDB") = "PROGRESS"
       AND  user_dbtype     <> "PROGRESS" 
       THEN RUN prodict/_dctalia.p
              ( INPUT user_dbname
              ). /* switch DICTDB only if needed */
      IF  DBTYPE("DICTDB") <> "PROGRESS"
       THEN CREATE ALIAS DICTDB FOR DATABASE VALUE(SDBNAME(user_dbname)).
      IF LDBNAME(user_dbname) <> SDBNAME(user_dbname) 
       AND CONNECTED(user_dbname)
       THEN CREATE ALIAS "DICTDBG"
            FOR DATABASE VALUE(LDBNAME(user_dbname)) 
            NO-ERROR.
     end.
  end.


/*------------------------------------------------------------------------
   Reset the current database - the last one may have been disconnected
   or what have you.
   
   Input Parameter:
      p_Prev - name of previously connected database or "" if there
      	       was no connected database.
------------------------------------------------------------------------*/
Procedure Reset_Db:

   Define INPUT PARAMETER p_Prev as char NO-UNDO.
   Define var junk as logical no-undo.  /* for output parm we don't care about */

   run Reset_Aliases.

   IF user_dbname = "" AND NUM-DBS > 0 THEN DO:
      RUN "prodict/_dctsget.p".
      RUN "prodict/_dctgues.p".
     end.
   /* If we've gone from having a database connected to not having one
      or vice-versa, we need to change menu gray state.
   */
   if (p_Prev = ""  AND user_dbname <> "") OR
      (p_Prev <> "" AND user_dbname = "") then
   do:
      assign
	 All_Items_ix = 0
	 submenu_ix = 0.
      run Menu_Walk (MENU mnu_Admin_Tool:HANDLE, "Do_Menu_Item_Db_Change", OUTPUT junk).
     end.
  end.


/*----------------------------------------------------------------------
   Perform_Func:

   Perform the function the user chose from the
   menu.  The input control string specifies what to do.
   Here is how the string is interpreted:

   If the statement looks like "number=value", where <number> is an
   integer from 1 to 19, then dictionary environment variable
   user_env[<number>] is set to <value>.  This is an assignment.

   If the statement begins with a star "*", it is an action statement.
   The different types of action strings are:
     *C - commit transaction
     *E - exit dictionary
     *N - nop (no operation, or do nothing)
     *O - OS-COMMAND (values in user_env[1-6] are the names of the
          programs to call (btos,msdos,os2,unix,vms,win32); user_env[7]
          is a default-value, used when any of user_env[1-6] = " "
     *Q - quit
     *R - rollback transaction.  this must be the last item in a
          control string, otherwise it is ignored (i.e., user_path
          must equal "*R" for this to execute properly).
     *T - start transaction, followed optionally by a colon ":" and
          the name of the procedure to execute when the user_path is
          empty.

   If the statement begins with a question mark "?", it is a DBTYPE
   string.  The name following the ? (e.g. "?RMS") is checked against
   the GATEWAYS function, and if the result is FALSE, a message is
   displayed and the control string is set to "".  In other words, 
   you can only perform this function if the current database is the
   given type.

   A "!" is similar to a "?", except that not only must the gateway be
   available in the current copy of PROGRESS, but the currently selected
   database must also be of that type.

   A "@" is similar to !, except that not only must the current database
   be of that type, but it must also be connected.

   If the statement is anything else, it is assumed to be a procedure.
   A "prodict/" is tacked on to the beginning, a ".p" is tacked on to
   the end, and it is RUN.
-----------------------------------------------------------------------*/
Procedure Perform_Func:
   Define INPUT parameter p_CtrlStr as char NO-UNDO.
   
   Define var prev_dbname as char    NO-UNDO.
   Define var ampersand   as integer NO-UNDO.
   Define var lbl         as char    NO-UNDO.

   hide message NO-PAUSE.

   /* Display menu pick text in header line, removing ampersand first */
   lbl = SELF:label.  /* based on trigger that called this function */
   ampersand = INDEX(lbl, "&").
   lbl = (if ampersand > 1 then SUBSTR(lbl,1,ampersand - 1,"character")
                           else "")
      	  + SUBSTR(lbl,ampersand + 1,-1,"character").
   lbl = TRIM (lbl, ".").  /* remove any elipsis */
   { prodict/user/userhdr.i lbl}

   assign
      user_path = p_CtrlStr   
      prev_dbname = user_dbname.

   /* I don't understand why this has to be done everytime, BEFORE we
      perform the function.  But that's what the char dictionary did so.. 
      (hutegger 95/07): _dctgues allows DICTDB to point at a _db with
      type <> PROGRESS. It should point at a PROGRESS _Db and
      Reset_Aliases in this case resets it to point at its schemaholder.
      DICTDB neds to point at PROGRESS type db because _dctexec has 
      _Db-reference in it.
   */
   run Reset_Aliases.

   /* Hide welcome frame */
   if frame guten-tag:visible then
      hide frame guten-tag.

   /* To parent dialogs properly. */
   &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
      CURRENT-WINDOW = win.
   &ENDIF

   /* this is the main execute loop */
mainl:
   DO WHILE user_path <> "" AND user_path <> "*E" AND user_path <> "*R"
             ON ERROR UNDO, LEAVE mainl
             ON STOP  UNDO, LEAVE mainl:
      IF user_dbname = ""
       THEN RUN "prodict/_dctexen.p".
       ELSE do:
        IF dict_trans
         THEN 
          _dict: DO TRANSACTION:
         RUN "prodict/_dctexec.p".
         IF user_path = "*R" THEN UNDO _dict,LEAVE _dict.
         end.
        ELSE RUN "prodict/_dctexec.p".
       end.
     end.

   IF user_path = "*E" 
     THEN APPLY "CHOOSE" TO MENU-ITEM mi_Exit IN MENU mnu_Database.

   /* Make sure userpath is set properly, in the event main loop exited
    * because of an error.
    * We also need to clear the window and reset the cursor.
    *                                                   (hutegger 95/09)
    */
   user_path =  "".
   hide all no-pause.
   RUN adecomm/_setcurs.p ("").
   { prodict/user/usercon.i }
  
   /* Make sure current database is set properly. */
   IF user_path <> "*E" 
     THEN run Reset_Db (INPUT prev_dbname).

   assign
      dict_trans  = FALSE
      dict_dirty  = FALSE
      user_trans  = ""
      user_env    = "".

   /* Reset context line */
   lbl = "Main Menu".
   { prodict/user/userhdr.i lbl}

  end.


/*------------------------------------------------------------------------ 
   Disable myself because user has run another tool.  This simply
   means disable the menu bar.
------------------------------------------------------------------------*/
Procedure disable_widgets:

   /* Unset global active ade tool variable. */
   ASSIGN h_ade_tool = ?.

   num_connected = NUM-DBS.   /* remember this for later */

   /* This only does top level menu items, so we don't have to worry 
      about messing up grayed/non-grayed items.
   */
   menu mnu_Admin_Tool:sensitive = no.

   if frame guten-tag:visible then
      hide frame guten-tag.
   hide frame user_hdr.
   hide frame user_ftr.
   hide message NO-PAUSE.
   SESSION:SUPPRESS-WARNINGS = sw_sav.  /* sw_sav is defined in _dictc.p */
  end.


/*------------------------------------------------------------------------ 
   Reactivate myself now that invoked tool has returned. 
------------------------------------------------------------------------*/
Procedure enable_widgets:
   
   Define var ix        as integer NO-UNDO.
   Define var all_conn  as logical NO-UNDO init yes.
   Define var curr_conn as logical NO-UNDO.
   Define var prev_db   as char    NO-UNDO.

   /* Set global active ade tool procedure handle to Procedure Editor. */
   ASSIGN h_ade_tool = THIS-PROCEDURE.

   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
      /* Reattach the menubar and redisplay hdr and ftr frames */
      default-window:menubar = menu mnu_Admin_Tool:handle.
      {prodict/user/userhdr.i user_hdr}
      { prodict/user/usercon.i user_filename} /* displays footer */

      /* Reset session attributes in case tool clobbered them. */
      ASSIGN
      	 session:APPL-ALERT-BOXES = NO
      	 session:SYSTEM-ALERT-BOXES = NO
      	 SESSION:SUPPRESS-WARNINGS = YES.
   &ELSE
      /* Reset session attributes in case tool clobbered them. */
      ASSIGN
	 session:APPL-ALERT-BOXES = NO
	 session:SYSTEM-ALERT-BOXES = YES
	 SESSION:SUPPRESS-WARNINGS = YES.
   &ENDIF

   menu mnu_Admin_Tool:sensitive = yes.

   /* If any of the databases that were connected before are no longer
      connected or there are new ones connected, then reset the
      database cache.
   */
   curr_conn = connected (user_dbname).
   if num_connected = NUM-DBS then  /* this check is for efficiency */
      do ix = 1 to cache_db# while all_conn:
	 all_conn = all_conn AND CONNECTED(cache_db_l[ix]).
        end.

   if num_connected <> NUM-DBS OR NOT all_conn then
   do:
      prev_db = user_dbname.
      if user_dbname <> "" AND NOT curr_conn then
      do:
      	 /* previous db no longer connected */
      	 user_dbname = "".
      	 user_dbtype = "".
      	 user_filename = "".
      	 { prodict/user/usercon.i user_filename} /* displays footer */
        end.
      if user_dbname = "" then
      	 run Reset_Db (INPUT prev_db). /* refresh cache and pick a new db */
      else 
      	 run "prodict/_dctsget.p".  /* refresh the cache */
     end.
    
   if curr_conn then 
   do:
     IF user_dbtype <> "PROGRESS" THEN
      /* Make sure alias is still set properly. */
        CREATE ALIAS "DICTDB" FOR DATABASE VALUE(SDBNAME(user_dbname)) NO-ERROR.
     ELSE
        CREATE ALIAS "DICTDB" FOR DATABASE VALUE(user_dbname) NO-ERROR.

      /* Since someone may have changed the schema in other tool */
      assign
      	 user_filename = ""
      	 cache_dirty = yes.
     end.
    
   {prodict/user/usercon.i user_filename} /* redisplay footer */
  end.


/*=============================Triggers==================================*/

/*---------------------------Database menu-------------------------------*/

/*----- SELECT DATABASE -----*/
on choose of menu-item mi_DB_Select 	in menu mnu_Database 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=get,_usrsget").
   &ELSE
   run Perform_Func ("1=get,_guisget").
   &ENDIF

/*----- CONNECT DATABASE -----*/
on choose of menu-item mi_DB_Connect    in menu mnu_Database
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=usr,_usrscon,1=new,_usrsget,*N").
   &ELSE
   run Perform_Func ("1=usr,_usrscon,1=new,_guisget,*N").
   &ENDIF

/*----- DISCONNECT DATABASE -----*/
on choose of menu-item mi_DB_Disconnect in menu mnu_Database
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=dis,_usrsget").
   &ELSE
   run Perform_Func ("1=dis,_guisget").
   &ENDIF

/*----- CREATE DATABASE -----*/
on choose of menu-item mi_DB_Create 	in menu mnu_Database
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrprdb,1=env,_usrscon,1=new,_usrsget,*N").
   &ELSE
   run Perform_Func ("_usrprdb,1=env,_usrscon,1=new,_guisget,*N").
   &ENDIF

/*----- EXIT -----*/
/* shouldn't be there, since it's the end-condition for the wait-for
 * (hutegger 95/10)
 *
 * on choose of menu-item mi_Exit      	in menu mnu_Database
 *    run Perform_Func ("*E").
 */

/*---------------------------Schema menu-------------------------------*/

&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
/*----- MODIFY TABLE -----*/
on choose of menu-item mi_Sch_ModTbl   in menu mnu_Schema
   run Perform_Func ("*T:_usrtrax,_usrtget,_usrtchg,19=alpha,_usrfchg").

/*----- CREATE TABLE -----*/
on choose of menu-item mi_Sch_AddTbl   in menu mnu_Schema
   run Perform_Func ("_usrtchg,19=alpha,_usrfchg").

/*----- DELETE TABLE -----*/
on choose of menu-item mi_Sch_DelTbl   in menu mnu_Schema  
   run Perform_Func ("_usrtdel").

/*----- FIELD EDITOR -----*/
on choose of menu-item mi_Sch_FldEdit  in menu mnu_Schema  
   run Perform_Func ("*T:_usrtrax,_usrtget,19=alpha,_usrfchg").

/*----- REORDER FIELDS -----*/
on choose of menu-item mi_Sch_ReordFld in menu mnu_Schema  
   run Perform_Func ("_usrtget,_usrfnum").

/*----- RENAME FIELDS -----*/
on choose of menu-item mi_Sch_RenamFld in menu mnu_Schema
   run Perform_Func ("_usrfglo").

/*----- INDEX EDITOR -----*/
on choose of menu-item mi_Sch_IdxEdit  in menu mnu_Schema
   run Perform_Func ("*T:_usrtrax,_usrtget,_usrichg").

/*----- SEQUENCE EDITOR -----*/
on choose of menu-item mi_Sch_SeqEdit  in menu mnu_Schema 
   run Perform_Func ("_usrkchg").
&ENDIF


/*----------------------------Admin/Dump menu---------------------------*/

/*----- DUMP DEFS -----*/
on choose of menu-item mi_Dump_Defs     in menu mnu_Dump 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=s,_usrtget,9=d,_usrdump,_dmpsddl").
   &ELSE
   run Perform_Func ("1=s,_guitget,9=d,_usrdump,_dmpsddl").
   &ENDIF

/*----- DUMP CONTENTS -----*/
on choose of menu-item mi_Dump_Contents in menu mnu_Dump 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=s,_usrtget,9=f,_usrdump,*N,_dmpdata").
   &ELSE
   run Perform_Func ("1=s,_guitget,9=f,_usrdump,*N,_dmpdata").
   &ENDIF

/*----- DUMP VIEWS -----*/
on choose of menu-item mi_Dump_Views    in menu mnu_Dump 
   run Perform_Func ("!PROGRESS,2=ALL,9=v,_usrdump,_dmpview").

/*----- DUMP USERS -----*/
on choose of menu-item mi_Dump_User     in menu mnu_Dump 
   run Perform_Func ("!PROGRESS,9=u,_usrdump,_dmpuser").

/*----- DUMP AUTO-CONNECT -----*/
on choose of menu-item mi_Dump_AutoConn in menu mnu_Dump 
   run Perform_Func ("!PROGRESS,9=a,_usrdump,_dmpsddl").

/*----- DUMP COLLATE/TRANSLATE Stuff -----*/
on choose of menu-item mi_Dump_CollTran in menu mnu_Dump 
   run Perform_Func ("!PROGRESS,9=c,_usrdump,_dmpsddl").

/*----- DUMP SEQUENCES -----*/
on choose of menu-item mi_Dump_SeqDefs  in menu mnu_Dump 
   run Perform_Func ("!PROGRESS,9=s,_usrdump,_dmpsddl").

/*----- DUMP SEQUENCE VALUES -----*/
on choose of menu-item mi_Dump_SeqVals  in menu mnu_Dump 
   run Perform_Func ("!PROGRESS,9=k,_usrdump,_dmpseqs").

/*----- INCREMENTAL DF FILE -----*/
on choose of menu-item mi_Dump_IncrDF   in menu mnu_Dump
   run Perform_Func ("_usrincr,*N,_dmpincr").

/*----- CONVERT V5 to CURRENT -----*/
/* on choose of menu-item mi_Dump_ConvV5   in menu mnu_Dump
 *   run Perform_Func ("_usr5cvt,_dmp5cvt").
 */
/*----- DUMP DEFS ala V5 -----*/
/*on choose of menu-item mi_Dump_DumpV5   in menu mnu_Dump
 *   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
 *   run Perform_Func ("1=s,_usrtget,9=5,_usrdump,_dmpv5df").
 *   &ELSE
 *   run Perform_Func ("1=s,_guitget,9=5,_usrdump,_dmpv5df").
 *   &ENDIF
 */

/*----------------------------Admin/Load menu---------------------------*/

/*----- LOAD DEFS -----*/
on choose of menu-item mi_Load_Defs     in menu mnu_Load 
   run Perform_Func ("*T,9=d,_usrload,_lodv5df,*C,_lodsddl,9=h,_usrload").

/*----- LOAD CONTENTS -----*/
on choose of menu-item mi_Load_Contents in menu mnu_Load 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=s,_usrtget,9=f,_usrload,*N,_loddata").
   &ELSE
   run Perform_Func ("1=s,_guitget,9=f,_usrload,*N,_loddata").
   &ENDIF

/*----- LOAD VIEWS -----*/
on choose of menu-item mi_Load_Views    in menu mnu_Load 
   run Perform_Func ("2=ALL,9=v,_usrload,_lodview").

/*----- LOAD USERS -----*/
on choose of menu-item mi_Load_User     in menu mnu_Load 
   run Perform_Func ("9=u,_usrload,_loduser").

/*----- LOAD SEQUENCE VALUES -----*/
on choose of menu-item mi_Load_SeqVals  in menu mnu_Load 
   run Perform_Func ("!PROGRESS,9=k,_usrload,_lodseqs").

/*----- RECONSTRUCT BAD LOAD RECORDS -----*/
on choose of menu-item mi_Load_BadRecs  in menu mnu_Load 
   run Perform_Func ("_usrlrec,_dctlrec").


/*-------------------------Admin/Security menu---------------------------*/

/*----- EDIT USER LIST -----*/
on choose of menu-item mi_Sec_EditUser  in menu mnu_Security 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!PROGRESS,_usruchg").
   &ELSE
   run Perform_Func ("!PROGRESS,_guiuchg").
   &ENDIF

/*----- CHANGE PASSWORD -----*/
on choose of menu-item mi_Sec_Password  in menu mnu_Security 
   run Perform_Func ("!PROGRESS,_usrupwd").

/*----- EDIT DATA SECURITY -----*/
on choose of menu-item mi_Sec_DataSec   in menu mnu_Security 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,9=rw,_usrsecu").
   &ELSE
   run Perform_Func ("9=rw,_guisecu").
   &ENDIF

/*----- SECURITY ADMINISTRATORS -----*/
on choose of menu-item mi_Sec_Adminors  in menu mnu_Security 
   run Perform_Func ("!PROGRESS,_usradmn").

/*----- DISALLOW BLANK USERID ACCESS -----*/
on choose of menu-item mi_Sec_BlankId   in menu mnu_Security 
   run Perform_Func ("!PROGRESS,_usrblnk").

/*----- USER REPORT -----*/
on choose of menu-item mi_Sec_UserRpt   in menu mnu_Security 
   run Perform_Func ("_rptuqik").


/*---------------------------Admin/Export menu---------------------------*/
	
/*----- EXPORT DIF -----*/
on choose of menu-item mi_Exp_DIF	   in menu mnu_Export 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,9=dif,_usrexpo,_dmpdiff").
   &ELSE
   run Perform_Func ("_guitget,9=dif,_usrexpo,_dmpdiff").
   &ENDIF

/*----- EXPORT SYLK -----*/
on choose of menu-item mi_Exp_SYLK      in menu mnu_Export 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,9=sylk,_usrexpo,_dmpsylk").
   &ELSE
   run Perform_Func ("_guitget,9=sylk,_usrexpo,_dmpsylk").
   &ENDIF

/*----- EXPORT ASCII -----*/
on choose of menu-item mi_Exp_ASCII     in menu mnu_Export 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,9=ascii,_usrexpo,_dmpasci").
   &ELSE
   run Perform_Func ("_guitget,9=ascii,_usrexpo,_dmpasci").
   &ENDIF

/*----- EXPORT WORDSTAR -----*/
on choose of menu-item mi_Exp_WordStar  in menu mnu_Export 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,9=ws,_usrexpo,_dmpasci").
   &ELSE
   run Perform_Func ("_guitget,9=ws,_usrexpo,_dmpasci").
   &ENDIF

/*----- EXPORT MS WORD -----*/
on choose of menu-item mi_Exp_MSWord    in menu mnu_Export 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,9=word,_usrexpo,_dmpasci").
   &ELSE
   run Perform_Func ("_guitget,9=word,_usrexpo,_dmpasci").
   &ENDIF

/*----- EXPORT WORDPERFECT -----*/
on choose of menu-item mi_Exp_Perfect   in menu mnu_Export 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,9=wperf,_usrexpo,_dmpasci").
   &ELSE
   run Perform_Func ("_guitget,9=wperf,_usrexpo,_dmpasci").
   &ENDIF

/*----- EXPORT BTOS/CTOS OfisWriter -----*/
/*--- Not supported anymore, at least for now -----
on choose of menu-item mi_Exp_OfisWrit  in menu mnu_Export 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,9=ofisw,_usrexpo,_dmpasci").
   &ELSE
   run Perform_Func ("_guitget,9=ofisw,_usrexpo,_dmpasci").
   &ENDIF
---------------------------------*/


/*---------------------------Admin/Import menu---------------------------*/

/*----- IMPORT DIF -----*/
on choose of menu-item mi_Imp_DIF	   in menu mnu_Import 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,9=dif,_usrimpo,_loddiff").
   &ELSE
   run Perform_Func ("_guitget,9=dif,_usrimpo,_loddiff").
   &ENDIF

/*----- IMPORT SYLK -----*/
on choose of menu-item mi_Imp_SYLK      in menu mnu_Import 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,9=sylk,_usrimpo,_lodsylk").
   &ELSE
   run Perform_Func ("_guitget,9=sylk,_usrimpo,_lodsylk").
   &ENDIF

/*----- IMPORT ASCII -----*/
on choose of menu-item mi_Imp_ASCII     in menu mnu_Import 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,9=ascii,_usrimpo,_lodasci").
   &ELSE
   run Perform_Func ("_guitget,9=ascii,_usrimpo,_lodasci").
   &ENDIF

/*----- IMPORT FIXED LENGTH -----*/
on choose of menu-item mi_Imp_FixedLen  in menu mnu_Import 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,9=fixed,_usrimpo,_lodasci").
   &ELSE
   run Perform_Func ("_guitget,9=fixed,_usrimpo,_lodasci").
   &ENDIF

/*----- IMPORT dBASE DEFS -----*/
on choose of menu-item mi_Imp_dBASEDefs in menu mnu_Import 
   run Perform_Func ("_usrsdbf,_lodsdbf,9=h,_usrload").

/*----- IMPORT dBASE FILE CONTENTS -----*/
on choose of menu-item mi_Imp_dBASECont in menu mnu_Import 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,9=dbf,_usrimpo,_loddbff").
   &ELSE
   run Perform_Func ("_guitget,9=dbf,_usrimpo,_loddbff").
   &ENDIF


/*---------------------------Admin/other menu items---------------------*/

/*----- CREATE BULK LOADER DESCRIPTION FILE -----*/
on choose of menu-item mi_BulkLoad      in menu mnu_Admin
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=s,_usrtget,9=b,_usrdump,_dmpbulk").
   &ELSE
   run Perform_Func ("1=s,_guitget,9=b,_usrdump,_dmpbulk").
   &ENDIF

/*----------------------------DataServer/CISAM-----------------------------*/

/*----- Create Schema -----*/
on choose of menu-item mi_CISAM_Create   in menu mnu_CISAM 
   run Perform_Func
      ("?CISAM,1=add,3=CISAM,_usrschg,*C,7=c,_gat_set,7=c,_gat_nxt").

/*----- Add File Def -----*/
on choose of menu-item mi_CISAM_AddFile  in menu mnu_CISAM 
   run Perform_Func ("!CISAM,7=c,_gat_nxt").

/*----- User Defined Data Types -----*/
on choose of menu-item mi_CISAM_uddt   in menu mnu_CISAM 
   run Perform_Func ("!CISAM,_ism_uid").

/*----- Redefine Index -----* /
 *on choose of menu-item mi_CISAM_RedIdx   in menu mnu_CISAM 
 *   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
 *   run Perform_Func ("!CISAM,_usrtget,_gat_rix").
 *   &ELSE
 *   run Perform_Func ("!CISAM,_guitget,_gat_rix").
 *   &ENDIF
 */

/*----- Display Record Layout -----*/
on choose of menu-item mi_CISAM_DispRec  in menu mnu_CISAM 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!CISAM,_usrtget,_gat_off").
   &ELSE
   run Perform_Func ("!CISAM,_guitget,_gat_off").
   &ENDIF

/*----- Edit Connect Info -----*/
on choose of menu-item mi_CISAM_ConnInfo in menu mnu_CISAM
   run Perform_Func ("!CISAM,1=chg,3=CISAM,_usrschg").

/*----- Change Code Page -----*/
on choose of menu-item mi_CISAM_ChgCP    in menu mnu_CISAM
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!CISAM,_gat_cp,_gat_cp1").
   &ELSE
   run Perform_Func ("!CISAM,_gat_cp,_gat_cp1").
   &ENDIF

/*----- Delete Schema -----*/
on choose of menu-item mi_CISAM_Delete   in menu mnu_CISAM 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!CISAM,_usrsdel,*N,1=sys,_usrsget").
   &ELSE
   run Perform_Func ("!CISAM,_usrsdel,*N,1=sys,_guisget").
   &ENDIF


/*--------------------------Data Server/DB2-400---------------------------*/

/*----- PROGRESS/400 Data Dictionary -----*/
on choose of menu-item mi_AS4V7_Dict in menu mnu_AS400V7        
   &IF "{&WINDOW-SYSTEM}" BEGINS  "MS-Win" &THEN 
     RUN prodict/as4/_as4dict.p.          
   &ELSE
     RUN Perform_Func("!AS400,_as4clnt").                      
   &ENDIF                        
   
/*----- DB2/400 Synchronize Definitions -----*/   
on choose of menu-item mi_AS4V7_Sync in menu mnu_AS400V7
     RUN Perform_Func ("@AS400,_as4sync").                          
     
/*----- Edit Connect Info -----*/
 on choose of menu-item mi_AS4V7_ConnInfo  in menu mnu_AS400V7   
   run Perform_Func ("!AS400,1=chg,3=AS400,_as4schg"). 
   
/*----- Create DB2/400 DataServer Schema -----*/
on choose of menu-item mi_AS4V7_Create     in menu mnu_AS400V7   
    run Perform_Func ("?AS400,1=add,3=AS400,_as4schg,*C,_as4crcn,*C,_as4sydd"). 
 
/*----- Delete Schema -----*/
on choose of menu-item mi_AS4V7_Delete    in menu mnu_AS400V7 
  &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
     run Perform_Func ("!AS400,_as4_del,*N,1=sys,_usrsget").
  &ELSE
     run Perform_Func ("!AS400,_as4_del,*N,1=sys,_guisget").
  &ENDIF


/*----------------------------DataServer/Odbc-----------------------------*/
 
/*----- Create Schema -----*/
on choose of menu-item mi_Odb_Create    in menu mnu_Odbc
   run Perform_Func
      ("?ODBC,1=add,3=ODBC,_usrschg,_gat_ini,*C,_gat_con,_odb_get,_odb_pul,_gat_cro").
 
/*----- Update File Def -----*/
on choose of menu-item mi_Odb_UpdFile   in menu mnu_Odbc
   run Perform_Func 
     ("!ODBC,1=upd,_gat_ini,*C,_gat_con,_odb_get,_odb_pul,_gat_cro").
 
/*----- Verify File Def -----*/
on choose of menu-item mi_Odb_VerFile   in menu mnu_Odbc
   run Perform_Func
     ("!ODBC,1=,_gat_ini,*C,_gat_con,25=compare,_odb_get,_odb_pul,_gat_cmp,_gat_cro").
 
/*----- Edit Connect Info -----*/
on choose of menu-item mi_Odb_ConnInfo  in menu mnu_Odbc
   run Perform_Func ("!ODBC,1=chg,3=ODBC,_usrschg").
 
/*----- Change Code Page -----*/
on choose of menu-item mi_Odb_ChgCP     in menu mnu_Odbc
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!ODBC,_gat_cp,_gat_cp1").
   &ELSE
   run Perform_Func ("!ODBC,_gat_cp,_gat_cp1").
   &ENDIF

/*----- Delete Schema -----*/
on choose of menu-item mi_Odb_Delete    in menu mnu_Odbc
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!ODBC,_usrsdel,*N,1=sys,_usrsget").
   &ELSE
   run Perform_Func ("!ODBC,_usrsdel,*N,1=sys,_guisget").
   &ENDIF 
   
/*----------------------------DataServer/ORACLE---------------------------*/

/*----- Create Schema -----*/
on choose of menu-item mi_ORA_Create    in menu mnu_ORACLE 
   run Perform_Func 
     ("?ORACLE,1=add,3=ORACLE,_usrschg,_gat_ini,*C,_gat_con,_ora_lk0,*C,_ora_lkc,_ora_lkl").

/*----- Update File Def -----*/
on choose of menu-item mi_ORA_UpdFile   in menu mnu_ORACLE 
   run Perform_Func 
     ("!ORACLE,1=upd,_gat_ini,*C,_gat_con,_ora_lk0,*C,_ora_lkc,_ora_lkl").

/*----- Verify File Def -----*/
on choose of menu-item mi_ORA_VerFile   in menu mnu_ORACLE 
   run Perform_Func 
     ("!ORACLE,1=,_gat_ini,*C,_gat_con,_ora_lk0,*C,_ora_lkc,25=compare,_ora_lkl").

/*----- Edit Connect Info -----*/
on choose of menu-item mi_ORA_ConnInfo  in menu mnu_ORACLE 
   run Perform_Func ("!ORACLE,1=chg,3=ORACLE,_usrschg").

/*----- Change Code Page -----*/
on choose of menu-item mi_ORA_ChgCP     in menu mnu_ORACLE
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!ORACLE,_gat_cp,_gat_cp1").
   &ELSE
   run Perform_Func ("!ORACLE,_gat_cp,_gat_cp1").
   &ENDIF

/*----- Delete Schema -----*/
on choose of menu-item mi_ORA_Delete    in menu mnu_ORACLE
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!ORACLE,_usrsdel,*N,1=sys,_usrsget").
   &ELSE
   run Perform_Func ("!ORACLE,_usrsdel,*N,1=sys,_guisget").
   &ENDIF

/*----- SQL Plus -----*/
on choose of menu-item mi_ORA_SQLPlus   in menu mnu_ORACLE
   run Perform_Func ("?ORACLE,1= ,2=plus31,3= ,4= ,5= ,6=sqlplus,7=sqlplus,*O").
/*
/*----- "Schema Migration Tools" pop-up menu: Generate DDL -----*/
on choose of menu-item mi_ora_GenDDL in menu mnu_ora_tools
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=a,_usrtget,22=ora,_usrtgen").
   &ELSE
   run Perform_Func ("1=a,_guitget,22=ora,_usrtgen").
   &ENDIF

/*----- "Schema Migration Tools" pop-up menu: Send DDL -----*/
on choose of menu-item mi_ora_SendDDL in menu mnu_ora_tools
   run Perform_Func ("@ORACLE,22=ora,ora/_sndsqui").

/*----- "Schema Migration Tools" pop-up menu: Adjust Schema -----*/
on choose of menu-item mi_ora_AdjstSI in menu mnu_ora_tools
   run Perform_Func ("!ORACLE,22=ora,ora/_beauty").
*/
/*----- "Schema Migration Tools" pop-up menu: Migrate DB to ORACLE -----*/
on choose of menu-item mi_ora_Migrate in menu mnu_ora_tools
   run Perform_Func ("?ORACLE,ora/protoora").

/*----------------------------DataServer/RMS-----------------------------*/

/*----- Create Schema -----*/
on choose of menu-item mi_RMS_Create    in menu mnu_RMS
   run Perform_Func ("?RMS,1=add,3=RMS,_usrschg,*C,7=r,_gat_set,_gat_nxt").

/*----- Add File Def -----*/
on choose of menu-item mi_RMS_AddFile   in menu mnu_RMS
   run Perform_Func ("!RMS,7=r,_gat_nxt").

/*----- Update File Def -----*/
on choose of menu-item mi_RMS_UpdFile   in menu mnu_RMS
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!RMS,_usrtget,2=chg,_rms_def,_usrtchg").
   &ELSE
   run Perform_Func ("!RMS,_guitget,2=chg,_rms_def,_usrtchg").
   &ENDIF

/*----- Import Defs -----*/
on choose of menu-item mi_RMS_Import    in menu mnu_RMS
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!RMS,_usrtget,_rms_cdd,_usrfchg").
   &ELSE
   run Perform_Func ("!RMS,_guitget,_rms_cdd,_usrfchg").
   &ENDIF

/*----- Redefine Index -----*/
on choose of menu-item mi_RMS_RedIdx    in menu mnu_RMS
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!RMS,_usrtget,_gat_rix").
   &ELSE
   run Perform_Func ("!RMS,_guitget,_gat_rix").
   &ENDIF

/*----- Display Record Layout -----*/
on choose of menu-item mi_RMS_DispRec   in menu mnu_RMS
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!RMS,_usrtget,_gat_off").
   &ELSE
   run Perform_Func ("!RMS,_guitget,_gat_off").
   &ENDIF

/*----- Edit Connect Info -----*/
on choose of menu-item mi_RMS_ConnInfo  in menu mnu_RMS
   run Perform_Func ("!RMS,1=chg,3=RMS,_usrschg").

/*----- Change Code Page -----*/
on choose of menu-item mi_RMS_ChgCP     in menu mnu_RMS
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!RMS,_gat_cp,_gat_cp1").
   &ELSE
   run Perform_Func ("!RMS,_gat_cp,_gat_cp1").
   &ENDIF

/*----- Delete Schema -----*/
on choose of menu-item mi_RMS_Delete    in menu mnu_RMS
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!RMS,_usrsdel,*N,1=sys,_usrsget").
   &ELSE
   run Perform_Func ("!RMS,_usrsdel,*N,1=sys,_guisget").
   &ENDIF


/*----------------------------DataServer/SYBASE-10------------------------*/
 
/*----- Create Schema -----*/
on choose of menu-item mi_s10_Create    in menu mnu_syb10
   run Perform_Func
      ("?SYB10,1=add,3=SYB10,_usrschg,_gat_ini,*C,_gat_con,_odb_get,_odb_pul,_gat_cro").
 
/*----- Update File Def -----*/
on choose of menu-item mi_s10_UpdFile   in menu mnu_syb10
   run Perform_Func
      ("!SYB10,1=upd,_gat_ini,*C,_gat_con,_odb_get,_odb_pul,_gat_cro").
 
/*----- Verify File Def -----*/
on choose of menu-item mi_s10_VerFile   in menu mnu_syb10
   run Perform_Func
      ("!SYB10,1=,_gat_ini,*C,_gat_con,25=compare,_odb_get,_odb_pul,_gat_cmp,_gat_cro").
 
/*----- Edit Connect Info -----*/
on choose of menu-item mi_s10_ConnInfo  in menu mnu_syb10
   run Perform_Func ("!SYB10,1=chg,3=SYB10,_usrschg").
 
/*----- Change Code Page -----*/
on choose of menu-item mi_s10_ChgCP     in menu mnu_syb10
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!SYB10,_gat_cp,_gat_cp1").
   &ELSE
   run Perform_Func ("!SYB10,_gat_cp,_gat_cp1").
   &ENDIF

/*----- Delete Schema -----*/
on choose of menu-item mi_s10_Delete    in menu mnu_syb10
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!SYB10,_usrsdel,*N,1=sys,_usrsget").
   &ELSE
   run Perform_Func ("!SYB10,_usrsdel,*N,1=sys,_guisget").
   &ENDIF

/*----- "Schema Migration Tools" pop-up menu: Generate DDL -----*/
on choose of menu-item mi_syb_GenDDL in menu mnu_syb_tools
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=a,_usrtget,22=s10,_usrtgen").
   &ELSE
   run Perform_Func ("1=a,_guitget,22=s10,_usrtgen").
   &ENDIF

/*----- "Schema Migration Tools" pop-up menu: Send DDL -----*/
on choose of menu-item mi_syb_SendDDL in menu mnu_syb_tools
   run Perform_Func ("@SYB10,22=s10,odb/_sndsqui").

/*----- "Schema Migration Tools" pop-up menu: Adjust Schema -----*/
on choose of menu-item mi_syb_AdjstSI in menu mnu_syb_tools
   run Perform_Func ("!SYB10,22=s10,odb/_beauty").

/*----- "Schema Migration Tools" pop-up menu: Migrate DB to SYBASE -----*/
on choose of menu-item mi_syb_Migrate in menu mnu_syb_tools
   run Perform_Func ("?SYB10,odb/protos10").

/*----------------------------DataServer/Rdb-----------------------------*/

/*----- Create Schema -----*/
on choose of menu-item mi_Rdb_Create    in menu mnu_Rdb
   run Perform_Func 
      ("?RDB,1=add,3=RDB,_usrschg,_gat_ini,*C,_gat_con,_rdb_get,_rdb_mak").

/*----- Update File Def -----*/
on choose of menu-item mi_Rdb_UpdFile   in menu mnu_Rdb
   run Perform_Func ("!RDB,1=upd,_gat_ini,*C,_gat_con,_rdb_get,_rdb_mak").

/*----- Verify File Def -----*/
on choose of menu-item mi_Rdb_VerFile   in menu mnu_Rdb
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!RDB,1=,_usrtget,9=pro,_rdb_vrf,_rdb_mak").
   &ELSE
   run Perform_Func ("!RDB,1=,_guitget,9=pro,_rdb_vrf,_rdb_mak").
   &ENDIF

/*----- Edit Connect Info -----*/
on choose of menu-item mi_Rdb_ConnInfo  in menu mnu_Rdb
   run Perform_Func ("!RDB,1=chg,3=RDB,_usrschg").

/*----- Change Code Page -----*/
on choose of menu-item mi_Rdb_ChgCP     in menu mnu_Rdb
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!Rdb,_gat_cp,_gat_cp1").
   &ELSE
   run Perform_Func ("!Rdb,_gat_cp,_gat_cp1").
   &ENDIF

/*----- Delete Schema -----*/
on choose of menu-item mi_Rdb_Delete    in menu mnu_Rdb
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("!RDB,_usrsdel,*N,1=sys,_usrsget").
   &ELSE
   run Perform_Func ("!RDB,_usrsdel,*N,1=sys,_guisget").
   &ENDIF

 /*###*/

/*-----------------------Utilities/Quoter menu--------------------------*/

/*----- ENTIRE LINES -----*/
on choose of menu-item mi_Quo_EntLines  in menu mnu_Quoter 
   run Perform_Func ("1=1,_usrquot").

/*----- By DELIMITER -----*/
on choose of menu-item mi_Quo_Delimiter in menu mnu_Quoter 
   run Perform_Func ("1=d,_usrquot").

/*----- BY COLUMN RANGES -----*/
on choose of menu-item mi_Quo_ColRanges in menu mnu_Quoter 
   run Perform_Func ("1=c,_usrquot").

/*----- INCLUDE FILE -----*/
on choose of menu-item mi_Quo_InclFile  in menu mnu_Quoter 
   run Perform_Func ("1=m,_usrquot").


/*------------------Utilities/General Include File menu------------------*/

/*----- ASSIGN -----*/
on choose of menu-item mi_Incl_Assign   in menu mnu_GenIncl 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,_usrcgen").
   &ELSE
   run Perform_Func ("_guitget,_usrcgen").
   &ENDIF

/*----- FORM -----*/
on choose of menu-item mi_Incl_FORM     in menu mnu_GenIncl 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,_usrfgen").
   &ELSE
   run Perform_Func ("_guitget,_usrfgen").
   &ENDIF

/*----- DEFINE WORKFILE -----*/
on choose of menu-item mi_Incl_WorkFile in menu mnu_GenIncl 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,_usrwgen").
   &ELSE
   run Perform_Func ("_guitget,_usrwgen").
   &ENDIF


/*-----------------------Utilities/other menu items---------------------*/

/*----- PARAMETER FILE EDITOR -----*/
on choose of menu-item mi_Util_ParmFile in menu mnu_Utilities 
   run Perform_Func ("_usrpfed").

/*----- AUTO-CONNECT EDITOR -----*/
on choose of menu-item mi_Util_AutoConn  in menu mnu_Utilities
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrauto").
   &ELSE
   run Perform_Func ("_guiauto").
   &ENDIF

/*----- FREEZE/UNFREEZE -----*/
on choose of menu-item mi_Util_Freeze   in menu mnu_Utilities 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("_usrtget,_usrcold").
   &ELSE
   run Perform_Func ("_guitget,_usrcold").
   &ENDIF

/*----- INDEX DEACTIVATE -----*/
on choose of menu-item mi_Util_IdxDeact in menu mnu_Utilities
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("9=note,_usriact,1=a,_usrtget,9=off,_usriact").
   &ELSE
   run Perform_Func ("9=note,_usriact,1=a,_guitget,9=off,_usriact").
   &ENDIF

/*----- INFORMATION -----*/
on choose of menu-item mi_Util_Info     in menu mnu_Utilities 
   run Perform_Func ("_usrinfo").


/*---------------------------PRO/SQL menu-------------------------------*/

/*----- VIEW REPORT -----*/
on choose of menu-item mi_SQL_ViewRpt   in menu mnu_SQL 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=a,_usrvget,_rptvqik").
   &ELSE
   run Perform_Func ("1=a,_guivget,_rptvqik").
   &ENDIF

/*----- DUMP AS CREATE VIEW -----*/
on choose of menu-item mi_SQL_DumpView  in menu mnu_SQL 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=a,_usrvget,_usrvgen").
   &ELSE
   run Perform_Func ("1=a,_guivget,_usrvgen").
   &ENDIF

/*----- DUMP AS CREATE TABLE -----*/
on choose of menu-item mi_SQL_DumpTable in menu mnu_SQL 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=a,_usrtget,_usrtgen").
   &ELSE
   run Perform_Func ("1=a,_guitget,_usrtgen").
   &ENDIF


/*--------------------------Reports menu----------------------------------*/

/*----- DETAILED TABLE REPORT -----*/
on choose of menu-item mi_Rpt_DetTbl    in menu mnu_Reports 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=a,_usrtget,9=,_rptflds").
   &ELSE
   run Perform_Func ("1=a,_guitget,9=,_rptflds").
   &ENDIF

/*----- TABLE REPORT -----*/
on choose of menu-item mi_Rpt_Table     in menu mnu_Reports 
   run Perform_Func ("_rpttqik").

/*----- FIELD REPORT -----*/
on choose of menu-item mi_Rpt_Field     in menu mnu_Reports 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=a,_usrtget,19=o,_rptfqik").
   &ELSE
   run Perform_Func ("1=a,_guitget,19=o,_rptfqik").
   &ENDIF

/*----- INDEX REPORT -----*/
on choose of menu-item mi_Rpt_Index     in menu mnu_Reports 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=a,_usrtget,_rptiqik").
   &ELSE
   run Perform_Func ("1=a,_guitget,_rptiqik").
   &ENDIF

/*----- VIEW REPORT -----*/
on choose of menu-item mi_Rpt_View      in menu mnu_Reports 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=a,_usrvget,_rptvqik").
   &ELSE
   run Perform_Func ("1=a,_guivget,_rptvqik").
   &ENDIF

/*----- SEQUENCE REPORT -----*/
on choose of menu-item mi_Rpt_Sequence  in menu mnu_Reports 
   run Perform_Func ("_rptsqik").

/*----- TRIGGER REPORT -----*/
on choose of menu-item mi_Rpt_Trigger   in menu mnu_Reports 
   run Perform_Func ("_rpttrig").

/*----- USER REPORT -----*/
on choose of menu-item mi_Rpt_User      in menu mnu_Reports 
   run Perform_Func ("_rptuqik").

/*----- TABLE RELATIONS REPORT -----*/
on choose of menu-item mi_Rpt_TblRel    in menu mnu_Reports 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   run Perform_Func ("1=a,_usrtget,_rptrels").
   &ELSE
   run Perform_Func ("1=a,_guitget,_rptrels").
   &ENDIF


/*------------------------------Tools menu-------------------------------*/

&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   {adecomm/toolrun.i &MENUBAR       = "mnu_Admin_Tool"
		      &EXCLUDE_ADMIN = yes 
      	       	      &EXCLUDE_DICT  = yes
   }
&ELSE
   {adecomm/toolrun.i &MENUBAR       = "mnu_Admin_Tool"
		      &EXCLUDE_ADMIN = yes 
   }
&ENDIF
if tool_bomb then return.  /* admin is already running so quit. */


/*-----------------------------Help menu----------------------------------*/

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
/*----- HELP CONTENTS -----*/
on choose of menu-item mi_Hlp_Topics in menu mnu_Help
   RUN "adecomm/_adehelp.p" ("admn", "TOPICS", ?, ?).

/*----- MESSAGES HELP -----*/
on choose of menu-item mi_Hlp_Messages in menu mnu_Help
   run prohelp/_msgs.p.

/*----- RECENT MESSAGES HELP -----*/
on choose of menu-item mi_Hlp_Recent   in menu mnu_Help
   run prohelp/_rcntmsg.p.

/*----- HELP ABOUT ADMIN -----*/
on choose of menu-item mi_Hlp_About in MENU mnu_Help
   run adecomm/_about.p ("Database Administration", "adeicon/admin%").

&ENDIF



