/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------
File: menu.i

Description:
   This include file contains the definition of the dictionary menu.
 
Arguments:
   {1} - this is either "new shared" or "shared".

Author: Laura Stern

Date Created: 02/17/92 
----------------------------------------------------------------------------*/

{adecomm/toolmenu.i &EXCLUDE_DICT = yes}


Define sub-menu s_mnu_QuickFld
   menu-item mi_f_CurrTbl   label "&Selected Table"
   menu-item mi_f_AllTbls   label "&All Tables".

Define sub-menu s_mnu_QuickIdx
   menu-item mi_i_CurrTbl   label "&Selected Table"
   menu-item mi_i_AllTbls   label "&All Tables".

Define sub-menu s_mnu_TblRel
   menu-item mi_r_CurrTbl   label "&Selected Table"
   menu-item mi_r_AllTbls   label "&All Tables".

Define sub-menu s_mnu_Reports	
   menu-item mi_DetailedTbl   	 label "&Detailed Table..."
   menu-item mi_QuickTbl         label "Quick &Table"
   sub-menu s_mnu_QuickFld       label "Quick &Field"
   sub-menu s_mnu_QuickIdx       label "Quick &Index"
   menu-item mi_QuickViw         label "PRO/SQL &View"
   menu-item mi_QuickSeq      	 label "&Sequence"
   menu-item mi_Trigger	      	 label "Tri&gger"
   menu-item mi_QuickUsr         label "&User"
   sub-menu  s_mnu_TblRel        label "Table &Relations".

Define sub-menu s_mnu_Database
   menu-item mi_Crt_Database	 label "&Create..."   ACCELERATOR "SHIFT-F3"
   menu-item mi_Connect		 label "Co&nnect..."  ACCELERATOR "F3"
   menu-item mi_Disconnect    	 label "&Disconnect"  ACCELERATOR "F8"
   sub-menu  s_mnu_Reports	 label "&Reports"
   menu-item mi_Exit		 label "E&xit".
      
Define sub-menu s_mnu_Edit	
   menu-item mi_Undo	      	 label "&Undo Transaction"  ACCELERATOR "CTRL-Z"
   menu-item mi_Commit	      	 label "&Commit Transaction" 
      	       	     	      	       	     	      	    ACCELERATOR "CTRL-Y"
   RULE	     
   menu-item mi_Delete      	 label "&Delete"            ACCELERATOR "CTRL-D"
   menu-item mi_Properties	 label "&Properties..."     ACCELERATOR "CTRL-P".
      
Define sub-menu s_mnu_Create      
   menu-item mi_Crt_Table	 label "&Table..."    ACCELERATOR "CTRL-T"
   /* menu-item mi_Crt_Domain	 label "&Domain..."  */
   menu-item mi_Crt_Sequence	 label "&Sequence..." ACCELERATOR "CTRL-S"
   menu-item mi_Crt_Field     	 label "&Field..."    ACCELERATOR "CTRL-F"
   menu-item mi_Crt_Index     	 label "&Index..."    ACCELERATOR "CTRL-X".

Define sub-menu s_mnu_Options
   menu-item mi_Field_Rename   	 label "&Globally Rename Fields..."
   menu-item mi_Field_Renumber 	 label "&Renumber Fields in Table..."
   RULE
   menu-item mi_Mode_Db 	 label "&Database Mode" ACCELERATOR "SHIFT-F6"
   menu-item mi_Mode_Tbl 	 label "&Table Mode"    ACCELERATOR "SHIFT-F7"
   menu-item mi_Mode_Seq	 label "&Sequence Mode" ACCELERATOR "SHIFT-F8"
   menu-item mi_Mode_Fld	 label "&Field Mode"    ACCELERATOR "SHIFT-F9"
   menu-item mi_Mode_Idx	 label "&Index Mode"    ACCELERATOR "SHIFT-F10".

Define sub-menu s_mnu_View
   menu-item mi_Order_Fields  label "&Order Fields Alphabetically"
   menu-item mi_Show_Hidden   label "&Show Hidden Tables" TOGGLE-BOX.

Define sub-menu s_mnu_Help SUB-MENU-HELP
   menu-item mi_Contents    label "&Help Topics"
   RULE
   menu-item mi_messages    label "M&essages..."
   menu-item mi_recent      label "&Recent Messages..."
   RULE
   menu-item mi_About       label "&About Dictionary".

Define {1} menu s_mnu_Dict
   MENUBAR
   sub-menu s_mnu_Database	 label "&Database"
   sub-menu s_mnu_Edit		 label "&Edit"
   sub-menu s_mnu_Create      	 label "&Create"
   sub-menu s_mnu_View	      	 label "&View"
   sub-menu s_mnu_Options	 label "&Options"
   sub-menu mnu_Tools            label "&Tools"
   sub-menu s_mnu_Help	      	 label "&Help".




