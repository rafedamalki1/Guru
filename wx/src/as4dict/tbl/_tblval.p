/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _tblval.p

Description:   
   Display and handle the table validation dialog box.

Returns: "mod" if user OK'ed changes (though we really don't
      	 check to see if the values actually are different),
      	 "" if user Cancels.

Author: Laura Stern

Date Created: 02/18/92 

Modified on 07/08/94 by gfs - Fixed bug 94-06-14-073.

----------------------------------------------------------------------------*/


{as4dict/dictvar.i shared}
{as4dict/uivar.i shared}
{as4dict/TBL/tblvar.i shared}
{as4dict/menu.i shared}

Define var retval as char NO-UNDO init "". /* return value */

form
   SKIP({&TFM_WID})
   "Expression that must be TRUE to allow record deletions:"  at  2 
      	       	    view-as TEXT 
   SKIP
   b_File._Valexp   view-as EDITOR SCROLLBAR-VERTICAL
                    SIZE 73 BY 3 {&STDPH_ED4GL_SMALL}       at  2 
   SKIP({&VM_WIDG})

   "Message to be displayed for disallowed deletions:" 	      at  2   
      	       	    view-as TEXT
   SKIP
   b_File._Valmsg   format "x(72)" {&STDPH_FILL}   	      at  2  

   {adecomm/okform.i
      &BOX    = s_rect_btns
      &STATUS = no
      &OK     = s_btn_OK
      &CANCEL = s_btn_Cancel
      &HELP   = s_btn_Help}
   
   with frame tblvalid
        DEFAULT-BUTTON s_btn_OK CANCEL-BUTTON s_btn_Cancel
        NO-LABELS  
        view-as DIALOG-BOX TITLE "Table Validation".       
  

/*------------------------------Trigger Code---------------------------------*/

/*-----WINDOW-CLOSE-----*/
on window-close of frame tblvalid
   apply "END-ERROR" to frame tblvalid.


/*----- SELECTION of OK BUTTON or GO -----*/
on GO of frame tblvalid	   /* or OK due to AUTO-GO */
do:
   Define var msg  as char NO-UNDO.
   Define var expr as char NO-UNDO.

   assign
      msg = TRIM(input frame tblvalid b_File._Valmsg)
      expr = TRIM(input frame tblvalid b_File._Valexp).

   if NOT (msg = "" OR msg = ?) then 
   do:  /* message is not blank */
      if expr = "" OR expr = ? then    
      do:
      	 message "Please enter a validation expression to" SKIP
      	       	 "go along with your validation message."
      	       	 view-as ALERT-BOX ERROR buttons OK.
      	 apply "entry" to b_File._Valexp in frame tblvalid.
      	 return NO-APPLY.
      end.
   end.
   else  /* message is blank */ 
      if NOT (expr = "" OR expr = ?) then 
      do:
	 message "Please enter a validation message to" SKIP
		  "go along with your validation expression."
		  view-as ALERT-BOX ERROR buttons OK.
	 apply "entry" to b_File._Valexp in frame tblvalid.
	 return NO-APPLY.
      end.
end.


/*----- HELP -----*/
on HELP of frame tblvalid OR choose of s_btn_Help in frame tblvalid  
    RUN "adecomm/_kwhelp.p" (INPUT b_File._Valexp:HANDLE,
                             INPUT "as4d",
                             INPUT {&AS4_Table_Validation_Dlg_Box}).

/*------------------------------Mainline Code--------------------------------*/

/* Run time layout for button area. */
{adecomm/okrun.i  
   &FRAME = "frame tblvalid" 
   &BOX   = "s_rect_Btns"
   &OK    = "s_btn_OK" 
   &HELP  = "s_btn_Help"
}

/* So Return doesn't hit default button in editor widget */
b_File._Valexp:RETURN-INSERT = yes.

do ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE  ON STOP UNDO, LEAVE:
   if s_Tbl_ReadOnly then
   do:
      display b_File._Valmsg b_File._Valexp with frame tblvalid.     
      color display normal   b_file._Valexp with frame tblvalid.    
          update s_btn_Cancel  s_btn_Help  with frame tblvalid.       

   end.
   else do:
      update b_File._Valexp   
      	     b_File._Valmsg
      	     s_btn_Ok  	      
      	     s_btn_Cancel
      	     s_btn_Help
      	     with frame tblvalid.

      /* Remove any line feeds (which we get on WINDOWS) and remove
      	 extraneous spaces and carriage returns. */
      ASSIGN b_File._Valexp = TRIM(replace (b_File._Valexp, CHR(13), ""))    
                       b_File._Fil-Misc1[1] = b_File._Fil-Misc1[1] + 1  
                       b_File._Fil-Res1[8] = 1.             
                       
      IF b_File._Fil-res1[7] < 0 then assign b_File._Fil-res1[7] = 0.                   

      if NOT s_Adding then
      	 {as4dict/setdirty.i &Dirty = "true"}.
      retval = "mod".
   end.
end.

hide frame tblvalid.  
return retval.



