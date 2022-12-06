/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _libnam.p

Description:
   The name field of a create dialog or property window has been "left".
   Check the name to see if it's allright.

   Note: This is a separate .p instead of an internal procedure in edittrig.i
   in order to make edittrig.i smaller.  I was hitting the size limit!.

Input Parameters:
   p_OrigName - If editing, this was the original name before we sensitive it
      	        for edit.  If adding, this is ignored.
   p_Win      - If editing the window to parent any alert boxes to.  If
      	        adding this is ignored.

Output Parameters:
   p_Name     - Set to the new library name value.      
   f_name      -  Set to File name
   p_Okay     - Set to:
      	       	  yes if name is okay, and validation on it should continue.
      	       	  no  if name is invalid and caller should return NO-APPLY
 
Author: Laura Stern

Date Created: 07/14/92 

----------------------------------------------------------------------------*/

{as4dict/dictvar.i shared}

Define INPUT  PARAMETER p_OrigName          as char    	      NO-UNDO.
Define INPUT  PARAMETER p_Win  	                     as widget-handle   NO-UNDO.           
Define  INPUT PARAMETER f_name                    as char                       NO-UNDO. 
Define OUTPUT PARAMETER p_Name 	    as char 	      NO-UNDO.
Define OUTPUT PARAMETER p_Okay 	    as logical 	      NO-UNDO.    


p_Name = TRIM (SELF:screen-value). 
p_Okay = ?.
 
IF p_Name = ? or p_Name = "" then assign p_Name = p_OrigName.

IF f_Name = ? or f_Name = "" then do:        
    p_Okay = no.
    return.
end.
    
/* Make sure the file and library names are available.  Used CHKF because it
     will check file and library both.  */   

dba_cmd = "CHKF".
RUN as4dict/_dbaocmd.p 
	 (INPUT "PF", 
	  INPUT f_name,
      	  INPUT p_name,
	  INPUT 0,
	  INPUT 0).

if dba_return =   2 then DO :
     dba_cmd = "RESERVE".
     RUN as4dict/_dbaocmd.p 
	 (INPUT "PF", 
	  INPUT f_name,
      	  INPUT p_name,
	  INPUT 0,
	  INPUT 0).     
    if dba_return = 12 then
                   ASSIGN  reserved = yes
                                     p_okay = yes.
    else  do:                              
        RUN as4dict/_dbamsgs.p.       
         ASSIGN p_okay = no.
     end.
end.               
ELSE IF dba_return = 1 AND dba_unres THEN.    
ELSE IF dba_return = 1 AND NOT dba_unres THEN DO:
  MESSAGE "Physical object already exists in Library" 
                           "with this name. "  SKIP
                   VIEW-AS ALERT-BOX ERROR BUTTON OK.                
     ASSIGN p_okay = no.                   
END.
ELSE IF dba_return = 11 THEN DO:
    MESSAGE "Physical object already exists in Library" 
                           "with this name."  SKIP
                   VIEW-AS ALERT-BOX ERROR BUTTON OK.                
     ASSIGN p_okay = no.                   
END.
ELSE DO:                     
    RUN as4dict/_dbamsgs.p.  
        p_okay = no.
END.
 
SELF:screen-value = CAPS(p_Name).  /* Reset the trimmed value */





