/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: idxvar.i

Description:   
   Include file which defines the user interface components and related data
   for the add index dialog box.
 
Arguments:
   {1} - this is either "new shared" or "shared".

Author: Laura Stern

Date Created: 04/22/92 
----------------------------------------------------------------------------*/
Define {1} frame newidx.    /* new index dialog box */
Define {1} frame idxprops.  /* index properties */

	   
/* The variables needed for both add and properties - we also use the 
   b_Index record buffer. */
Define {1} var s_Idx_Primary as logical NO-UNDO.
Define {1} var s_Idx_Abbrev  as logical NO-UNDO.
/* FIX - remove s_Idx_Word if toggles work on integers */
Define {1} var s_Idx_Word    as logical NO-UNDO.  
Define {1} var s_lst_IdxFlds as char    NO-UNDO. 


/* Variables needed for the add index dialog only. */
Define {1} var s_lst_IdxFldChoice as char NO-UNDO.

Define button s_btn_IdxFldAdd label "&Add >>"     SIZE 15 by 1.125.
Define button s_btn_IdxFldRmv label "<< &Remove"  SIZE 15 by 1.125.
Define button s_btn_IdxFldDwn label "Move &Down"  SIZE 15 by 1.125.
Define button s_btn_IdxFldUp  label "Move &Up"    SIZE 15 by 1.125.

Define {1} var s_IdxFld_AscDesc as char NO-UNDO
   view-as radio-set horizontal
   radio-buttons "A&scending", "A", "D&escending", "D".

/* Variables needed for index properties only. */
Define {1} var s_txt_List_Labels as char NO-UNDO extent 2.
Define {1} var ActRec as logical no-undo view-as toggle-box.
/* These are the forms for the index properties and new index windows. */
{adedict/IDX/idxprop.f "{1}"}
{adedict/IDX/newidx.f}


