/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: newidx.f

Description:   
   This file contains the form for adding an index.

Author: Laura Stern

Date Created: 04/22/92

----------------------------------------------------------------------------*/


form
   SKIP({&TFM_WID})
 
  b_Index._Index-Name 	label "Index &Name"  colon 20 {&STDPH_FILL}
   SKIP({&VM_WID})
   b_Index._Desc        label "Descri&ption" colon 20
      	       	     	view-as EDITOR SCROLLBAR-VERTICAL
      	       	     	SIZE 46 BY 2                 {&STDPH_EDITOR}
   SKIP({&VM_WIDG})

   s_Idx_Primary     	label "P&rimary"    at 9 
      	       	     	view-as TOGGLE-BOX  SPACE({&HM_WIDG})
   b_Index._Active   	label "Ac&tive" 	    
      	       	     	view-as TOGGLE-BOX  SPACE({&HM_WIDG})
   b_Index._Unique 	label "Uni&que"      
      	       	     	view-as TOGGLE-BOX  SPACE({&HM_WIDG})
   s_Idx_Word           label "&Word Index"  
                        view-as TOGGLE-BOX  SPACE({&HM_WIDG})
   s_Idx_Abbrev         label "A&bbreviated"     
      	       	     	view-as TOGGLE-BOX 
   SKIP({&VM_WIDG})

   "Select field to add to index:":t33   	    at 2
                        view-as TEXT                               
   "New Index's Fields:":t25 		    at 51
                        view-as TEXT                               
   SKIP

   s_lst_IdxFldChoice 	NO-LABEL      	    at 2
                        view-as SELECTION-LIST SINGLE 
                        INNER-CHARS 25 INNER-LINES 12
                        SCROLLBAR-V SCROLLBAR-H        
   
   s_lst_IdxFlds     	NO-LABEL       	    at 51 
                        view-as SELECTION-LIST SINGLE 
                        INNER-CHARS 28 INNER-LINES 12     
                        SCROLLBAR-V SCROLLBAR-H       
   SKIP({&VM_WID})

   s_IdxFld_AscDesc	NO-LABEL            at 51
   SKIP(.1)

   s_Status             NO-LABEL            at 2
                        format "x(50)" view-as TEXT 

   {adecomm/okform.i
      &BOX    = s_rect_btns
      &STATUS = yes
      &OK     = s_btn_OK
      &CANCEL = s_btn_Add /* where cancel button usually goes */
      &OTHER  = "SPACE({&HM_DBTN}) s_btn_Done"
      &HELP   = s_btn_Help}

   s_btn_IdxFldAdd   	      	       	     at col 33 row 8 SKIP({&VM_WID})
   s_btn_IdxFldRmv   	      	       	     at     33       SKIP({&VM_WIDG})
   s_btn_IdxFldDwn                           at     33       SKIP({&VM_WID})
   s_btn_IdxFldUp                            at     33

   with frame newidx
      default-button s_btn_Add cancel-button s_btn_Done
      SIDE-LABELS SCROLLABLE
      TITLE "Create Index" view-as DIALOG-BOX.




