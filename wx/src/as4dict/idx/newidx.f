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
    Modified: 01/95    DLM Changed to work with PROGRESS/400
              06/24/97 DLM Added Word Index support          

----------------------------------------------------------------------------*/


form
   SKIP({&TFM_WID})
 
  b_Index._Index-Name 	label "Index &Name"  colon 20 {&STDPH_FILL}
   SKIP 
  b_Index._AS4-file     label "AS/&400 Name" FORMAT "x(10)" colon 20 {&STDPH_FILL}   
  b_Index._AS4-Library label "Library Name" FORMAT "x(10)" {&STDPH_FILL}
    SKIP
   b_Index._Desc        label "&Description" colon 20  FORMAT "x(50)"   {&STDPH_FILL}
   SKIP({&TFM_WID})

   s_Idx_Primary     	label "&Primary"    at 10 
      	       	     	view-as TOGGLE-BOX  SPACE({&HM_WIDG})
   s_Idx_Active   	label "A&ctive" 	    
      	       	     	view-as TOGGLE-BOX  SPACE({&HM_WIDG})
   s_Idx_Unique 	label "Uni&que"      
      	       	     	view-as TOGGLE-BOX  SPACE({&HM_WIDG})
                       	       	     	
   s_Idx_Abbrev         label "A&bbreviated"     
      	       	     	view-as TOGGLE-BOX 
   SKIP({&TFM_WID})
   s_Idx_Word           label "&Word Index   Word Size"  AT 20
                        view-as TOGGLE-BOX 
    word_size          no-label  SKIP({&VM_WIDG}) 
    "Select field to add to index:"   	    at 2
                        view-as TEXT                               
   "New Index's Fields:" 		    at 47 
                        view-as TEXT                               
   SKIP({&VM_WIDG})

   s_lst_IdxFldChoice 	NO-LABEL      	    at 2
                        view-as SELECTION-LIST SINGLE 
                        INNER-CHARS 25 INNER-LINES 10
                        SCROLLBAR-V SCROLLBAR-H        
   
   s_lst_IdxFlds     	NO-LABEL       	    at 47 
                        view-as SELECTION-LIST SINGLE 
                        INNER-CHARS 28 INNER-LINES 10     
                        SCROLLBAR-V SCROLLBAR-H       
   SKIP({&VM_WID})

   s_IdxFld_AscDesc	NO-LABEL            at 47 
   SKIP /*(.1)*/

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

