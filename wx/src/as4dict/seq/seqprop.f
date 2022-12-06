/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: seqprop.f

Description:   
   This file contains the form for adding and editing sequences.

Arguments:
   &frame_phrase - this should be a frame phrase for the frame that we want
                   to associate this form with. e.g., "frame foo OVERLAY".
   &apply_btn    - The apply button (e.g., Save or Create)
   &other_btns   - phrase for layout of other default buttons
   &col1         - column for lining up the colons
   &col2         - column for other stuff
 
Author: Laura Stern

Date Created: 02/20/92       
           Modified 03/17/95 to work with Progress/400 Data Dictionary D. McMann

----------------------------------------------------------------------------*/


form
   SKIP({&TFM_WID})

   b_Sequence._Seq-Name	   label "Sequence &Name"   colon {&col1} {&STDPH_FILL}
   SKIP({&VM_WID})

   b_Sequence._Seq-Init	   label "Initial &Value"   FORMAT "->>>>>>>>9"
    colon {&col1} {&STDPH_FILL}  SKIP({&VM_WID})

   b_Sequence._Seq-Incr    label "&Increment by"     FORMAT "->>>>>>>>9"
      colon {&col1} {&STDPH_FILL} SKIP({&VM_WID})

   s_Seq_Limit 	     	   label "&Upper Limit"     FORMAT "->>>>>>>>9"
         colon {&col1} {&STDPH_FILL} SKIP({&VM_WID})

   s_Seq_Cycle_Ok          label "&Cycle at Limit?" at    {&col2}
      	       	     	   view-as toggle-box                
   SKIP({&VM_WID})

   SKIP(.25)

   s_Status                NO-LABEL format "x(50)"  at    2 
                           view-as TEXT 

   {adecomm/okform.i
      &BOX    = s_rect_btns
      &STATUS = yes
      &OK     = s_btn_OK
      &CANCEL = {&apply_btn}
      &OTHER  = "{&other_btns}"
      &HELP   = s_btn_Help}

   with {&frame_phrase} SIDE-LABELS.








