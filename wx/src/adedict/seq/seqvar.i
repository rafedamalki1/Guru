/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: seqvar.i

Description:   
   Include file which defines the user interface components for the add
   sequence and edit sequence forms.
 
Arguments:
   {1} - this is either "new shared" or "shared".

Author: Laura Stern

Date Created: 02/20/92 
----------------------------------------------------------------------------*/


Define {1} buffer b_Sequence  for DICTDB._Sequence.

Define {1} frame newseq.    /* for adding a new sequence */
Define {1} frame seqprops.  /* sequence properties */
Define {1} var s_Seq_Type     as   char                      NO-UNDO. 
Define {1} var s_Seq_Limit    like DICTDB._Sequence._Seq-max NO-UNDO.



/* This is the form for the seqprops and newseq windows. */
&IF "{&WINDOW-SYSTEM}" begins "MS-WIN" &THEN
   {adedict/SEQ/seqprop.f  
      &frame_phrase = "frame seqprops NO-BOX 
       default-button s_btn_OK cancel-button s_btn_Close"
      &apply_btn  = s_btn_Save
      &other_btns = "SPACE({&HM_DBTN}) s_btn_Close SPACE({&HM_DBTNG}) 
     s_btn_Prev SPACE({&HM_DBTN}) s_btn_Next"
      &col1 = 19
      &col2 = 21
   }
&ELSE /* Motif */
   {adedict/SEQ/seqprop.f  
      &frame_phrase = "frame seqprops NO-BOX 
       default-button s_btn_OK cancel-button s_btn_Close"
      &apply_btn  = s_btn_Save
      &other_btns = "SPACE({&HM_DBTN}) s_btn_Close SPACE({&HM_DBTNG}) 
     s_btn_Prev SPACE({&HM_DBTN}) s_btn_Next"
      &col1 = 23
      &col2 = 25
   }
&ENDIF

{adedict/SEQ/seqprop.f  
   &frame_phrase = "frame newseq view-as DIALOG-BOX TITLE ""Create Sequence""
                 default-button s_btn_Add cancel-button s_btn_Done"
   &apply_btn  = s_btn_Add
   &other_btns = "SPACE({&HM_DBTN}) s_btn_Done"
   &col1 = 19
   &col2 = 21
}



