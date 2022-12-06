/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: fldvar.i

Description:   
   Include file which defines the user interface components and related data
   for the main field editor window and its subsidiary dialog boxes.
   Since domains and fields are essentially the same, this is used for
   domains as well.
 
Arguments:
   {1} - this is either "new shared" or "shared".

Author: Laura Stern

Date Created: 02/04/92 
----------------------------------------------------------------------------*/


Define {1} buffer   b_Field for _Field. 

/*--- Save policy has been changed --------------------------------
   this is obsolete but leave in case we change our minds!

Define {1} workfile w_Field NO-UNDO like DICTDB._Field.
Define {1} workfile w_FTrig NO-UNDO like DICTDB._Field-trig.
------------------------------------------------------------------*/

Define {1} frame newfld.    /* for create field/domain dialog box */
Define {1} frame fldprops.  /* field properties */
/* Define {1} frame domprops. */ /* domain properties */ 

/* The main field properties - mostly we user the record buffer. 
   Note: We can't use the data type or format field of the record buffer 
   as part of a combo box - combo triggers would have to have
   "Using buffer" phrase.  So create a variable for it.
*/
Define button  s_btn_Fld_DType IMAGE-UP FILE "btn-down-arrow".
Define {1} var s_Fld_DType     as char format "x(32)" NO-UNDO.
Define {1} var s_lst_Fld_Dtype as char
   view-as SELECTION-LIST SINGLE  
   INNER-CHARS 32 INNER-LINES 7 SCROLLBAR-VERTICAL.

Define button s_btn_Fld_Format LABEL "&Examples..." SIZE 15 by 1.125.

Define {1} var s_Fld_Array     as logical NO-UNDO.
Define {1} var s_Fld_InIndex   as logical NO-UNDO 
   format "Member of an Index: yes/Member of an Index: no". 
Define {1} var s_Fld_InView    as logical NO-UNDO
   format "Member of a View: yes/Member of a View: no". 

Define button s_btn_Fld_Copy   label "Copy Fiel&d..." SIZE 17 by 1.125.

Define button s_btn_Fld_Triggers    label "Tri&ggers..."     SIZE 17 by 1.125.
Define button s_btn_Fld_Validation  label "Valida&tion..."   SIZE 17 by 1.125.
Define button s_btn_Fld_ViewAs      label "Vie&w-As..."      SIZE 17 by 1.125.
Define button s_btn_Fld_StringAttrs label "St&ring Attrs..." SIZE 17 by 1.125.
Define button s_btn_Fld_Gateway	  label "DataSer&ver..."   SIZE 17 by 1.125.

/* This is the form for the field properties, new field and new
   domain windows. newfld is used for both fields and domains.  We
   can use the same frame for "new" because only one will be displayed
   at a given time - not the case for properties.
*/
{adedict/FLD/fldprop.f  
   &frame_phrase = "frame fldprops NO-BOX
		    default-button s_btn_OK cancel-button s_btn_Close"
   &apply_btn    = s_btn_Save
   &other_btns   = "SPACE({&HM_DBTN}) s_btn_Close SPACE({&HM_DBTNG}) 
		    s_btn_Prev SPACE({&HM_DBTN}) s_btn_Next"
}

{adedict/FLD/fldprop.f  
   &frame_phrase = "frame newfld view-as DIALOG-BOX TITLE ""Create Field""
      	       	    default-button s_btn_Add cancel-button s_btn_Done"
   &apply_btn    = s_btn_Add
   &other_btns   = "SPACE({&HM_DBTN}) s_btn_Done"
}

/* This is the form for the domain properties window. */
/*------------
{adedict/FLD/fldprop.f  &frame_phrase = "frame domprops CENTERED NO-BOX"}
-------------*/

/* Variables to save data type info. */
Define {1} var s_Fld_Gatetype as char  	 NO-UNDO.  /* gate dtype short string */
Define {1} var s_Fld_Protype  as char  	 NO-UNDO.  /* pro dtype string */
Define {1} var s_Fld_Typecode as integer NO-UNDO.  /* Data type integer code */	

/* Field capabilities to handle gateways properly. */
Define {1} var s_Fld_Capab as char NO-UNDO.

/*--- Save policy has been changed --------------------------------
   this is obsolete but leave in case we change our minds!

/* Flags which when true indicates that trigger records or gateway info 
   have been saved in the work file for the current field. */
Define {1} var s_FldTrigs_Stashed as logical NO-UNDO.
Define {1} var s_FldGate_Stashed  as logical NO-UNDO.
------------------------------------------------------------------*/

/* Symbolic constants for dtype values. */
&global-define 	  DTYPE_CHARACTER   1
&global-define 	  DTYPE_DATE  	    2
&global-define 	  DTYPE_LOGICAL     3
&global-define 	  DTYPE_INTEGER	    4
&global-define 	  DTYPE_DECIMAL	    5
&global-define 	  DTYPE_RAW   	    6
&global-define 	  DTYPE_RECID 	    7

/* adjusting of the InView and InIndex fields */
do with frame fldprops:
  assign
    s_Fld_InIndex:x = s_btn_Fld_Gateway:x
                    + s_btn_Fld_Gateway:width-pixels
                    - s_Fld_InIndex:width-pixels
    s_Fld_InView:x  = s_btn_Fld_Gateway:x
                    + s_btn_Fld_Gateway:width-pixels
                    - s_Fld_InView:width-pixels.
  end.
do with frame newfld:
  assign
    s_Fld_InIndex:x = s_btn_Fld_Gateway:x
                    + s_btn_Fld_Gateway:width-pixels
                    - s_Fld_InIndex:width-pixels
    s_Fld_InView:x  = s_btn_Fld_Gateway:x
                    + s_btn_Fld_Gateway:width-pixels
                    - s_Fld_InView:width-pixels.
  end.





