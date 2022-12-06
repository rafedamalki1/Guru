/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: tblvar.i

Description:   
   Include file which defines the user interface components and related data
   for the main table editor window.
 
Arguments:
   {1} - this is either "new shared" or "shared".

Author: Laura Stern

Date Created: 03/06/92 
----------------------------------------------------------------------------*/


Define {1} buffer   b_File          for  DICTDB._File. 

/*--- Save policy has been changed --------------------------------
   this is obsolete but leave in case we change our minds!

Define {1} workfile w_File  NO-UNDO like DICTDB._File.
Define {1} workfile w_TTrig NO-UNDO like DICTDB._File-trig.
------------------------------------------------------------------*/

Define {1} frame newtbl.    /* for create table dialog box */
Define {1} frame tblprops.  /* table properties */

/* The main table properties - mostly we user the record buffer. */
Define {1} var s_Tbl_Type     as char              NO-UNDO. 
Define {1} var s_Tbl_IdxCnt   as integer format ">>>9"  NO-UNDO.

Define button s_btn_Tbl_Triggers    label "Tri&ggers..."     SIZE 17 by 1.125.
Define button s_btn_Tbl_Validation  label "Valida&tion..."   SIZE 17 by 1.125.
Define button s_btn_Tbl_StringAttrs label "St&ring Attrs..." SIZE 17 by 1.125.
Define button s_btn_Tbl_ds          label "DataSer&ver..."   SIZE 17 by 1.125.

/* These are the forms for the table properties and new table windows. */
&IF "{&WINDOW-SYSTEM}" begins "MS-WIN" &THEN
   {adedict/TBL/tblprop.f  
      &frame_phrase = "frame tblprops NO-BOX 
       default-button s_btn_OK cancel-button s_btn_Close"
      &apply_btn  = s_btn_Save
      &ds_btn     = "SPACE({&HM_BTN}) s_btn_Tbl_ds"
      &other_btns = "SPACE({&HM_DBTN}) s_btn_Close SPACE({&HM_DBTNG}) 
     s_btn_Prev SPACE({&HM_DBTN}) s_btn_Next"
      &col1       = 18
      &col2       = 20
      &colbtn     = 4.5
   }
&ELSE /* motif */
   {adedict/TBL/tblprop.f  
      &frame_phrase = "frame tblprops NO-BOX 
       default-button s_btn_OK cancel-button s_btn_Close"
      &apply_btn  = s_btn_Save
      &ds_btn     = "SPACE({&HM_BTN}) s_btn_Tbl_ds"
      &other_btns = "SPACE({&HM_DBTN}) s_btn_Close SPACE({&HM_DBTNG}) 
     s_btn_Prev SPACE({&HM_DBTN}) s_btn_Next"
      &col1       = 20
      &col2       = 22
      &colbtn     = 2
   }
   /* &colbtn     = 8 */ /**/ /* &ds_btn ... */
&ENDIF

{adedict/TBL/tblprop.f  
   &frame_phrase = "frame newtbl view-as DIALOG-BOX TITLE ""Create Table"" 
                default-button s_btn_OK cancel-button s_btn_Done"
   &apply_btn  = s_btn_Add
   &ds_btn     = "   "
   &other_btns = "SPACE({&HM_DBTN}) s_btn_Done"
   &col1       = 18
   &col2       = 20
   &colbtn     = 4
}

/*--- Save policy has been changed --------------------------------
   this is obsolete but leave in case we change our minds!

/* Flag which when true indicates that trigger records have been saved
   in the work file for the current table. */
Define {1} var s_TblTrigs_Stashed as logical NO-UNDO.
------------------------------------------------------------------*/

/* Symbolic constants */

/* for file number (_File-Number) values. */
&global-define   TBLNUM_FASTTRK_START   -29
&global-define   TBLNUM_FASTTRK_END     -7

define variable odbtyp   as character no-undo.

assign
  odbtyp = {adecomm/ds_type.i
              &direction = "odbc"
              &from-type = "odbtyp"
           }.
/*----------------------------------------------------------------*/
