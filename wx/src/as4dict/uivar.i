/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: uivar.i

Description:
   Include file defining some common user interface components that are used
   in many places throughout the dictionary.

Author: Laura Stern

Date Created: 02/19/92 
----------------------------------------------------------------------------*/

/* (FIX - uncomment)
&IF DEFINED(ADESTDSI) = 0 &THEN 
{adecomm/adestds.i}  /* Layout Global defines, colors etc. */
&ENDIF */
 
Define button s_btn_OK     label "OK"     {&STDPH_OKBTN} AUTO-GO.
Define button s_btn_Cancel label "Cancel" {&STDPH_OKBTN} AUTO-ENDKEY.

/* The label on the Done button will change to "Close" after a
   change occurs that will not be undone (e.g., having added a table).
   These are used in the add dialogs.
*/
Define button s_btn_Add    label "Create"  {&STDPH_OKBTN} AUTO-GO.
Define button s_btn_Done   label "Cancel"  {&STDPH_OKBTN} AUTO-ENDKEY.

/* The label on the Close button will change to "Close" after any 
   change occurs that will not be undone (e.g., a change in a sub-dialog).
   These are used for the property windows.
*/
Define button s_btn_Save   label "&Save"  {&STDPH_OKBTN}.
Define button s_btn_Close  label "Cancel" {&STDPH_OKBTN} AUTO-ENDKEY.

Define button s_btn_Next   label "&>" SIZE 4 BY {&H_OKBTN} margin-extra default.
Define button s_btn_Prev   label "&<" SIZE 4 BY {&H_OKBTN} margin-extra default.

Define button s_btn_Help   label "&Help"   {&STDPH_OKBTN}.

Define rectangle s_rect_Btns {&STDPH_OKBOX}.

/* "Blue bar" optional text */
define var s_Optional as char initial " Optional" {&STDPH_SDIV}
                              format "x(9)" view-as text.

/* Status line variable */
Define {1} var s_Status as char NO-UNDO.

/* The beginning of any "no permissions" messages. */
Define {1} var s_NoPrivMsg as char NO-UNDO
   init "You do not have permission to".

Define {1} var s_OK_Hit as logical NO-UNDO. /* to control OK processing */
Define {1} var s_Valid  as logical NO-UNDO. /* to control validation 
      	       	     	      	       	       for apply buttons */
Define {1} var s_Widget as widget-handle NO-UNDO. /* general widget handle */
