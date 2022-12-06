/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _dfltfmt.p

Description:
   Set the format to it's default value based on the data type, conditional
   upon the value of p_Default.  In any case, fix the initial value if
   the data type is logical, to be consistent with the current or new default
   format.

Shared Input (The following shared variables must have been set):
   if database is Progress:
      s_Fld_Typecode  - data type code corresponding to currently selected data
      	       	        type.
   else
      s_Fld_Gatetype  - The gateway data type string (short form)
      s_Fld_Protype   - Progress data type that this gateway type maps to.
      s_Gate_Typ_Proc - The procedure to call to get the format/type info for
      	       	        the current gateway type.
   
Input Parameters:
   p_Format  - The format widget handle.
   p_Initial - The initial value widget handle.
   p_Default - True, if caller wants format set to the default unconditionally,
      	       False if we should only set default if the current format value
      	       is the null string. 

Author: Laura Stern

Date Created: 08/05/92 

----------------------------------------------------------------------------*/

{as4dict/dictvar.i shared}
{adecomm/cbvar.i shared}
{as4dict/uivar.i shared}
{as4dict/FLD/fldvar.i shared}

Define INPUT PARAMETER p_Format  as widget-handle NO-UNDO.
Define INPUT PARAMETER p_Initial as widget-handle NO-UNDO.
Define INPUT PARAMETER p_Default as logical       NO-UNDO.

Define var io1 as integer  NO-UNDO.  
Define var io2 as integer  NO-UNDO.
Define var fmt as char     NO-UNDO.

fmt = p_Format:screen-value.
if p_Default OR (fmt = "" OR fmt = "?") then
do:
   /*Reset format to its default for this data type */
   assign fmt = ?.

   if NOT {as4dict/ispro.i} then
      run VALUE(s_Gate_Typ_Proc) 
      	 (INPUT-OUTPUT io1, 
	  INPUT-OUTPUT io2,
	  INPUT-OUTPUT s_Fld_Protype, 
	  INPUT-OUTPUT s_Fld_Gatetype,
	  OUTPUT fmt).
	  
   if fmt = ? then
      case s_Fld_Typecode:
	 when {&DTYPE_CHARACTER} then
	    fmt = "X(8)".
	 when {&DTYPE_DATE} then
	    fmt = "99/99/99".
	 when {&DTYPE_LOGICAL} then
	    fmt = "yes/no".
	 when {&DTYPE_INTEGER} then
	    fmt = "->,>>>,>>9".
	 when {&DTYPE_DECIMAL} then
	    fmt = "->>,>>9.99".
	 when {&DTYPE_RECID}                  then
	    fmt = ">>>>>>9".
      end.
   else case fmt:
	 when "c"  then
	    fmt = "X(8)".
	 when "d"  then
	    fmt = "99/99/99".
	 when "l"  then
	    fmt = "yes/no".
	 when "i"  then
	    fmt = "->,>>>,>>9".
	 when "#"  then
	    fmt = "->>,>>9.99".
      end.

   p_Format:screen-value = fmt.
end.

if s_Fld_Typecode = {&DTYPE_LOGICAL} then
   /* Reset the initial value to the first string in the format, e.g.,
      set to "male" if format is male/female.  However, special case
      yes/no and true/false to initialize to no and false respectively
      since that is what Progress does.
   */
   if fmt = "yes/no" then
      p_Initial:screen-value = "no".
   else if fmt = "true/false" then
      p_Initial:screen-value = "false".
   else
      p_Initial:screen-value = SUBSTR(fmt, 1, INDEX(fmt, "/") - 1).








