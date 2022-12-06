/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _dtcust.p

Description:
   Do data type customization.  This means changing visibility or labels
   of fields based on their relevance to the currently selected data type.
   For the fields affected, also take gateway capabilities and other 
   factors into account.

Shared Input:
   s_Fld_Typecode - The underlying code for data type
   s_Fld_Gatetype - The gateway data type.

Input Parameters:
   p_Case - Widget handle for the case sensitive widget.
   p_Dec  - Widget handle for the decimals widget.

Author: Laura Stern

Date Created: 10/02/92 

----------------------------------------------------------------------------*/

{adecomm/adestds.i} /* FIX - REMOVE */

{as4dict/dictvar.i shared}
{adecomm/cbvar.i shared}
{as4dict/uivar.i shared}
{as4dict/FLD/fldvar.i shared}
{as4dict/capab.i}

Define INPUT PARAMETER p_Dec  as widget-handle NO-UNDO.

assign
   p_Dec:sensitive =   NO 
   p_Dec:label = (if s_Fld_Gatetype = "Bits" then "Bit offset" 
      	       	     	      	       	     else "Decima&ls").






