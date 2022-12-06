/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: dbprop.f

Description:   
   This file contains the form for displaying some database properties.
   This information will be read-only.

Author: Laura Stern

Date Created: 03/05/92 
    Modified: 09/29/97 Added logical database name for as4dict
----------------------------------------------------------------------------*/

form
   SKIP({&TFM_WID})

   s_Db_Lname 	 LABEL "Logical Name" 	 colon 16 
                 FORMAT "x(32)" view-as TEXT       SKIP
   s_Db_Pname 	 LABEL "Physical Name"	 colon 16  
                 FORMAT "x(50)" view-as TEXT       SKIP
   s_Db_Holder	 LABEL "Schema Holder"	 colon 16  
                 FORMAT "x(32)" view-as TEXT       SKIP
   s_Db_Type 	 LABEL "Type"	      	 colon 16
                 FORMAT "x(12)" view-as TEXT       

   {adecomm/okform.i
      &BOX    = s_rect_btns
      &STATUS = no
      &OK     = s_btn_OK
      &HELP   = s_btn_Help}

   with frame dbprops SIDE-LABELS NO-BOX DEFAULT-BUTTON s_btn_OK.


