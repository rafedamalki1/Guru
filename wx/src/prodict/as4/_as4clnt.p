/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* __as4clnt.p -  Error message for non MS-Windows clients that selected
                               an option from the Data Admin tool that is not available to them.
                               
   Created 03/20/95 D. McMann
 */
 
 MESSAGE " The option which you have selected is " SKIP
                       " only available to MS-Windows Clients. " SKIP
                       VIEW-AS ALERT-BOX  ERROR BUTTON OK.