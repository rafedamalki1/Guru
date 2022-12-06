/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/
/* $Id: qurywidg.i,v 1.3 95/08/24 15:08:38 ross Exp $ */
/* $Log:	qurywidg.i,v $
Revision 1.3  95/08/24  15:08:38  ross
Mod #37062 by ross
Fixed bugs:
95-08-17-027
Now the sorts fields go back and forth smoothly.

Revision 1.2  95/04/12  17:15:02  ross
Mod #32810 by ross
Query tuning support.

Revision 1.1  94/02/22  09:11:35  lee
Initial revision

Revision 1.2  94/02/06  14:56:15  lee
Mod #22196 by lee
Fixed bugs:
94-01-31-026
Admin changes

Revision 1.1  93/11/05  14:46:35  lee
Initial revision

Revision 1.7  93/07/13  11:02:12  oconnor
No checkin message was available for this revision

Revision 1.1  93/07/13  09:01:00  oconnor
Initial revision

Revision 1.5  93/04/28  13:54:53  oconnor
Mod #15269 by oconnor
Fixed bugs:
New Query builder.

Revision 1.4  93/04/01  09:09:41  ravi
Mod #14331 by ravi
Fixed bugs:
Moved to 7.1C Syntax.

 */

/*----------------------------------------------------------------------------

File: qurywidg.i

Description:

Input Parameters:
   <None>

Output Parameters:
   <None>

Author: Greg O'Connor

Date Created: 1992 

----------------------------------------------------------------------------*/

/* Max tables in query definition */

&Glob MaxTbl 20 
/* &Global-Define MaxTbl       20 */
/* Make sure the seperator char is not a '!' which will conflict for lable stuff*/
&Global-Define Sep1          CHR (3)
&Global-Define Sep2          CHR (4)


/* Browser Props */

DEFINE {1} SHARED VARIABLE _4GLQury      AS CHAR                       NO-UNDO.
           /* 4GL code defining query              */
           
DEFINE {1} SHARED VARIABLE _UIB_4GLQury  AS CHAR                       NO-UNDO.
           /* 4GL code defining query              */
DEFINE {1} SHARED VARIABLE _TblList      AS CHAR                       NO-UNDO.
           /* List of tables in query              */
           
DEFINE {1} SHARED VARIABLE _FldList      AS CHAR                       NO-UNDO.
           /* List of fields selected for browse   */
           
DEFINE {1} SHARED VARIABLE _OrdList      AS CHAR                       NO-UNDO.
          /* List of fields in BREAK BY phrase     */
          
DEFINE {1} SHARED VARIABLE _JoinTo       AS INTEGER  EXTENT {&MaxTbl}  NO-UNDO.
          /* Parent Table                          */
         
DEFINE {1} SHARED VARIABLE _JoinCode     AS CHAR     EXTENT {&MaxTbl}  NO-UNDO.
         /* 4GL Join Code                          */
         
DEFINE {1} SHARED VARIABLE _Where        AS CHAR     EXTENT {&MaxTbl}  NO-UNDO.
         /* 4GL Where Code                         */
