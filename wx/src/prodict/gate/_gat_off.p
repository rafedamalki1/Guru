/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _gat_off.p - Shows RMS,CISAM,NetISAM,CTOSISAM record layout & fld overlaps */

{ prodict/user/uservar.i }
{ adecomm/commeng.i }

/* Use report code to see the info in an editor widget. _gat_of2.p
   does the real work of displaying the info. 
*/
RUN adecomm/_report.p 
   (INPUT ?,
    INPUT "Record Layout for Table: " + user_filename + 
      	  ", Database: " + user_dbname,
    INPUT "Record Layout for Table: " + user_filename,
    INPUT "",
    INPUT "",
    INPUT "prodict/gate/_gat_of2.p",
    INPUT "",
    INPUT {&Record_Layout_Dlg_Box} 
    ).

RETURN.

