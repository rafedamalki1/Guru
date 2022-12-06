/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _filter7.p

Description:
   This procedure is used against Progress V7 DICTDB (or later).
   The input is a RECID of a _FILE record and a filter string.  The
   output is YES or NO depending on whether the _FILE._FOR-TYPE
   field is in the filter string or not.
   (Note: _filter6.p is for DB's 6 or earlier.)

Input Parameters:
   p_recid  - Recid of _FILE record
   p_filter - Comma delimited list of types to be filtered
             
Output Parameters:
   p_in     - Set to true if _FILE._FLD-MISC2[8] is in p_filter string.

Author: Ross Hunter

Date Created: 06/15/92 

----------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER p_recid   AS RECID           NO-UNDO.
DEFINE INPUT  PARAMETER p_filter  AS CHARACTER       NO-UNDO.
DEFINE OUTPUT PARAMETER p_in      AS LOGICAL         NO-UNDO.

FIND DICTDB._FILE WHERE RECID(DICTDB._FILE) = p_recid NO-LOCK.
p_in = CAN-DO(p_filter,DICTDB._FILE._FOR-TYPE).