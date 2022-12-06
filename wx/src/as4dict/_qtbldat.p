/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _qtbldat.p

Description:
   Display _File information for the quick table report.  It will go to 
   the currently set output device (e.g., a file, the printer).
 
Input Parameters:
   p_DbId    - Id of the _Db record for this database.

Author: Tony Lavinio, Laura Stern

Date Created: 10/02/92
     Modified 02/10/95 for AS400 Data Dictionary - Donna McMann
----------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p_DbId AS RECID NO-UNDO.

DEFINE SHARED STREAM rpt.


DEFINE VARIABLE asname AS CHARACTER NO-UNDO.   

/* These two variables are used as a kludge because the stacked labels with
     integers are not lining up properly  */
                                                    
DEFINE VARIABLE nmfld      AS CHARACTER NO-UNDO.
DEFINE VARIABLE nmkey    AS CHARACTER NO-UNDO.

FORM    
    as4dict.p__File._File-name  FORMAT "x(25)"  COLUMN-LABEL "Progress Name"    
   asname                         FORMAT "x(21)"  COLUMN-LABEL "AS/400 Name"
   as4dict.p__File._Dump-name  FORMAT "x(8)"   COLUMN-LABEL "Dump Name"
   nmfld           FORMAT "x(5)"  COLUMN-LABEL "Field!count"
   nmkey     FORMAT "x(5)"  COLUMN-LABEL "Index!Count"    
   WITH FRAME shotable USE-TEXT STREAM-IO DOWN.
          

FOR EACH as4dict.p__File WHERE as4dict.p__File._Hidden = "N":
   ASSIGN
      asname = as4dict.p__File._For-name
      nmfld = string(as4dict.p__file._numfld)
      nmkey =     string(as4dict.p__File._numkey).
    
   DISPLAY STREAM rpt
      as4dict.p__File._File-name
      asname
      nmfld 
      nmkey
      as4dict.p__File._Dump-name
      WITH FRAME shotable.
   DOWN STREAM rpt WITH FRAME shotable.
END.







