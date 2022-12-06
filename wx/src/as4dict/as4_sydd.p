/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* -------------------------------------------------------------------   
  
   Procedure as4_sydd.p 

  Load is finished now see what user wants to do.  If there are no user files
    ask if the dictionary should be run, else ask if sync is to be done.  If these
    are not correct choices, user will be returned to data admin and they could
    choose the function they want                    
    
    Created 05/05/95  D. McMann         
    Modified 03/25/96 D. McMann Changed QUESTION to WARNING in messages   
             07/22/96 D. McMann Changed = MS-Window to BEGINS MS-WIN  
             09/30/96 D. McMann Removed option to go into dictionary since
                                the dictionary must be run differenctly in
                                usermenu.i now to make sure the meta schema
                                cached records can be redone after a sync.
----------------------------------------------------------------------- */   
DEFINE VARIABLE answer AS LOGICAL.
DEFINE VARIABLE namenow AS CHARACTER FORMAT "x(15)" NO-UNDO.       


FIND FIRST as4dict.p__file NO-ERROR.


IF AVAILABLE as4dict.p__file THEN 
_loop:
DO:               
  ASSIGN namenow = ldbname("as4dict") + " now?".
  answer = true.
  MESSAGE "Do you want to perform a synchronization on"  namenow SKIP
      	    view-as ALERT-BOX WARNING buttons YES-NO update answer.                   
      	    
  IF answer THEN DO ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
     RUN as4dict/as4_sync.p.              
     HIDE MESSAGE NO-PAUSE.       
     IF RETURN-VALUE = "error" THEN  DO:
        MESSAGE "Synchronization terminated. " SKIP
                              "Backing out information" SKIP
            VIEW-AS ALERT-BOX INFORMATION BUTTON OK.
        UNDO, LEAVE _loop.                                                               
      END.
     
     ELSE IF RETURN-VALUE =  "insync" THEN.
     ELSE
       MESSAGE "Synchronization" SKIP
                             "      Complete     " VIEW-AS ALERT-BOX INFORMATION BUTTON OK.       
  END.      
END.
RETURN.

