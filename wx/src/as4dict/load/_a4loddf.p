/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _A4loddf - Controlling jacket program for loading .df files  */
/*            from an AS/400 database.  This mirrors the load   */
/*            process (in usermenu.i) which runs from the       */
/*            admin menu in the Data Administration function.   */
/*  Initial creation:  May 8, 1995 - NHorn                      */   
/*          Modified:  12/18/95 - D McMann to run new load procedures
                                  if DB2/400 Database is empty 
                       09/09/96 - D. McMann changed assignment to user_dbname
                                  to use _Db._Db-addr since that is where the
                                  DB2/400 Dictionary Library is stored.
                       10-18096 - D. McMann changed assign to user_dbname
                                  to fix bug 96-10-17-004
                       03/21/97 - D. McMann added assign of user_env[34] = ""
                                  97-01-20-020                                   

*/                                                                       


{as4dict/dictvar.i shared}
{as4dict/menu.i shared}
{as4dict/dump/dumpvar.i shared}

DEFINE VARIABLE save_db AS CHARACTER NO-UNDO.     
DEFINE VARIABLE schema_empty AS LOGICAL NO-UNDO.

/*  Find the correct database selected.  Preserve the one in user_dbname */
  ASSIGN 
   save_db = user_dbname  
   user_dbname = PDBNAME("as4dict").
   

/* Check if the dictionary is dirty.  If so, force commit */
IF s_DictDirty THEN DO:
    MESSAGE "You have uncommitted transactions which must"
            "be committed before starting the load utility."
       	     VIEW-AS ALERT-BOX buttons OK.
    RETURN.
END.  /* If Dictdirty */

user_env[9] = "d". 

RUN "as4dict/load/_usrload.p".

IF NOT user_cancel THEN 
   DO:     
        FIND FIRST as4dict.p__File NO-LOCK NO-ERROR.
        IF NOT AVAILABLE as4dict.p__File THEN 
            ASSIGN schema_empty = true.     
       
      { as4dict/setdirty.i &dirty = "true" }  
      
      IF schema_empty THEN
            RUN "as4dict/load/_lodnddl.p".
      ELSE      
            RUN "as4dict/load/_lodsddl.p".           
       
      IF user_env[35] <> "s"  THEN DO:      
          user_env[9] = "h". 
          RUN "as4dict/load/_usrload.p".   
       END.        
       ELSE assign user_env[35] = "".
   END.

/* reset user cancel so user can come back in within same process */       
/* reset user_dbname back to selected db. */
ASSIGN 
   user_env[34] = ""
   user_cancel = no
   user_dbname = save_db.

