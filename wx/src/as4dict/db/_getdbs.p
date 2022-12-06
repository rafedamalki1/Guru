/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _getdbs.p

Description:   
   This procedure gets the list of databases as follows:

   This includes:
      all connected databases and foreign databases whose schema holders
      are connected.

Shared Output:
   s_CurrDb    	  - will have been set.

Author: Laura Stern

Date Created: 01/28/92 

----------------------------------------------------------------------------*/

{as4dict/dictvar.i shared}
{as4dict/brwvar.i shared}


/*----------------------------Mainline code---------------------------------*/

Define var dbcnt   as integer NO-UNDO.
Define var lname   as char    NO-UNDO.
Define var version as char    NO-UNDO.
Define var savedb  as char    NO-UNDO.


/* Save the name of the current DICTDB database - this is the
   preferred current database.
*/
savedb = LDBNAME("DICTDB").

/* In Progress NUM-DBS is the # of connected databases and DBTYPE gives the
   type for the nth connected database. 
*/

conn_db:
do dbcnt = 1 TO NUM-DBS:
   lname = LDBNAME(dbcnt).

   /* If already in the list, skip it. */
   if s_lst_Dbs:LOOKUP(lname) in frame browse <> 0 then
      next conn_db.

   /* We can't deal with v5 or v6 databases so don't bother putting them
      in the list. */
   version = DBVERSION(dbcnt).
   if version = "5" OR version = "6" then
   do:
      if NOT CAN-DO (s_OldDbs, lname) then
      do:
	 version = "V" + version.
	 message "Database" lname "is a" version "database." SKIP
		 "The V7 dictionary cannot be used with a PROGRESS V5" SKIP
		 "or V6 database.  Use the dictionary under PROGRESS" SKIP
      	       	 "V5 or V6 to access this database." SKIP(1)
		 "(Note: Database" lname "is still connected.)"
		 view-as ALERT-BOX INFORMATION buttons OK.

      	 /* Keep track of these old connected databases so we don't keep
      	    repeating this message to the user every time they connect
      	    to a new database.
      	 */
      	 s_OldDbs = s_OldDbs + (if s_OldDbs = "" then "" else ",") + 
      	       	    lname.
      end.
      next conn_db.
   end.

   /* If the database is not Progress, the schema holder must be connected
      in order for us to work with it - so skip it, and if the schema holder
      is indeed connected, we will find the foreign db name later from the 
      _Db record of the schema holder. */
   if DBTYPE(dbcnt) <> "PROGRESS" then
      next conn_db.

   /* Now pick up foreign databases for which this db is a schema holder.
      Set DICTDB as alias for this database so we can browse the _Db recs.
   */   
   create alias "DICTDB" for database VALUE (lname) NO-ERROR.        
   
 
   /* This has to be in a separate .p - instead of an internal procedure
      because it does database access, and when this "getdbs" runs we may not
      have any connected databases.  This bends Progress all out of shape.
   */                

   run as4dict/db/_getrdbs.p.
end.

/* Restore alias DICTDB to what it was when we came in here */
if savedb <> ? then
   create alias "DICTDB" for database VALUE(savedb) NO-ERROR.    
   

