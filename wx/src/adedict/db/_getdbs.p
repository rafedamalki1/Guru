/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*-----------------------------------------------------------------------

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

-----------------------------------------------------------------------*/

{ adedict/dictvar.i shared }
{ adedict/brwvar.i  shared }

{ adecomm/getdbs.i
  &new = "NEW"
  }

Define var l_i     as int     NO-UNDO.
Define var l_strng as char    NO-UNDO.

/*------------------------  INT.-PROCEDURES  -----------------------*/

/*---------------------------  MAIN-CODE  --------------------------*/

/* initialize cache */
assign
  s_DbCache_Cnt        = 0
  s_lst_dbs:list-items = "".

/* get list of dbs */
if NUM-DBS <> 0 
 then do:
  RUN adecomm/_dictdb.p.
  RUN adecomm/_getdbs.p.
  end.
  
/* create cache and selection-list */
for each s_ttb_db:
  
  /* get rid of older versions, because we can't handle them and
   * Keep track of these old connected databases so we don't keep
   * repeating this message to the user every time they connect
   * to a new database.
   */
  if s_ttb_db.vrsn < "8"
   then do:
    if NOT CAN-DO (s_OldDbs, s_ttb_db.ldbnm)
     then do:
      assign
        s_OldDbs = s_OldDbs
                 + (if s_OldDbs = "" then "" else ",")
                 + s_ttb_db.ldbnm.
        l_strng  = "V" + s_ttb_db.vrsn.
      message 
        "Database" s_ttb_db.ldbnm "is a" l_strng "database." SKIP
        "This dictionary cannot be used with a PROGRESS V5, V6" SKIP
        "or V7 database.  Use the dictionary under PROGRESS" SKIP
        "V5, V6 or V7 to access this database." SKIP(1)
        "(Note: Database" s_ttb_db.ldbnm "is still connected.)"
         view-as ALERT-BOX INFORMATION buttons OK.
      end.
    next.
    end.

  /* Skip auto-connect records
   */
  if   s_ttb_db.local = FALSE
   and s_ttb_db.dbtyp = "PROGRESS"
   then next.
   
  /* check for number of dbs to be maller than extent */
  if EXTENT(s_DbCache_Pname) <= s_DbCache_Cnt
   then next.

  assign
    /* Add the name to the select list in the browse window. */
    s_Res = ( if s_ttb_db.local = TRUE
                then s_lst_Dbs:add-last(s_ttb_db.ldbnm) in frame browse
                else s_lst_Dbs:add-last( " " + s_ttb_db.ldbnm
                                       + "(" + s_ttb_db.sdbnm + ")"
                                       ) in frame browse
            )
    /* generate internal db-type */
    l_strng = { adecomm/ds_type.i
                 &direction = "etoi"
                 &from-type = "s_ttb_db.dbtyp"
              }

    /* Add database to the cache. */
    { adedict/DB/cachedb.i
       &Lname  = s_ttb_db.ldbnm
       &Pname  = s_ttb_db.pdbnm
       &Holder = s_ttb_db.sdbnm
       &Type   = l_strng
       }

  end.  /* for each s_ttb_db */


/*------------------------- old version --------------------------------

/* Save the name of the current DICTDB database - this is the
   preferred current database.
*/
savedb = LDBNAME("DICTDB").

/* In Progress NUM-DBS is the # of connected databases and DBTYPE gives
   the type for the nth connected database. 
*/

conn_db:
do dbcnt = 1 TO NUM-DBS:
   
   /* num-dbs sometimes returns 1 instead of 0. If pdbname and ldbname
    * are ? then we just skip this dbcnt. (hutegger 95/01)
    */
   if   ldbname(dbcnt) = ?
    and pdbname(dbcnt) = ?
    then next conn_db.
    
   assign
     lname = LDBNAME(dbcnt).

   /* If already in the list, skip it. First however, make sure physical
      name is set in the cache.  We may have just connected the real
      foreign database - i.e., the name would have been in the list so we
      already could look at the schema, but now the foreign db itself is
      connected and we now know the physical name, so fill it in.
      (los 12/27/94)
   */
   ix = s_lst_Dbs:LOOKUP(lname) in frame browse.
   if ix <> 0 then do:
      if s_DbCache_Pname[ix] = ? then
        s_DbCache_Pname[ix] = PDBNAME(lname).
      next conn_db.
   end.

   /* We can't deal with v5, v6 or v7 databases so don't bother putting them
      in the list. */
   version = DBVERSION(dbcnt).
   if version = "5" OR version = "6" OR version = "7" then
   do:
      if NOT CAN-DO (s_OldDbs, lname) then
      do:
	 version = "V" + version.
	 message 
	   "Database" lname "is a" version "database." SKIP
	   "The V8 dictionary cannot be used with a PROGRESS V5," SKIP
	   "V6 or V7 database.  Use the dictionary under" SKIP
     	   "PROGRESS V5, V6  or V7 to access this database." SKIP(1)
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
      in order for us to work with it - so skip it, and if the schema
      holder is indeed connected, we will find the foreign db name later
      from the  _Db record of the schema holder. */
   if DBTYPE(dbcnt) <> "PROGRESS" then
      next conn_db.

   /* Add the name to the select list in the browse window. */
   s_Res = s_lst_Dbs:add-last(lname) in frame browse.

   /* Add database to the cache. */
   {adedict/DB/cachedb.i &Lname  = lname
      	       	     	 &Pname  = PDBNAME(lname)
      	       	     	 &Holder = lname
      	       	     	 &Type   = DBTYPE(lname)}

   /* Now pick up foreign databases for which this db is a schema holder.
      Set DICTDB as alias for this database so we can browse the _Db recs.
   */   
   create alias "DICTDB" for database VALUE (lname) NO-ERROR.      

   /* This has to be in a separate .p - instead of an internal procedure
      because it does database access, and when this "getdbs" runs we may
      not have any connected databases.  This bends Progress all out of
      shape.
   */
   run adedict/DB/_getrdbs.p.
end.

/* Restore alias DICTDB to what it was when we came in here */
if savedb <> ? then
   create alias "DICTDB" for database VALUE(savedb) NO-ERROR.

-----------------------------------------------------------------------*/
