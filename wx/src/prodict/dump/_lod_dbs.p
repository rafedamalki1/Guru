/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/
/*
File:
    prodict/dump/_lod_dbs.p
    
descr:
    transfers dbs-info from tmp-table to real db

history:
    hutegger    95/06/16    DataServer need DICTDB._Db-xl-name to be <> ?. If
                            .df doesn't contain codepage-name entry, I
                            set it to session:charset and also create a 
                            s_ttb_fake-cp record for this DICTDB._Db.  At the
                            end of the load-transaction in _lodsddl.p, 
                            I assign ? to DICTDB._Db-xl-name for all these 
                            DICTDB._Db-records
    hutegger    94/05/04    special cases for old codepage-names:
                                ISO-Latin-1   =now=>  ISO8859-1
                                ISO 8859-1    =now=>  ISO8859-1
                                Codepage 850  =now=>  IBM850
    hutegger    94/05/06    updated rule for "codepage allowed" when
                            modifying DICTDB._Db
   
*/
/*h-*/

/*---------------------------------------------------------------------*/

{ prodict/dump/loaddefs.i }
{ prodict/dictvar.i }
{ prodict/user/uservar.i }

define shared temp-table s_ttb_fake-cp
    field   db-name     as character
    field   db-recid    as recid.
    
DEFINE VARIABLE scrap AS CHARACTER NO-UNDO.

/*---------------------------------------------------------------------*/

FIND FIRST wdbs.
if imod <> "a":u
 then if wdbs._Db-name = "?"/* Comparing to the string value "?" fails whereas
                               comparing against the unknown value works.
                            */
   then FIND DICTDB._Db WHERE DICTDB._Db._Db-name = ? NO-ERROR.
   else FIND DICTDB._Db WHERE DICTDB._Db._Db-name = wdbs._Db-name. /* already proven to exist */


if imod = "a":u then do: /*--------------------------------------------*/

  if CAN-FIND(DICTDB._Db WHERE DICTDB._Db._Db-name = wdbs._Db-name) then
    ierror = 7. /* "&2 already exists with this name" */
  if ierror > 0 then RETURN.
  CREATE DICTDB._Db.
  ASSIGN
    DICTDB._Db._Db-name  = wdbs._Db-name
    DICTDB._Db._Db-addr  = wdbs._Db-addr
    DICTDB._Db._Db-comm  = (if wdbs._Db-comm = ? then "" else wdbs._Db-comm)
    DICTDB._Db._Db-type  = wdbs._Db-type
    DICTDB._Db._Db-slave = (wdbs._Db-type <> "PROGRESS":u)
    DICTDB._Db._Db-misc1[1] = wdbs._Db-misc1[1]
    DICTDB._Db._Db-misc1[2] = wdbs._Db-misc1[2]
    DICTDB._Db._Db-misc1[3] = wdbs._Db-misc1[3]
    DICTDB._Db._Db-misc1[4] = wdbs._Db-misc1[4]
    DICTDB._Db._Db-misc1[5] = wdbs._Db-misc1[5]
    DICTDB._Db._Db-misc1[6] = wdbs._Db-misc1[6]
    DICTDB._Db._Db-misc1[7] = wdbs._Db-misc1[7]
    DICTDB._Db._Db-misc1[8] = wdbs._Db-misc1[8]
    DICTDB._Db._Db-misc2[1] = wdbs._Db-misc2[1]
    DICTDB._Db._Db-misc2[2] = wdbs._Db-misc2[2]
    DICTDB._Db._Db-misc2[3] = wdbs._Db-misc2[3]
    DICTDB._Db._Db-misc2[4] = wdbs._Db-misc2[4]
    DICTDB._Db._Db-misc2[5] = wdbs._Db-misc2[5]
    DICTDB._Db._Db-misc2[6] = wdbs._Db-misc2[6]
    DICTDB._Db._Db-misc2[7] = wdbs._Db-misc2[7]
    DICTDB._Db._Db-misc2[8] = wdbs._Db-misc2[8]
    DICTDB._Db._Db-xl-name  = wdbs._Db-xl-name.
  if wdbs._Db-type <> "PROGRESS":u 
   then do:    /* switch only if not auto-connect DICTDB._Db */
    assign
      user_dbname    = wdbs._Db-name
      user_dbtype    = wdbs._Db-type
      drec_db        = RECID(DICTDB._Db).
    IF wdbs._Db-type = "ORACLE" AND wdbs._Db-misc1[3] = ? THEN DO:       
       IF OS-GETENV("ORAVERSION")   <> ? THEN
            DICTDB._Db._Db-misc1[3] = INTEGER(OS-GETENV("ORAVERSION")). 
          ELSE
            DICTDB._Db._Db-misc1[3] = 7.       
    END.  
    {prodict/dictgate.i
      &action =query
      &dbtype =user_dbtype 
       &dbrec =? 
       &output=scrap
       }
    assign scrap = "prodict/" + ENTRY(9,scrap) 
                       + "/_" + ENTRY(9,scrap) 
                       + "_sys.p".
    RUN VALUE (scrap) (drec_db,INPUT-OUTPUT imod).
    end.

  end. /*--------------------------------------------------------------*/
else if imod = "m":u then do: /*---------------------------------------*/

  if DICTDB._Db._Db-type <> wdbs._Db-type
   then ierror = 6. /* "cannot change dbtype" */
  if ierror > 0 then RETURN.
  assign wdbs._Db-comm = (if wdbs._Db-comm = ? then "" else wdbs._Db-comm).
  if DICTDB._Db._Db-addr <> wdbs._Db-addr
    then assign DICTDB._Db._Db-addr = wdbs._Db-addr.
  if DICTDB._Db._Db-comm <> wdbs._Db-comm 
   then assign DICTDB._Db._Db-comm = wdbs._Db-comm.
  if DICTDB._Db._Db-type <> "PROGRESS":u OR DICTDB._Db._Db-local
   then assign
    user_dbname = ( if DICTDB._Db._Db-type = "PROGRESS":u
                     then LDBNAME("DICTDB")
                     else wdbs._Db-name
                  )
    user_dbtype = wdbs._Db-type
    drec_db     = RECID(DICTDB._Db).

  if DICTDB._Db._Db-xl-name <> wdbs._Db-xl-name then do:
/* Jannery: codepage can be changed via .df-load, but
 *          only when db is empty! <hutegger> 94/06 
 *     assign ierror = 31. /*invalid code-page */
 *       RETURN.
 *       end.
 */
 
     /* don't allow changes to DICTDB._Db-xl-name if               */
     /* + the PROGRESS database already contains            */
     /*     + data-definition  or                           */
     /*     + _User -records   or                           */
     /*     + slave-db's (= shwmaholders)                   */
     /*     => the user should use PROUTIL                  */
     /* + the DICTDB._Db is a schemaHolder for a foreign DB        */
     /*     => the user should use the Admin-Tool           */
     if    DICTDB._Db._db-slave
      and not can-find(first s_ttb_fake-cp
                       where s_ttb_fake-cp.db-recid = RECID(DICTDB._Db))
      then do:
       assign ierror = 31. /*invalid code-page */
       RETURN.
       end.
     
     if  DICTDB._Db._db-local
      and ( can-find(first DICTDB._file 
               where DICTDB._file._db-recid = RECID(DICTDB._Db)
               and   DICTDB._File._file-number > 0)
      or    can-find(first DICTDB._User)
          )
      then do:
       assign ierror = 31. /*invalid code-page */
       RETURN.
       end.
     case wdbs._Db-xl-name:
      when "ISO-Latin-1"  then assign DICTDB._Db._Db-xl-name = "ISO8859-1".
      when "ISO 8859-1"   then assign DICTDB._Db._Db-xl-name = "ISO8859-1".
      when "codepage 850" then assign DICTDB._Db._Db-xl-name = "IBM850".
      otherwise                assign DICTDB._Db._Db-xl-name = wdbs._Db-xl-name.
      end case.
      end.     /* codepage-name of db to be changed */

          
  if DICTDB._Db._Db-coll-name <> wdbs._Db-coll-name then
     DICTDB._Db._Db-coll-name = wdbs._Db-coll-name.
     
  ASSIGN
    DICTDB._Db._Db-xlate[1]   = wdbs._Db-xlate[1]
    DICTDB._Db._Db-xlate[2]   = wdbs._Db-xlate[2]
    DICTDB._Db._Db-collate[1] = wdbs._Db-collate[1]
    DICTDB._Db._Db-collate[2] = wdbs._Db-collate[2]
    DICTDB._Db._Db-collate[3] = wdbs._Db-collate[3]
    DICTDB._Db._Db-collate[4] = wdbs._Db-collate[4]
    DICTDB._Db._Db-collate[5] = wdbs._Db-collate[5]
    DICTDB._Db._Db-misc1[1] = wdbs._Db-misc1[1]
    DICTDB._Db._Db-misc1[2] = wdbs._Db-misc1[2]
    DICTDB._Db._Db-misc1[3] = wdbs._Db-misc1[3]
    DICTDB._Db._Db-misc1[4] = wdbs._Db-misc1[4]
    DICTDB._Db._Db-misc1[5] = wdbs._Db-misc1[5]
    DICTDB._Db._Db-misc1[6] = wdbs._Db-misc1[6]
    DICTDB._Db._Db-misc1[7] = wdbs._Db-misc1[7]
    DICTDB._Db._Db-misc1[8] = wdbs._Db-misc1[8]
    DICTDB._Db._Db-misc2[1] = wdbs._Db-misc2[1]
    DICTDB._Db._Db-misc2[2] = wdbs._Db-misc2[2]
    DICTDB._Db._Db-misc2[3] = wdbs._Db-misc2[3]
    DICTDB._Db._Db-misc2[4] = wdbs._Db-misc2[4]
    DICTDB._Db._Db-misc2[5] = wdbs._Db-misc2[5]
    DICTDB._Db._Db-misc2[6] = wdbs._Db-misc2[6]
    DICTDB._Db._Db-misc2[7] = wdbs._Db-misc2[7]
    DICTDB._Db._Db-misc2[8] = wdbs._Db-misc2[8].

  end. /*--------------------------------------------------------------*/
else if imod = "r":u then do: /*---------------------------------------*/

  if CAN-FIND(FIRST DICTDB._Db WHERE DICTDB._Db._Db-name = irename) then
    ierror = 7. /* "&2 already exists with name &3" */
  if ierror > 0 then RETURN.
  DICTDB._Db._Db-name = irename.
  if DICTDB._Db._Db-type <> "PROGRESS":u
   OR DICTDB._Db._Db-local
   then drec_db = RECID(DICTDB._Db).

  end. /*--------------------------------------------------------------*/
else if imod = "d":u then do: /*---------------------------------------*/

  if DICTDB._Db._Db-type <> "PROGRESS":u then do:
    { prodict/dictgate.i
      &action = can_delete
      &dbtype = DICTDB._Db._Db-type
      &dbrec  = RECID(DICTDB._Db)
      &output = scrap
      }
    if scrap <> "ok":u then ierror = 21. /* "Cannot del database with files" */
    if ierror > 0 then RETURN.
    FOR EACH _File OF DICTDB._Db:
      { prodict/dump/loadkill.i }
      end.
    end.
  DELETE DICTDB._Db.

  end. /*--------------------------------------------------------------*/

kindexcache = "". /* and reset index delete cache on db or file change */

/* Any loading after the load of this DB record, until the next DICTDB._Db record 
   will be for this database type.  The DICTDB._Db record is always dumped for
   foreign databases.
*/
if imod <> "d":u
 then do:  /* add/modify */

  if wdbs._db-type    <> "PROGRESS"
   and DICTDB._Db._db-xl-name =  ?
   and not can-find(first s_ttb_fake-cp
                    where s_ttb_fake-cp.db-recid = RECID(DICTDB._Db))
   then do:
    create s_ttb_fake-cp.
    assign
      DICTDB._Db._db-xl-name = SESSION:CHARSET
      s_ttb_fake-cp.db-name = DICTDB._Db._db-name
      s_ttb_fake-cp.db-recid = RECID(DICTDB._Db).
    end.

  if gate_dbtype <> DICTDB._Db._db-type
   then do:
    assign
      user_dbname = ( if DICTDB._Db._Db-type = "PROGRESS":u
                     then LDBNAME("DICTDB")
                     else DICTDB._Db._Db-name
                    )
      user_dbtype = DICTDB._Db._Db-type
      gate_dbtype = DICTDB._Db._Db-type 
      drec_db     = RECID(DICTDB._Db).

    if gate_dbtype <> "PROGRESS"
     then do:
      {prodict/dictgate.i 
         &action=query 
         &dbtype=gate_dbtype 
      	 &dbrec =? 
         &output=scrap
         }
      gate_proc = "prodict/" + ENTRY(9,scrap) + "/_" + ENTRY(9,scrap) + "_typ.p".
      end.
    end.

  end.     /* add/modify */

RETURN.

/*---------------------------------------------------------------------*/
