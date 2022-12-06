/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* dictexen.p - dictionary threaded language interpreter - no dbs connected */
/*
File:
    prodict/_dctexec.p
    
descr:
    runs all routines from user_path, locks MetaSchema (search for *T),
    We return from this routine to its caller (usermenu.i: procedure
    perform_Func) either
    a) if the user_path   = ""      (no more programs to run)
    b) entry(1,user_path) = "*E"    (leave _dictc.p)
    c) entry(1,user_path) = "*T"    (start transaction)
    d) entry(1,user_path) = "*C"    (commit transaction)

history:
    hutegger    08/96   changed *O behavior. executable can have different
                        names per OPSYS. So I use user_env[1-6] to hold
                        the names of the programs per opsys. user_env[7]
                        holds the default value, which gets used when
                        one of user_env[1-6] = ""
                        ALSO ported the change of tomn from dctexec.p
    tomn        07/96   Changed the "proc name massage" logic to correctly
                        produce the correct relative path name for the
                        "protoxxx" utilities.  Since they do not follow the
                        original naming convention, the logic had to be changed
                        so as to not attempt to insert a subdirectory spec.
*/
/*h-*/
/*---------------------------------------------------------------------*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }
{ prodict/user/userhdr.f }

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 3 NO-UNDO INITIAL [
  /*1,2*/ "Operations on", "are not supported in this copy of PROGRESS.",
  /*  3*/ "The dictionary is in read-only mode - alterations not allowed."
].
/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

DEFINE VARIABLE i  AS INTEGER   NO-UNDO.
DEFINE VARIABLE op AS CHARACTER NO-UNDO.
DEFINE VARIABLE p  AS CHARACTER NO-UNDO.
DEFINE VARIABLE z  AS CHARACTER NO-UNDO.

ASSIGN
  user_filename = ""
  user_dbname   = ""
  user_dbtype   = "".
DISPLAY user_dbname user_filename WITH FRAME user_ftr.

DO WHILE user_path <> "":
  ASSIGN
    z         = ENTRY(1,user_path)
    i         = INDEX(z,"=")
    op        = (IF i > 0 THEN "*A" ELSE z)
    user_path = TRIM(SUBSTRING(user_path,LENGTH(z) + 2)).
  IF op BEGINS "?" OR op BEGINS "!" THEN _dbim: DO:
    IF CAN-DO(GATEWAYS,SUBSTRING(op,2)) THEN LEAVE _dbim.
    HIDE MESSAGE NO-PAUSE.
    MESSAGE new_lang[1] SUBSTRING(op,2) new_lang[2] 
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    user_path = "".
  END.
  ELSE
  IF op = "*A" THEN /*-------------------------------------*/ /* assignment */
    user_env[INTEGER(SUBSTRING(z,1,i - 1))] = SUBSTRING(z,i + 1).
  ELSE
  IF op = "*C" THEN RETURN. /*---------------------*/ /* commit transaction */
  ELSE
  IF op = "*E" THEN DO: /*---------------------------------------*/ /* exit */
    user_path = op.
    LEAVE.
  END.
  ELSE
  IF op BEGINS "*L" THEN /*-------------------------*/ /* run local program */
    RUN VALUE(SUBSTRING(z,4)).
  ELSE
  IF op = "*N" THEN . /*-----------------------------------------*/ /* noop */
  ELSE
  IF op = "*O" THEN DO: /*--------------------------*/ /* run opsys program */
    PAUSE 0.
      case OPSYS:
        when "BTOS"  then assign p = ( if user_env[1] = ""
                                        then user_env[7]
                                        else user_env[1]
                                     ).
        when "MSDOS" then assign p = ( if user_env[2] = ""
                                        then user_env[7]
                                        else user_env[2]
                                     ).
        when "OS2"   then assign p = ( if user_env[3] = ""
                                        then user_env[7]
                                        else user_env[3]
                                     ).
        when "UNIX"  then assign p = ( if user_env[4] = ""
                                        then user_env[7]
                                        else user_env[4]
                                     ).
        when "VMS"   then assign p = ( if user_env[5] = ""
                                        then user_env[7]
                                        else user_env[5]
                                     ).
        when "WIN32" then assign p = ( if user_env[6] = ""
                                        then user_env[7]
                                        else user_env[6]
                                     ).
       end case.
      os-command value(p).
  END.
  ELSE
  IF op = "*Q" THEN QUIT. /*-------------------------------------*/ /* quit */
  ELSE DO:
    IF      z BEGINS "_dct" THEN .
    ELSE IF z BEGINS "_usr" THEN z = "user/" + z.
    ELSE IF z BEGINS "_gui"  THEN z = "gui/"  + z.
    ELSE IF z BEGINS "_dmp"
         OR z BEGINS "_lod" THEN z = "dump/" + z.
    ELSE IF z BEGINS "_gat" THEN z = "gate/" + z.
    ELSE IF z BEGINS "_as4"  THEN z = "as4/"  + z.  /* AS400 */
    ELSE IF z BEGINS "_bti"  THEN z = "bti/"  + z.  /* CTOS-ISAM */
    ELSE IF z BEGINS "_gen"  THEN z = "gen/"  + z.  /* Generic */
    ELSE IF z BEGINS "_ism"  THEN z = "ism/"  + z.  /* C-ISAM and NetISAM */
    ELSE IF z BEGINS "_oag"  THEN z = "oag/"  + z.  /* Object Access */
    ELSE IF z BEGINS "_ora"  THEN z = "ora/"  + z.  /* Oracle */
    ELSE IF z BEGINS "_pro"  THEN z = "pro/"  + z.  /* PROGRESS */
    ELSE IF z BEGINS "_rdb"  THEN z = "rdb/"  + z.  /* Rdb */
    ELSE IF z BEGINS "_rms"  THEN z = "rms/"  + z.  /* RMS */
    ELSE IF z BEGINS "_syb"  THEN z = "syb/"  + z.  /* SYBASE */
    ELSE IF z BEGINS "_odb"  THEN z = "odb/"  + z.  /* ODBC */
    /*--------------------------------------------------------------------*
       This had to be changed to allow the "protoxxx" utilities to be 
       called from the DataServer menu (tsn 7/96).  Before, it was just
       "else z = 'misc/' + z", which would tack an unwanted "misc/" on
       the beginning of, for example, "ora/protoora".
     *-----------------------------------------------------------*/
    else if z BEGINS "_"     then z = "misc/" + z.
    RUN VALUE("prodict/" + z + ".p").
  END.
  user_trans = "". /* not supported when no dbs connected */
END.

RETURN.
