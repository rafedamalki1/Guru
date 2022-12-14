/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corp{&db-type}tion. */
/*************************************************************/
/*
File:    prodict/gate/_gat_md9.i

Description:
    loads in all data in all (apropriate) tables

Text-Parameters:
    &db-type        "ora", "odb" or "syb"
    &tmp-name       "osh", "odb" or "ssh"
    
Included in:
  prodict/odb/_odb_md9.p
  prodict/ora/_ora_md9.p
  prodict/syb/_syb_md9.p
  
History:
    94/08/09    hutegger    inserted sys.p-support and
                            extented where-clause for each _File
                            plus contracted ora- odb- & syb_md9.p to use
                            this include  
    94/02/11    hutegger    changed input-parameter of runload.i to "y"
*/

{ prodict/dictvar.i NEW }
{ prodict/{&db-type}/{&db-type}var.i }

DEFINE NEW SHARED STREAM   loaderr.
DEFINE NEW SHARED VARIABLE errs   AS INTEGER INITIAL 0 NO-UNDO.
DEFINE NEW SHARED VARIABLE recs   AS INTEGER INITIAL 0. /*UNDO*/
DEFINE NEW SHARED VARIABLE xpos   AS INTEGER INITIAL ? NO-UNDO.
DEFINE NEW SHARED VARIABLE ypos   AS INTEGER INITIAL ? NO-UNDO.
DEFINE            VARIABLE noload AS CHARACTER NO-UNDO.


IF NOT SESSION:BATCH-MODE THEN
    OUTPUT TO VALUE ({&tmp-name}_dbname + "out.tmp") NO-MAP.

CREATE ALIAS "DICTDB2" FOR DATABASE VALUE({&db-type}_dbname).
FIND DICTDB._Db WHERE DICTDB._Db._Db-name = {&db-type}_dbname.

assign noload = "u".
{ prodict/dictgate.i
    &action = "undumpload"
    &dbtype = "_Db._Db-type"
    &dbrec  = "RECID(_Db)"
    &output = "noload"
    }
    
FOR EACH DICTDB._File OF DICTDB._Db
  WHERE NOT DICTDB._File._Hidden
  AND   _file._File-number > 0
  AND   (noload = "" OR NOT CAN-DO(noload,_File._For-type))
  BY DICTDB._File._File-name:

  OUTPUT STREAM loaderr TO VALUE(_Dump-name + ".e") NO-ECHO.
  INPUT FROM VALUE(_Dump-name + ".d") NO-ECHO NO-MAP.
  RUN prodict/misc/_runload.i (INPUT "y") _File-name 0 100 _File-name 0.
  INPUT  CLOSE.
  OUTPUT STREAM loaderr CLOSE.  


END.

IF NOT SESSION:BATCH-MODE THEN
    OUTPUT CLOSE.

RETURN.
