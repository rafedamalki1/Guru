/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/ism/_ism_udt.p

Description:

    extractc specifications of user-defined datatypes out of
    _db-misc2[8]

          
Output-Parameters: 
    p_descr         description of the datatype
    p_for-typ       foreign datatype name
    p_stnd-lngth    standard-length
    p_id-number     datatype id number
    p_pro-type      PROGRESS datatype
    p_family        datatype-family
    p_dflt-frmt     default-format
    
Used/Modified Shared Objects:
    drec_db         used to identify the right _Db-record

    
History:
    hutegger    94/09/01    creation
    
--------------------------------------------------------------------*/
/*h-*/

/*----------------------------  DEFINES  ---------------------------*/

define output parameter p_descr      as   character.
define output parameter p_for-type   as   character.
define output parameter p_stnd-lngth as   character.
define output parameter p_id-number  as   character.
define output parameter p_pro-type   as   character.
define output parameter p_family     as   character.
define output parameter p_dflt-frmt  as   character.

{ prodict/dictvar.i }

define variable   i           as integer.
define variable   l_pt        as character
     initial "c,da,de,i,l,r".
define variable   l_pro-types as character
     initial "character,date,decimal,integer,logical,recid".
define variable   l_stri      as character.


/*---------------------------  TRIGGERS  ---------------------------*/

/*------------------------  INT.-PROCEDURES  -----------------------*/

/*---------------------------  MAIN-CODE  --------------------------*/

find first _Db where RECID(_Db) = drec_db no-lock no-error.
if not available _Db or _Db-type <> "CISAM"
 then find first _Db where RECID(_Db) = s_DbRecId no-lock no-error.

if not available _Db
 then do:  /* this should never happen */
  /* in case it does, the results should be very significant */
  assign
    p_descr      = ?
    p_for-type   = ?
    p_stnd-lngth = ?
    p_id-number  = ?
    p_pro-type   = ?
    p_family     = ?
    p_dflt-frmt  = ?.
  leave.
  end.    /* this should never happen */
  
repeat i = 1 to num-entries(_Db._Db-misc2[8],"~\"):
  assign
    l_stri       = entry(i,_Db._Db-misc2[8],"~\")
    p_descr      = p_descr      + entry(1,l_stri,"|") + ","
    p_for-type   = p_for-type   + entry(2,l_stri,"|") + ","
    p_stnd-lngth = p_stnd-lngth + entry(3,l_stri,"|") + ","
    p_id-number  = p_id-number  + entry(4,l_stri,"|") + ","
    p_pro-type   = p_pro-type   
                 +  entry(lookup(entry(5,l_stri,"|"),l_pt),l_pro-types)
                 + ","
    p_family     = p_family     + entry(6,l_stri,"|") + ","
    p_dflt-frmt  = p_dflt-frmt  + entry(7,l_stri,"|") + "|".
  end.

/*------------------------------------------------------------------*/
