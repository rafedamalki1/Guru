/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _ism_typ.p - C-ISAM/NetISAM-to-Progress datatype conversion subroutine */

/*
Pass parameters, and this routine will fill in the unknowns, as long as
io-pro-type or io-gate-type is defined.

  in:  io-pro-type  = ? or Progress datatype
       io-length    = ? or _Fld-stlen; size, or 0 for variable length
       io-dtype     = ? or _Fld-stdtype; gate datatype code for _Fld-stdtype
       io-gate-type = ? or C-ISAM/NetISAM datatype
  out: io-pro-type  = Progress datatype
       io-length    = _Fld-stlen
       io-dtype     = _Fld-stdtype
       io-gate-type = C-ISAM/NetISAM datatype
       io-format    = suggested format

To get the list of progress types that can be mapped to a particular
gateway type:
  in:  io-pro-type  = "get-list"
       io-gate-type = ?
  out: io-pro-type = comma separated list of pro_types

To get the C-ISAM/NetISAM-to-PROGRESS tables copied to the environment:
  in:  io-pro-type  = ?
       io-gate-type = ?
  out: user_env[11] = gate_desc
       user_env[12] = gate_type
       user_env[13] = gate_stlen
       user_env[14] = gate_stdtype
       user_env[15] = pro_type
       user_env[16] = gate_family
       user_env[17] = pro_format

  Note: gate_family is the data type family.  This indicates which data
  types can be changed to which other types - only types within the same
  family can be switched with each other.  0 = an orphan: cannot be changed.
*/

/* history:
    94/07/14    hutegger    added output of format in user_env[17] 
                            plus support of i, l, # for formats
                            plus usage of this program for _xxx_mak.p
*/ 

                            
/* NOTES for gate-config: 
1) Standard character string must be first in table! 
2) All entries that are the same gateway type must be contiguous in the 
   table.
3) the format can contain either 
   * a specific format                                             OR 
   * {c<d,i,l,#} for (character, date, integer, logical, decimal} to use 
     the format created by the hardcoded algorithm in _xxx_mak.i   OR
   * ? to use the PROGRESS default format  
*/ 

/* NOTE! cd = 40 is being used for COBOL Unsign Packed which appears in the */
/* table after COBOL Packed (cd = 19). Be sure not to accidently reuse!! */

DEFINE VARIABLE gate-config AS CHARACTER EXTENT 55 NO-UNDO INITIAL [
 /*description          datatype   sz cd pro type fm format*/
 /*-------------        --------   -- -- -------- -- ------*/
  "Characters          ,Char      , 0, 1,character,1,|c",
  "Signed Short        ,Int       , 2, 2,integer  ,1,|->>>>9",
  "Signed Long         ,Long      , 4, 3,integer  ,1,|->,>>>,>>>,>>9",
  "Decimal             ,Decimal   , 0, 4,decimal  ,1,|->,>>>,>>>,>>9.99",
  "Float               ,Float     , 0, 5,decimal  ,1,|->,>>>,>>>,>>9.99",
  "Double              ,Double    , 0, 6,decimal  ,1,|->,>>>,>>>,>>9.99",
  "Raw Data            ,Raw       , 0, 7,character,1,|c",
  "PROGRESS Date       ,Date      , 4, 8,date     ,1,|99/99/99",
  "Logical             ,Logical   , 0, 9,logical  ,1,|yes/no",
  "String              ,String    , 0,10,character,1,|c",
  "Native Short        ,Natint    , 2,11,integer  ,1,|->>>>9",
  "Native Long         ,Natlong   , 4,12,integer  ,1,|->,>>>,>>>,>>9",
  "Informix Char       ,Ichar     , 0,13,character,1,|c",
  "Informix Int        ,Iint      , 2,14,integer  ,1,|->>>>9",
  "Informix Long       ,Ilong     , 4,15,integer  ,1,|->,>>>,>>>,>>9",
  "Informix Dec        ,Idecimal  , 0,16,decimal  ,1,|->,>>>,>>>,>>9.99",
  "Informix Date       ,Idate     , 4,17,date     ,1,|99/99/99",
  "Byte                ,Byte      , 1,18,integer  ,1,|->>9",
  "COBOL Packed        ,Cpacked   , 0,19,decimal  ,1,|->,>>>,>>>,>>9.99",
  "COBOL Packed        ,Cpacked   , 0,19,integer  ,1,|->,>>>,>>>,>>9",
  "COBOL Unsign Packed ,CUpacked  , 0,40,decimal  ,1,|->,>>>,>>>,>>9.99",
  "COBOL Unsign Packed ,CUpacked  , 0,40,integer  ,1,|->,>>>,>>>,>>9",
  "COBOL Binary        ,Cbinary   , 0,20,decimal  ,1,|->,>>>,>>>,>>9.99",
  "COBOL Binary        ,Cbinary   , 0,20,integer  ,1,|->,>>>,>>>,>>9",
  "COBOL Lead Sep Sign ,Cleadsep  , 0,21,decimal  ,1,|->,>>>,>>>,>>9.99",
  "COBOL Lead Sep Sign ,Cleadsep  , 0,21,integer  ,1,|->,>>>,>>>,>>9",
  "COBOL Trail Sep Sign,Ctrailsep , 0,22,decimal  ,1,|->,>>>,>>>,>>9.99",
  "COBOL Trail Sep Sign,Ctrailsep , 0,22,integer  ,1,|->,>>>,>>>,>>9",
  "COBOL Leading Sign  ,Cleadsign , 0,23,decimal  ,1,|->,>>>,>>>,>>9.99",
  "COBOL Leading Sign  ,Cleadsign , 0,23,integer  ,1,|->,>>>,>>>,>>9",
  "COBOL Trailing Sign ,Ctrailsign, 0,24,decimal  ,1,|->,>>>,>>>,>>9.99",
  "COBOL Trailing Sign ,Ctrailsign, 0,24,integer  ,1,|->,>>>,>>>,>>9",
  "ASCII YY/MM/DD      ,Adateymds , 8,25,date     ,1,|99/99/99",
  "ASCII YYYY/MM/DD    ,Adateyymds,10,26,date     ,1,|99/99/9999",
  "ASCII MM/DD/YY      ,Adatemdys , 8,27,date     ,1,|99/99/99",
  "ASCII MM/DD/YYYY    ,Adatemdyys,10,28,date     ,1,|99/99/9999",
  "ASCII DD/MM/YY      ,Adatedmys , 8,29,date     ,1,|99/99/99",
  "ASCII DD/MM/YYYY    ,Adatedmyys,10,30,date     ,1,|99/99/9999",
  "ASCII YYMMDD        ,Adateymd  , 6,31,date     ,1,|99/99/99",
  "ASCII YYYYMMDD      ,Adateyymd , 8,32,date     ,1,|99/99/9999",
  "ASCII MMDDYY        ,Adatemdy  , 6,33,date     ,1,|99/99/99",
  "ASCII MMDDYYYY      ,Adatemdyy , 8,34,date     ,1,|99/99/9999",
  "ASCII DDMMYY        ,Adatedmy  , 6,35,date     ,1,|99/99/99",
  "ASCII DDMMYYYY      ,Adatedmyy , 8,36,date     ,1,|99/99/9999",
  "UNSIGNED BINARY     ,Ubinary   , 0,37,decimal  ,1,|>>>,>>>,>>>,>>>,>>>,>>9", 
  "UNSIGNED BINARY     ,Ubinary   , 0,37,integer  ,1,|>>>,>>>,>>>,>>>,>>>,>>9", 
  "ACU COBOL LEADING   ,Aleadsign , 0,38,decimal  ,1,|->,>>>,>>>,>>9.99",
  "ACU COBOL LEADING   ,Aleadsign , 0,38,integer  ,1,|->,>>>,>>>,>>9",
  "ACU COBOL TRAILING  ,Atrailsign, 0,39,decimal  ,1,|->,>>>,>>>,>>9.99",
  "ACU COBOL TRAILING  ,Atrailsign, 0,39,integer  ,1,|->,>>>,>>>,>>9",
  ?
].

{ prodict/user/uservar.i }

DEFINE INPUT-OUTPUT PARAMETER io-dtype     AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER io-length    AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER io-pro-type  AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER io-gate-type AS CHARACTER NO-UNDO.
DEFINE       OUTPUT PARAMETER io-format    AS CHARACTER NO-UNDO.

DEFINE              VARIABLE  c            AS CHARACTER NO-UNDO.
DEFINE              VARIABLE  gate_desc    AS CHARACTER NO-UNDO.
DEFINE              VARIABLE  gate_stdtype AS CHARACTER NO-UNDO.
DEFINE              VARIABLE  gate_stlen   AS CHARACTER NO-UNDO.
DEFINE              VARIABLE  gate_type    AS CHARACTER NO-UNDO.
DEFINE              VARIABLE  gate_family  AS CHARACTER NO-UNDO.
DEFINE              VARIABLE  i            AS INTEGER   NO-UNDO.
DEFINE              VARIABLE  pro_format   AS CHARACTER NO-UNDO.
DEFINE              VARIABLE  pro_type     AS CHARACTER NO-UNDO.

/* get the user-defined data-types from the _db-record */
RUN prodict/ism/_ism_udt.p
  ( OUTPUT gate_desc,
    OUTPUT gate_type,
    OUTPUT gate_stlen,
    OUTPUT gate_stdtype,
    OUTPUT pro_type,
    OUTPUT gate_family,
    OUTPUT pro_format
    ).

/* If we want the list of progress types that match a given gateway
   type, then just get that.  A gateway type that matches multiple
   Progress types are in contiguous entries in the gate-config table.
*/
IF io-gate-type <> ? AND io-pro-type = "get-list" THEN DO:
  DO i = 1 TO i + 1 WHILE gate-config[i] <> ?:
    IF TRIM(ENTRY(2,gate-config[i])) = io-gate-type THEN
       LEAVE.
  END.
  io-pro-type = "".
  DO WHILE TRIM(ENTRY(2,gate-config[i])) = io-gate-type:
    io-pro-type = io-pro-type + (IF io-pro-type = "" THEN "" ELSE ",") +
      	       	  TRIM(ENTRY(5,gate-config[i])).
    i = i + 1.
  END.
  
  /* user-defined data-type? */
  IF io-pro-type = "" THEN DO:
    repeat i = 1 to num-entries(gate_type):
      IF ENTRY(i,gate_type) = io-gate-type
      THEN ASSIGN io-pro-type = io-pro-type
                              + (IF io-pro-type = "" THEN "" ELSE ",")
                              + TRIM(ENTRY(i,pro_type)).
    END.  
  END.
  
  RETURN.
END.

/* extract the information from the table above into a form better
   used by the dictionary programs */

DO i = 1 TO i + 1 WHILE gate-config[i] <> ?:
  assign
    gate_desc      = gate_desc    + TRIM(ENTRY(1,gate-config[i])) + ","
    gate_type      = gate_type    + TRIM(ENTRY(2,gate-config[i])) + ","
    gate_stlen     = gate_stlen   + TRIM(ENTRY(3,gate-config[i])) + ","
    gate_stdtype   = gate_stdtype + TRIM(ENTRY(4,gate-config[i])) + ","
    pro_type       = pro_type     + TRIM(ENTRY(5,gate-config[i])) + ","
    gate_family    = gate_family  + TRIM(ENTRY(6,gate-config[i])) + ","
    gate-config[i] = SUBSTRING(gate-config[i]
                              ,INDEX(gate-config[i],"|") + 1
                              ,-1
                              ,"character")
    pro_format     = pro_format   + gate-config[i] + "|".
END.

IF io-gate-type = ? AND io-pro-type = ? THEN DO: /* set user_env fields */
  assign
    user_env[11] = gate_desc
    user_env[12] = gate_type
    user_env[13] = gate_stlen
    user_env[14] = gate_stdtype
    user_env[15] = pro_type
    user_env[16] = gate_family
    user_env[17] = pro_format.
  RETURN.
END.

assign i = 0.

/* PROGRESS datatype -> ISAM datatype (io-pro-type given, io-gate-type = ?) */
IF io-gate-type = ? THEN DO: 
  DO i = 1 TO NUM-ENTRIES(gate_type) - 1:
    IF (io-pro-type = ENTRY(i,pro_type))
      AND INTEGER(ENTRY(i,gate_stdtype)) = io-dtype
      AND (INTEGER(ENTRY(i,gate_stlen))  = io-length
        OR INTEGER(ENTRY(i,gate_stlen))  = 0) THEN LEAVE.
  END.
  io-gate-type = ENTRY(i,gate_type).
END.
ELSE DO: /* io-gate-type given, io-pro-type may be ? */
   i = LOOKUP(io-gate-type,gate_type).
   IF io-pro-type  = ? THEN 
      io-pro-type = ENTRY(i,pro_type).
   ELSE 
      /* Make sure we have the entry that matches both io-pro-type AND
      	 io-gate-type.
      */
      IF io-pro-type <> ENTRY(i,pro_type) THEN
	DO i = i + 1 to NUM-ENTRIES(gate_type) - 1:
	   IF io-pro-type = ENTRY(i,pro_type) AND
	      io-gate-type = ENTRY(i,gate_type) THEN LEAVE.
	END.
END.

ASSIGN
  io-length  = (IF INTEGER(ENTRY(i,gate_stlen)) = 0 THEN io-length
               ELSE INTEGER(ENTRY(i,gate_stlen)))
  io-format  = (IF gate-config[i] = "d" THEN "99/99/99"
               ELSE IF gate-config[i] = "c" THEN
                 "x(" + STRING(IF io-length = 0 THEN 8 ELSE io-length) + ")"
               ELSE gate-config[i])
  io-dtype   = INTEGER(ENTRY(i,gate_stdtype)).

RETURN.

