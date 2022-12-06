/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _as4_ddl.p - AS/400 DDL Dump manager for Progress databases */
/* Modified 12/15/93 jmorin 
            - Add check for CR and LF characters within dumped data fields.
            - Do not dump index containing char field > 120 chars.
   Modified 12/21/93 jmorin
            - Generate error file for def's not dumped.

   Modified 5/4/94 dmcmann
            - Removed default assigning of field-name to
              column label if no column label is defined.            
              
    Modified 6/1/95 dmcmann
            - Added shared frame so field name and index name will display Bug 94-08-01-030
            - Changed how size of index is calculated Bug 95-05-26-021
            
    Modified 7/27/95 dmcmann
            - Added skipping of word indexes Bug 95-04-19-002            
*/             

{ prodict/dictvar.i }
{ prodict/as4/as4_pro.f  }

DEFINE INPUT PARAMETER fil_recid AS RECID NO-UNDO.

DEFINE SHARED STREAM ddl.
DEFINE SHARED STREAM dumperr.

DEFINE SHARED VARIABLE fil-e    AS CHARACTER           NO-UNDO.
DEFINE SHARED VARIABLE fil-df   AS CHARACTER           NO-UNDO.
DEFINE SHARED VARIABLE i-err    AS LOGICAL             NO-UNDO.
DEFINE SHARED VARIABLE i-cnt    AS INTEGER             NO-UNDO.                      
DEFINE VARIABLE ddl   AS CHARACTER EXTENT 30 NO-UNDO.
DEFINE VARIABLE i     AS INTEGER             NO-UNDO.
DEFINE VARIABLE x     AS INTEGER             NO-UNDO.
DEFINE VARIABLE y     AS INTEGER             NO-UNDO.
DEFINE VARIABLE xlist AS CHARACTER
                      INITIAL "1,2,3,4,5,6,7,8,9,0" NO-UNDO.
DEFINE VARIABLE clen  AS CHARACTER           NO-UNDO.
DEFINE VARIABLE ilen  AS INTEGER             NO-UNDO.         

FIND DICTDB._File WHERE RECID(DICTDB._File) = fil_recid.
ASSIGN ddl = "".
      
RUN "prodict/_dctquot.p" (_File._Can-Create,'"',OUTPUT ddl[2]).
RUN "prodict/_dctquot.p" (_File._Can-Read  ,'"',OUTPUT ddl[3]).
RUN "prodict/_dctquot.p" (_File._Can-Write ,'"',OUTPUT ddl[4]).
RUN "prodict/_dctquot.p" (_File._Can-Delete,'"',OUTPUT ddl[5]).
RUN "prodict/_dctquot.p" (_File._Desc      ,'"',OUTPUT ddl[6]).
RUN "prodict/_dctquot.p" (_File._Valexp    ,'"',OUTPUT ddl[7]).
RUN "prodict/_dctquot.p" (_File._Valmsg    ,'"',OUTPUT ddl[8]).
RUN "prodict/_dctquot.p" (_File._Dump-name ,'"',OUTPUT ddl[11]).

RUN "prodict/as4/_as4bchr.p" (INPUT-OUTPUT ddl[7],chr(10)).
RUN "prodict/as4/_as4bchr.p" (INPUT-OUTPUT ddl[7],chr(13)).
RUN "prodict/as4/_as4bchr.p" (INPUT-OUTPUT ddl[6],chr(10)).
RUN "prodict/as4/_as4bchr.p" (INPUT-OUTPUT ddl[6],chr(13)).
   
ASSIGN /* PROGRESS */
  ddl[ 1] = 'ADD FILE "' + _File._File-name + '" TYPE PROGRESS'
  ddl[ 2] = "  CAN-CREATE " + ddl[2]  /*_Can-Create*/
  ddl[ 3] = "  CAN-READ   " + ddl[3]  /*_Can-Read*/
  ddl[ 4] = "  CAN-WRITE  " + ddl[4]  /*_Can-Write*/
  ddl[ 5] = "  CAN-DELETE " + ddl[5]  /*_Can-Delete*/
  ddl[ 6] = (IF _File._Desc = "" OR _File._Desc = ?
            THEN "" ELSE "  DESCRIPTION " + ddl[6])
  ddl[ 7] = (IF _File._Valexp = "" OR _File._Valexp = ?
            THEN "" ELSE "  VALEXP " + ddl[7])
  ddl[ 8] = (IF _File._Valmsg = "" OR _File._Valmsg = ?
            THEN "" ELSE "  VALMSG " + ddl[8])
  ddl[ 9] = (IF _File._Frozen THEN "  FROZEN" ELSE "")
  ddl[10] = (IF _File._Hidden THEN "  HIDDEN" ELSE "")
  ddl[11] = "  DUMP-NAME " + ddl[11].
{ prodict/as4/as4_ddl.i }
PUT STREAM ddl UNFORMATTED SKIP(1).

ddl = "".
FOR EACH DICTDB._Field OF DICTDB._File BY _Field._Field-rpos:       
  display DICTDB._Field._Field-name with frame marker.
  RUN "prodict/_dctquot.p" (_Field._Desc     ,'"',OUTPUT ddl[ 2]).
  RUN "prodict/_dctquot.p" (_Field._Format   ,'"',OUTPUT ddl[ 3]).
  RUN "prodict/_dctquot.p" (_Field._Initial  ,'"',OUTPUT ddl[ 4]).
  RUN "prodict/_dctquot.p" (_Field._Label    ,'"',OUTPUT ddl[ 5]).
  RUN "prodict/_dctquot.p" (_Field._Col-label,'"',OUTPUT ddl[ 6]).
  RUN "prodict/_dctquot.p" (_Field._Can-Read ,'"',OUTPUT ddl[ 7]).
  RUN "prodict/_dctquot.p" (_Field._Can-Write,'"',OUTPUT ddl[ 8]).
  RUN "prodict/_dctquot.p" (_Field._Valexp   ,'"',OUTPUT ddl[ 9]).
  RUN "prodict/_dctquot.p" (_Field._Valmsg   ,'"',OUTPUT ddl[10]).
  RUN "prodict/_dctquot.p" (_Field._Help     ,'"',OUTPUT ddl[11]).
  
  RUN "prodict/as4/_as4bchr.p" (INPUT-OUTPUT ddl[2],chr(10)).
  RUN "prodict/as4/_as4bchr.p" (INPUT-OUTPUT ddl[2],chr(13)).
  RUN "prodict/as4/_as4bchr.p" (INPUT-OUTPUT ddl[9],chr(10)).
  RUN "prodict/as4/_as4bchr.p" (INPUT-OUTPUT ddl[9],chr(13)).
  
  ASSIGN
    ddl[ 1] = 'ADD FIELD "' + _Field._Field-name
            + '" OF "' + DICTDB._File._File-name
            + '" AS ' + _Field._Data-type
    ddl[ 2] = (IF _Field._Desc = "" OR _Field._Desc = ?
              THEN "" ELSE "  DESCRIPTION " + ddl[2])
    ddl[ 3] = "  FORMAT " + ddl[3]
    ddl[ 4] = "  INITIAL " + ddl[4]
    ddl[ 5] = (IF _Field._Label = ? 
              THEN "  LABEL " + '"' + _Field._Field-name + '"'
              ELSE "  LABEL " + ddl[5])
    ddl[ 6] = (IF _Field._Col-label = ?  THEN ""                           
    
               ELSE "  COLUMN-LABEL " + ddl[6])
    ddl[ 7] = "  CAN-READ  " + ddl[7]
    ddl[ 8] = "  CAN-WRITE " + ddl[8]
    ddl[ 9] = (IF _Valexp = ? OR _Valexp = "" THEN "" ELSE
              "  VALEXP " + ddl[9])
    ddl[10] = (IF _Valmsg = ? OR _Valmsg = "" THEN "" ELSE
              "  VALMSG " + ddl[10])
    ddl[11] = (IF _Help = "" OR _Help = ?
              THEN "" ELSE "  HELP " + ddl[11])
    ddl[12] = (IF _Extent = 0 THEN "" ELSE "  EXTENT "
            + STRING(_Field._Extent))
    ddl[13] = (IF _Decimals = ? THEN ""
              ELSE "  DECIMALS " + STRING(_Field._Decimals))
    ddl[14] = (IF _Field._Mandatory THEN "  MANDATORY" ELSE "")
    ddl[15] = "  ORDER " + STRING(_Field._Order)
    ddl[16] = (IF _Fld-case THEN "  CASE-SENSITIVE" ELSE "")
    ddl[25] = (IF _Fld-misc1[5] = ? THEN "" ELSE
              "  FOREIGN-ALLOCATED " + STRING(_Field._Fld-misc1[5]))
    ddl[26] = (IF _Fld-misc1[6] = ? THEN "" ELSE
              "  FOREIGN-MAXIMUM " + STRING(_Field._Fld-misc1[6])).
  { prodict/as4/as4_ddl.i }
  PUT STREAM ddl UNFORMATTED SKIP(1).
END.
 
_idxloop: FOR EACH DICTDB._Index OF DICTDB._File:
  /* don't print the default index */
  IF DICTDB._File._dft-pk AND DICTDB._File._Prime-Index = RECID(_Index)
    THEN NEXT.            
  IF DICTDB._Index._Wordidx  <> ? AND DICTDB._Index._Wordidx > 0 THEN DO:   
    OUTPUT STREAM dumperr TO VALUE(fil-e) APPEND NO-ECHO.
     IF i-cnt = 0 THEN
        PUT STREAM dumperr UNFORMATTED 
            "Schema information not dumped to file: " fil-df   SPACE (20) TODAY  SKIP (1).
    PUT STREAM dumperr UNFORMATTED  " " SKIP
     "  Word indexes are not supported" SKIP
           "   File/Index:  " .
    ASSIGN i-cnt = i-cnt + 1.
    PUT STREAM dumperr UNFORMATTED
    DICTDB._File._File-name "/"
    DICTDB._Index._Index-name  SKIP(1) .
    OUTPUT STREAM dumperr CLOSE.
    NEXT _idxloop.
  END.  
    DISPLAY DICTDB._Index._Index-name with frame marker.         
    ASSIGN ilen = 0.
  FOR EACH DICTDB._Index-field OF _Index NO-LOCK:  
        FOR EACH DICTDB._Field OF _Index-field NO-LOCK:     
            IF DICTDB._Field._dtype = 1 THEN DO:          
                ASSIGN clen = "".      
                IF (SUBSTRING(DICTDB._Field._Format,1,1) = "x" OR
                     SUBSTRING(DICTDB._Field._Format,1,1) = "a" OR
                     SUBSTRING(DICTDB._Field._Format,1,1) = "!")
                  AND SUBSTRING(DICTDB._Field._Format,2,1) = "("  THEN DO:      
                        REPEAT x = 1 TO LENGTH(DICTDB._Field._Format):
                            ASSIGN y = LOOKUP(SUBSTRING(DICTDB._Field._Format,x,1),xlist).
                            IF y <= 9 AND y NE 0 THEN ASSIGN clen = (clen + STRING(y)).
                            ELSE IF y = 10 THEN ASSIGN clen = (clen + "0").
                        END.     
                        ASSIGN ilen = ilen + INTEGER(clen).  
                END.
                ELSE ASSIGN ilen = ilen + LENGTH(DICTDB._Field._Format).
            END.  /* end character data type */
            ELSE ASSIGN ilen = ilen + LENGTH(DICTDB._Field._Format).
        END. /* end _field = index field */     
    END. /*  end of all index fields in index */              
        
    IF ilen GT 200 THEN DO:    
       OUTPUT STREAM dumperr TO VALUE(fil-e) APPEND NO-ECHO.     
        IF i-cnt = 0 THEN
            PUT STREAM dumperr UNFORMATTED 
                "Schema information not dumped to file: " fil-df  SPACE (20) TODAY SKIP (1).
            PUT STREAM dumperr UNFORMATTED " " SKIP
                "  Index with length > 200 characters " SKIP
                "   File/Index:  ".
            ASSIGN i-cnt = i-cnt + 1.
    
        PUT STREAM dumperr UNFORMATTED
           DICTDB._File._File-name  "/" 
           DICTDB._Index._Index-name   SKIP(1).                 
          OUTPUT STREAM dumperr CLOSE.
        NEXT _idxloop.
    END. /* ilen gt 120 */
 
  ASSIGN  
    i      = 1 
    ddl    = ""
    ddl[1] = "ADD " 
           + (IF _Index._Unique THEN "UNIQUE " ELSE "")
           + (IF _Index._Active THEN "" ELSE "INACTIVE ")
           + (IF DICTDB._File._Prime-index = RECID(_Index)
             THEN "PRIMARY " ELSE "")
           + 'INDEX "' + _Index._Index-Name 
           + '" ON "' + DICTDB._File._File-name + '"'. 
  FOR EACH DICTDB._Index-field OF _Index,DICTDB._Field OF _Index-field
    BREAK BY DICTDB._Index-field._Index-seq:
    ASSIGN
      i = i + 1
      ddl[i] = '  INDEX-FIELD "' + _Field._Field-Name + '" '
             + (IF _Index-field._Ascending THEN "A" ELSE "DE") + "SCENDING"
             + (IF _Index-field._Abbreviate THEN " ABBREVIATED" ELSE "").
  END.
  { prodict/as4/as4_ddl.i }
  PUT STREAM ddl UNFORMATTED SKIP(1).

END.

RETURN.
