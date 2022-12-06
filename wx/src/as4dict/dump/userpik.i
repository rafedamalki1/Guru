/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* userpik.i - definitions for flexible 'pick' program _usrpick.p 

   Modified 06/05/97 to work with the AS/400 DataServer

*/
/*
pik_chosen - list of chosen element ids (was pik_choice)
pik_column - column position of frame   (0 for default)
pik_count  - number of elements in      (was pik_chextent)
pik_down   - iterations of down frame   (usually 0 for default)
pik_first  - first element out or ?     (same as pik_list[pik_chosen[1]])
pik_hide   - hide frame when done?
pik_init   - initial value to position cursor
pik_list   - list of elements in        (was pik_chlist)
pik_multi  - can pick multiple?         (was pik_multiple)
pik_number - number the elements?       (only with pik_multi)
pik_return - number of elements chosen  (was pik_chcnt)
pik_row    - row position of frame      (0 for default)
pik_skip   - use 'skip number' option   (only with pik_number)
pik_start  - set true/false from values in pik_chosen
pik_title  - title of frame             (spaces prepended and appended)
pik_wide   - use wide frame?
pik_text   - instructional text 
pik_help   - a help context Id to explain this use of the picker.
*/

/*input:*/
DEFINE {1} SHARED VARIABLE pik_column AS INTEGER                NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_count  AS INTEGER                NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_down   AS INTEGER                NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_hide   AS LOGICAL   INITIAL TRUE NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_init   AS CHARACTER INITIAL  ""	NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_list   AS CHARACTER EXTENT 0640	NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_multi  AS LOGICAL   INIT FALSE   NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_number AS LOGICAL		NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_row    AS INTEGER		NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_skip   AS LOGICAL   INIT FALSE   NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_title  AS CHARACTER		NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_wide   AS LOGICAL		NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_text   AS CHARACTER INITIAL ?	NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_help   AS INTEGER 	        NO-UNDO.

/*output:*/
DEFINE {1} SHARED VARIABLE pik_chosen AS INTEGER   EXTENT 0640  NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_first  AS CHARACTER              NO-UNDO.
DEFINE {1} SHARED VARIABLE pik_return AS INTEGER   INITIAL 0    NO-UNDO.

/* Added defines for PROGRESS/400 Data Dictionary incremental dump      */
DEFINE {1} SHARED VARIABLE pick-fg AS CHARACTER INITIAL "NORMAL"   NO-UNDO.
DEFINE {1} SHARED VARIABLE pick-bg AS CHARACTER INITIAL "MESSAGES" NO-UNDO.
DEFINE {1} SHARED VARIABLE cache_db# AS INTEGER  INITIAL 0    NO-UNDO.
DEFINE {1} SHARED VARIABLE cache_db_t  AS CHARACTER EXTENT 64   NO-UNDO.
DEFINE {1} SHARED VARIABLE cache_db_l  AS CHARACTER EXTENT 64   NO-UNDO.

DEFINE {1} SHARED STREAM logfile.
DEFINE {1} SHARED VARIABLE logfile_open AS LOGICAL NO-UNDO INITIAL false.

/* Definitions for temporary tables to work with two as400 databases without build change */
DEFINE {1} SHARED TEMP-TABLE wfil NO-UNDO LIKE as4dict.p__File
  INDEX p__file IS PRIMARY _File-number
  INDEX p__filel0 _File-name.
DEFINE {1} SHARED TEMP-TABLE wfit NO-UNDO LIKE as4dict.p__Trgfl.
DEFINE {1} SHARED TEMP-TABLE wfld NO-UNDO LIKE as4dict.p__Field
 INDEX p__field IS PRIMARY _file-number _fld-number
 INDEX p__Fieldl1 _field-name _file-number
 INDEX p__Fieldl2 _file-number _fld-number.
DEFINE {1} SHARED TEMP-TABLE wflt NO-UNDO LIKE as4dict.p__Trgfd.
DEFINE {1} SHARED TEMP-TABLE widx NO-UNDO LIKE as4dict.p__Index
  INDEX p__Index IS PRIMARY _file-number _index-name
  INDEX p__indexl0 _Index-name
  INDEX p__Indexl1 _File-number _idx-num.
DEFINE {1} SHARED TEMP-TABLE wixf NO-UNDO LIKE as4dict.p__Idxfd
  INDEX p__idxfd IS PRIMARY _file-number _idx-num _Index-seq
  INDEX p__idxfdl1 _file-number _idx-num _Fld-number.
DEFINE {1} SHARED TEMP-TABLE wseq NO-UNDO LIKE as4dict.p__Seq. 

/*local:*/
DEFINE VARIABLE p_mark  AS CHARACTER NO-UNDO.
DEFINE VARIABLE p_recid AS INTEGER   NO-UNDO.
DEFINE VARIABLE nextnum AS INTEGER   NO-UNDO.

/*frames:*/
DEFINE {1} SHARED FRAME pick1.
DEFINE {1} SHARED FRAME pick2.

/*forms:*/
FORM
  p_mark            FORMAT "x(3)" NO-ATTR-SPACE SPACE(0)
  pik_list[p_recid] FORMAT "x(32)"   ATTR-SPACE SPACE(0)
  WITH FRAME pick1 SCROLL 1 OVERLAY NO-LABELS NO-ATTR-SPACE
  pik_down DOWN ROW pik_row COLUMN pik_column
  COLOR DISPLAY VALUE(pick-fg) PROMPT VALUE(pick-bg)
  TITLE COLOR VALUE(pick-fg) pik_title
  USE-TEXT.

FORM
  p_mark            FORMAT "x(3)" NO-ATTR-SPACE SPACE(0)
  pik_list[p_recid] FORMAT "x(40)"   ATTR-SPACE SPACE(0)
  WITH FRAME pick2 SCROLL 1 OVERLAY NO-LABELS NO-ATTR-SPACE
  pik_down DOWN ROW pik_row COLUMN pik_column
  COLOR DISPLAY VALUE(pick-fg) PROMPT VALUE(pick-bg)
  TITLE COLOR VALUE(pick-fg) pik_title
  USE-TEXT. 
  
/* This frame is used to display the next number in the
 * flagging sequence when 'skip' is turned on. Otherwise,
 * it is hidden.
 */
FORM
  nextnum FORMAT ">,>>9" NO-ATTR-SPACE SPACE(0)
  WITH FRAME fskip NO-LABELS TITLE "Next Num" USE-TEXT.
