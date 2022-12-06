/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/
/* File:  as4dict/dump/dumpvar.i                             */
/*                                                           */
/* The following definitions were added from prodict/user/uservar.i to  */
/* be used in the load/dump utilities.                                  */
/*                                                            */
/* Initial creation: May 4, 1995   Nhorn                      */

/*         Modified: 02/19/97 DLM changed cache_file from 1024 to 2048
                                  bug 97-02-19-002 
*/
DEFINE BUTTON    btn_Ok     LABEL "OK"     {&STDPH_OKBTN} AUTO-GO.
DEFINE BUTTON    btn_Cancel LABEL "Cancel" {&STDPH_OKBTN} AUTO-ENDKEY.
DEFINE RECTANGLE rect_Btns {&STDPH_OKBOX}.
DEFINE BUTTON    btn_Help LABEL "&Help" {&STDPH_OKBTN}.

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN 
   &GLOBAL-DEFINE   HLP_BTN  &HELP = "btn_Help"
   &GLOBAL-DEFINE   HLP_BTN_NAME btn_Help
&ENDIF

DEFINE {1} VARIABLE user_dbname   AS CHARACTER           NO-UNDO.
DEFINE {1} VARIABLE user_dbtype   AS CHARACTER           NO-UNDO.
DEFINE {1} VARIABLE user_path     AS CHARACTER           NO-UNDO.
DEFINE {1} VARIABLE user_filename AS CHARACTER           NO-UNDO.
DEFINE {1} VARIABLE user_hdr      AS CHARACTER           NO-UNDO.

DEFINE {1} VARIABLE cache_file# AS INTEGER  INITIAL 0    NO-UNDO.
DEFINE {1} VARIABLE cache_file  AS CHARACTER EXTENT 2048 NO-UNDO.
DEFINE {1} VARIABLE cache_dirty AS LOGICAL  INITIAL TRUE NO-UNDO.

DEFINE {1} VARIABLE drec_db AS RECID NO-UNDO.
    
/* DEFINE {1} VARIABLE file_num   AS INTEGER   INITIAL ?  NO-UNDO. */
/* DEFINE {1} VARIABLE User_Cancel AS LOGICAL NO-UNDO. */



