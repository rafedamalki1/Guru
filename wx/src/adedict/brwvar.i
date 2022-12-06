/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: brwvar.i

Description:   
   Include file which defines the user interface components in the browse
   window of the dictionary.
 
Arguments:
   {1} - this is either "new shared" or "shared".

Author: Laura Stern

Date Created: 01/28/92 
Date Modified: 11/27/96 kkelley - removed text under data dictionary icons

----------------------------------------------------------------------------*/

/* (FIX - uncomment) 
&IF DEFINED(ADESTDSI) = 0 &THEN 
{adecomm/adestds.i}  /* Layout Global defines, colors etc. */
&ENDIF */

Define {1} frame browse.

/* Toggling icons for selecting which object type is current. */
Define button s_icn_Dbs  image  FILE "adeicon/db-u" tooltip "Databases".
Define button s_icn_Tbls image  FILE "adeicon/table-u" tooltip "Tables".
/* Define image s_icn_Doms FILE "adeicon/domup". */
Define button s_icn_Seqs image  FILE "adeicon/seq-u" tooltip "Sequences".
Define button s_icn_Flds image  FILE "adeicon/flds-u" tooltip "Fields".
Define button s_icn_Idxs image  FILE "adeicon/index-u" tooltip "Indexes".

/* Labels for toggling icons */
/* Removed text under dictionary icons for WIN95
   Define {1} var s_DbLbl  as char NO-UNDO init "Databases".
   Define {1} var s_TblLbl as char NO-UNDO init "Tables".
   /* Define {1} var s_DomLbl as char NO-UNDO init "Domains". */
   Define {1} var s_SeqLbl as char NO-UNDO init "Sequences".
   Define {1} var s_FldLbl as char NO-UNDO init "Fields".
   Define {1} var s_IdxLbl as char NO-UNDO init "Indexes".
*/
/* Label for each level of object to indicate what is being displayed. */
Define {1} var s_DbLbl2  as char NO-UNDO init " Databases".
Define {1} var s_Lvl1Lbl as char NO-UNDO.
Define {1} var s_Lvl2Lbl as char NO-UNDO.

/* Fill-in fields for searching through selection lists */
Define {1} var s_DbFill  as char NO-UNDO.
Define {1} var s_TblFill as char NO-UNDO.
/* Define {1} var s_DomFill as char NO-UNDO. */
Define {1} var s_SeqFill as char NO-UNDO.
Define {1} var s_FldFill as char NO-UNDO.
Define {1} var s_IdxFill as char NO-UNDO.

/* Selection Lists for the lists of database objects. The view-as and size
   specifications are in the form (in browse.f) so that all formatting is
   done in one place.
*/
Define {1} var s_lst_Dbs  as char NO-UNDO.
Define {1} var s_lst_Tbls as char NO-UNDO.
/* Define {1} var s_lst_Doms as char NO-UNDO. */
Define {1} var s_lst_Seqs as char NO-UNDO.
Define {1} var s_lst_Flds as char NO-UNDO.
Define {1} var s_lst_Idxs as char NO-UNDO.

/* Action buttons to perform Create, Modify and Delete operations. The
   labels on these will change depending on the current object.  So
   for example, it s_btn_Create might say "Create Table" or "Create Index".
*/
Define button s_btn_Create SIZE 24 by 1.125.
Define button s_btn_Props  SIZE 24 by 1.125.
Define button s_btn_Delete SIZE 24 by 1.125.

/* Status line variable */
Define {1} var s_Browse_Stat as char NO-UNDO.

/* Form definition for the browse and icon frames. - must be here,
   below all widget definitions. */
{adedict/browse.f}







