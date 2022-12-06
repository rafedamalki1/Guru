/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/
/*-------------------------------------------------------------
File: as4synix.i

Description:  These are the field assignments for the _Index
       file, which loads the values stored in the p__Index record
       (AS/400) to the _Index (PROGRESS Schema file).  
       
       ***Note that there are some fields that we may
       not want to overlay with AS/400 information.  Recid fields
       will never be overlayed.  
       
History:

      nhorn     12/09/94   Created   
-------------------------------------------------------------*/


/* ==================== Main Line Code  ===================== */
Assign _Index._Active = (If as4dict.p__Index._Active = "Y" then yes else no).
    
if _Index._Desc <> as4dict.p__Index._Desc then     
    _Index._Desc  =  as4dict.p__Index._Desc. 

if _Index._For-name <> as4dict.p__Index._For-name then 
     _Index._For-name = as4dict.p__Index._For-name.

if _Index._For-Type <> as4dict.p__Index._For-Type then 
       _Index._For-Type       =     as4dict.p__Index._For-Type.

if _Index._I-Misc1[1] <> as4dict.p__Index._I-Misc1[1] then 
       _Index._I-Misc1[1]   =     as4dict.p__Index._I-Misc1[1].
if _Index._I-Misc1[2] <> as4dict.p__Index._I-Misc1[2] then        
       _Index._I-Misc1[2]   =     as4dict.p__Index._I-Misc1[2].
if _Index._I-Misc1[3] <> as4dict.p__Index._I-Misc1[3] then 
       _Index._I-Misc1[3]   =     as4dict.p__Index._I-Misc1[3].
if _Index._I-Misc1[4] <> as4dict.p__Index._I-Misc1[4] then 
       _Index._I-Misc1[4]   =     as4dict.p__Index._I-Misc1[4].
if _Index._I-Misc1[5] <> as4dict.p__Index._I-Misc1[5] then 
       _Index._I-Misc1[5]   =     as4dict.p__Index._I-Misc1[5].
if _Index._I-Misc1[6] <> as4dict.p__Index._I-Misc1[6] then 
       _Index._I-Misc1[6]   =     as4dict.p__Index._I-Misc1[6].
if _Index._I-Misc1[7] <> as4dict.p__Index._I-Misc1[7] then 
       _Index._I-Misc1[7]   =     as4dict.p__Index._I-Misc1[7].
if _Index._I-Misc1[8] <> as4dict.p__Index._I-Misc1[8] then 
       _Index._I-Misc1[8]   =     as4dict.p__Index._I-Misc1[8].

if _Index._I-Misc2[1] <> as4dict.p__Index._I-Misc2[1] then 
       _Index._I-Misc2[1]   =     as4dict.p__Index._I-Misc2[1].
if _Index._I-Misc2[2] <> as4dict.p__Index._I-Misc2[2] then        
       _Index._I-Misc2[2]   =     as4dict.p__Index._I-Misc2[2].
if _Index._I-Misc2[3] <> as4dict.p__Index._I-Misc2[3] then 
       _Index._I-Misc2[3]   =     as4dict.p__Index._I-Misc2[3].
if _Index._I-Misc2[4] <> as4dict.p__Index._I-Misc2[4] then 
       _Index._I-Misc2[4]   =     as4dict.p__Index._I-Misc2[4].
if _Index._I-Misc2[5] <> as4dict.p__Index._I-Misc2[5] then 
       _Index._I-Misc2[5]   =     as4dict.p__Index._I-Misc2[5].
if _Index._I-Misc2[6] <> as4dict.p__Index._I-Misc2[6] then 
       _Index._I-Misc2[6]   =     as4dict.p__Index._I-Misc2[6].
if _Index._I-Misc2[7] <> as4dict.p__Index._I-Misc2[7] then 
       _Index._I-Misc2[7]   =     as4dict.p__Index._I-Misc2[7].
if _Index._I-Misc2[8] <> as4dict.p__Index._I-Misc2[8] then 
       _Index._I-Misc2[8]   =     as4dict.p__Index._I-Misc2[8].

Assign _Index._Unique = (If as4dict.p__Index._Unique = "Y" then yes else no).

if _Index._Wordidx <> as4dict.p__Index._Wordidx then 
       _Index._Wordidx   =     as4dict.p__Index._Wordidx. 
                          
       
