/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/
/*-------------------------------------------------------------
File: as4syntn.i

Description:  These are the field assignments for the _file 
       file, which load the values stored in the p__file record
       (AS/400) to the _file (PROGRESS Schema file).  
       
       ***Note that there are some fields that we may
       not want to overlay with AS/400 information.  Recid fields
       will never be overlayed.  
       
History:

      nhorn     10/28/94   Created   
-------------------------------------------------------------*/


/* ==================== Main Line Code  ===================== */          
/* Dump name have to be unique thoughout the .db like sequences this
    takes care of it by making it unique if another exists.  Else the one
    coming in is used */    
if _File._Dump <> as4dict.p__File._Dump then DO:
    ASSIGN nam = as4dict.p__File._Dump-name.
     {prodict/dump/dumpname.i}
     ASSIGN _file._Dump-name = nam.
 END.

if _file._for-name <> as4dict.p__file._for-name then 
     _file._for-name       =     as4dict.p__file._for-name.

if _file._Desc <> as4dict.p__file._Desc 
AND as4dict.p__File._Desc <> "" then     
       _file._Desc           =     as4dict.p__file._Desc. 

assign _file._dft-pk         = (If as4dict.p__file._dft-pk = "Y" then yes else no).

if _file._Fil-Misc1[1] <> as4dict.p__file._Fil-Misc1[1]  THEN
       _file._Fil-Misc1[1]   =     as4dict.p__file._Fil-Misc1[1].    
       
if _file._Fil-Misc1[2] <> as4dict.p__file._Fil-Misc1[2]
AND as4dict.p__File._Fil-Misc1[2] <> 0 then        
       _file._Fil-Misc1[2]   =     as4dict.p__file._Fil-Misc1[2].   
       
if _file._Fil-Misc1[3] <> as4dict.p__file._Fil-Misc1[3]
AND as4dict.p__File._Fil-Misc1[3] <> 0 then 
       _file._Fil-Misc1[3]   =     as4dict.p__file._Fil-Misc1[3].  

/* The Foreign-level is assigned here and must contain something */       
if _file._Fil-Misc1[4] <> as4dict.p__file._Fil-Misc1[4]  then
       _file._Fil-Misc1[4]   =     as4dict.p__file._Fil-Misc1[4].    
       
if _file._Fil-Misc1[5] <> as4dict.p__file._Fil-Misc1[5]
AND as4dict.p__File._Fil-Misc1[5] <> 0 then 
       _file._Fil-Misc1[5]   =     as4dict.p__file._Fil-Misc1[5].  
       
if _file._Fil-Misc1[6] <> as4dict.p__file._Fil-Misc1[6]
AND as4dict.p__File._Fil-Misc1[6] <> 0 then
       _file._Fil-Misc1[6]   =     as4dict.p__file._Fil-Misc1[6].  
       
if _file._Fil-Misc1[7] <> as4dict.p__file._Fil-Misc1[7]
AND as4dict.p__File._Fil-Misc1[7] <> 0  then 
       _file._Fil-Misc1[7]   =     as4dict.p__file._Fil-Misc1[7].    
       
if _file._Fil-Misc1[8] <> as4dict.p__file._Fil-Misc1[8] 
AND as4dict.p__File._Fil-Misc1[8] <> 0 then 
       _file._Fil-Misc1[8]   =     as4dict.p__file._Fil-Misc1[8].

if _file._Fil-Misc2[1] <> as4dict.p__file._Fil-Misc2[1]
AND as4dict.p__File._Fil-Misc2[1] <>  "" then 
       _file._Fil-Misc2[1]   =     as4dict.p__file._Fil-Misc2[1].  
       
if _file._Fil-Misc2[2] <> as4dict.p__file._Fil-Misc2[2] 
AND as4dict.p__File._Fil-Misc2[2] <>  "" then        
       _file._Fil-Misc2[2]   =     as4dict.p__file._Fil-Misc2[2]. 
       
if _file._Fil-Misc2[3] <> as4dict.p__file._Fil-Misc2[3] 
AND as4dict.p__File._Fil-Misc2[3] <>  "" then 
       _file._Fil-Misc2[3]   =     as4dict.p__file._Fil-Misc2[3].    
       
if _file._Fil-Misc2[4] <> as4dict.p__file._Fil-Misc2[4] 
AND as4dict.p__File._Fil-Misc2[4] <>  "" then 
       _file._Fil-Misc2[4]   =     as4dict.p__file._Fil-Misc2[4].  
       
if _file._Fil-Misc2[5] <> as4dict.p__file._Fil-Misc2[5]
AND as4dict.p__File._Fil-Misc2[5] <>  ""  then 
       _file._Fil-Misc2[5]   =     as4dict.p__file._Fil-Misc2[5].
       
if _file._Fil-Misc2[6] <> as4dict.p__file._Fil-Misc2[6]
AND as4dict.p__File._Fil-Misc2[6] <>  ""  then 
       _file._Fil-Misc2[6]   =     as4dict.p__file._Fil-Misc2[6]. 
       
if _file._Fil-Misc2[7] <> as4dict.p__file._Fil-Misc2[7]
AND as4dict.p__File._Fil-Misc2[7] <>  ""  then 
       _file._Fil-Misc2[7]   =     as4dict.p__file._Fil-Misc2[7].
       
if _file._Fil-Misc2[8] <> as4dict.p__file._Fil-Misc2[8] 
AND as4dict.p__File._Fil-Misc2[8] <>  "" then 
       _file._Fil-Misc2[8]   =     as4dict.p__file._Fil-Misc2[8].

if _file._File-Label <> as4dict.p__file._File-Label
AND as4dict.p__File._File-Label <>  ""  then 
       _file._File-Label     =     as4dict.p__file._File-Label.

if _file._File-Label-SA <> as4dict.p__file._File-Label-SA 
AND as4dict.p__File._File-Label-SA <>  "" then 
       _file._File-Label-SA  =     as4dict.p__file._File-Label-SA .

if _file._For-Cnt1 <> as4dict.p__file._For-Cnt1
AND as4dict.p__File._For-Cnt1 <> 0  then 
       _file._For-Cnt1       =     as4dict.p__file._For-Cnt1.

if _file._For-Cnt2 <> as4dict.p__file._For-Cnt2
AND as4dict.p__File._For-Cnt2 <> 0  then 
       _file._For-Cnt2       =     as4dict.p__file._For-Cnt2.

if _file._For-Flag <> as4dict.p__file._For-Flag
AND as4dict.p__File._For-Flag <> 0  then 
       _file._For-Flag       =     as4dict.p__file._For-Flag.

if _file._For-Format <> as4dict.p__file._For-Format then 
       _file._For-Format     =     as4dict.p__file._For-Format.   /* Record Format Name */

if _file._For-Id <> as4dict.p__file._For-Id
AND as4dict.p__File._For-Id <> 0  then 
       _file._For-Id         =     as4dict.p__file._For-Id.

if _file._For-Info <> as4dict.p__file._For-Info
AND as4dict.p__File._For-Info <> ""  then 
       _file._For-Info       =     as4dict.p__file._For-Info.

if _file._For-Number <> as4dict.p__file._For-Number then 
       _file._For-Number     =     as4dict.p__file._For-Number.

if _file._For-Owner <> as4dict.p__file._For-Owner
AND as4dict.p__File._For-Owner <> ""  then 
       _file._For-Owner      =     as4dict.p__file._For-Owner.

if _file._For-Size <> as4dict.p__file._For-Size
AND as4dict.p__File._For-Size <> 0  then 
       _file._For-Size       =     as4dict.p__file._For-Size.

if _file._For-Type <> as4dict.p__file._For-type
AND as4dict.p__File._For-Type <> ""  then 
       _file._For-Type       =     as4dict.p__file._For-Type.

assign _file._Hidden         = (if as4dict.p__file._Hidden = "Y" then yes else no).

IF _File._Prime-Index <> as4dict.p__file._Prime-index 
  AND as4dict.p__file._Prime-index > 0 THEN
       _File._Prime-index    =     as4dict.p__File._Prime-index.
       
if _file._Valexp <> as4dict.p__file._Valexp
AND as4dict.p__File._Valexp <> "" then 
       _file._Valexp         =     as4dict.p__file._Valexp.

if _file._Valmsg <> as4dict.p__file._Valmsg
AND as4dict.p__File._Valmsg <> "" then 
       _file._Valmsg         =     as4dict.p__file._Valmsg.

if _file._Valmsg-SA <> as4dict.p__file._Valmsg-SA
AND as4dict.p__File._Valmsg-SA <> "" then 
       _file._Valmsg-SA      =     as4dict.p__file._Valmsg-SA. 
                                                      
       
       
