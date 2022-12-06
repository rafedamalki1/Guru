/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*  Copied from prodict/dump/copy_fld.i and modified for the p__field fields 
     Used in the PROGRESS/400 Dictionary 
     Modified 1/18/95 - D McMann
*/

IF {&all} THEN
  ASSIGN
    {&to}._Field-Name    = {&from}._Field-Name
    {&to}._File-recid    = {&from}._File-recid   
    {&to}._Fld-number = {&from}._Fld-number
    {&to}._Data-Type     = {&from}._Data-Type
    {&to}._dtype         = {&from}._dtype
    {&to}._Format        = {&from}._Format
    {&to}._Initial       = {&from}._Initial
    {&to}._Order         = {&from}._Order
    {&to}._field-rpos    = {&from}._field-rpos
    {&to}._sys-field     = {&from}._sys-field.
    
ASSIGN
  {&to}._Can-Read      = {&from}._Can-Read
  {&to}._Can-Write     = {&from}._Can-Write
  {&to}._Col-label     = {&from}._Col-label
  {&to}._Col-label-SA  = {&from}._Col-label-SA
  {&to}._Decimals      = {&from}._Decimals
  {&to}._Desc          = {&from}._Desc
  {&to}._Extent        = {&from}._Extent
  {&to}._Fld-case      = {&from}._Fld-case
  {&to}._Format-SA     = {&from}._Format-SA
  {&to}._Help          = {&from}._Help
  {&to}._Help-SA       = {&from}._Help-SA
  {&to}._Initial-SA    = {&from}._Initial-SA
  {&to}._Label         = {&from}._Label
  {&to}._Label-SA      = {&from}._Label-SA
  {&to}._Mandatory     = {&from}._Mandatory
  {&to}._Valexp        = {&from}._Valexp
  {&to}._Valmsg        = {&from}._Valmsg
  {&to}._Valmsg-SA     = {&from}._Valmsg-SA
  {&to}._View-As       = {&from}._View-As

  {&to}._Fld-stdtype   = {&from}._Fld-stdtype
  {&to}._Fld-stlen     = {&from}._Fld-stlen

  {&to}._For-Allocated = {&from}._For-Allocated
  {&to}._For-Itype     = {&from}._For-Itype
  {&to}._For-Maxsize   = {&from}._For-Maxsize
  {&to}._For-Name      = {&from}._For-Name
  {&to}._For-Retrieve  = {&from}._For-Retrieve
  {&to}._For-Scale     = {&from}._For-Scale
  {&to}._For-Separator = {&from}._For-Separator
  {&to}._For-Spacing   = {&from}._For-Spacing
  {&to}._For-Type      = {&from}._For-Type
  {&to}._For-Xpos      = {&from}._For-Xpos

  {&to}._Fld-misc1[1]  = {&from}._Fld-misc1[1]
  {&to}._Fld-misc1[2]  = {&from}._Fld-misc1[2]
  {&to}._Fld-misc1[3]  = {&from}._Fld-misc1[3]
  {&to}._Fld-misc1[4]  = {&from}._Fld-misc1[4]
  {&to}._Fld-misc1[5]  = {&from}._Fld-misc1[5]  
  {&to}._Fld-misc1[6]  = {&from}._Fld-misc1[6]
  {&to}._Fld-misc2[2]  = {&from}._Fld-misc2[2] 
  {&to}._Fld-misc2[3]  = {&from}._Fld-misc2[3]
  {&to}._Fld-misc2[5]  = {&from}._Fld-misc2[5]  
  {&to}._Fld-misc2[6]  = {&from}._Fld-misc2[6]       /* added for AS400  */
  {&to}._Fld-misc2[4]  = {&from}._Fld-misc2[4].
