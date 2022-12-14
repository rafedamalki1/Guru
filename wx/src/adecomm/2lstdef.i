/*----------------------------------------------------------------------------

File: 2lstdef.i

Description:
    Includes all the definitions needed to make a shadow pick list work

Arguments
   
Author: David Lee

Date Created: 03/04/93
----------------------------------------------------------------------------*/
/*Copyright (c) by PROGRESS SOFTWARE CORPORATION. 1993 - All Rights Reserved.*/


&IF (DEFINED(include2lstup) = 0)
&THEN

  {adecomm/lstdef1.i}

  DEFINE VARIABLE lst2_index      AS INTEGER NO-UNDO.
  DEFINE VARIABLE lst2_shadow     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lst2_pick_list  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lst2_orig_index AS INTEGER NO-UNDO.

&SCOPED-DEFINE include2lstup 1
&ENDIF
