/*----------------------------------------------------------------------------

File: lstdef1.i

Description:
    Includes all the definitions needed for a choice in a single pick list
    to be moved up or down one slot

Arguments
   
Author: David Lee

Date Created: 03/04/93
----------------------------------------------------------------------------*/
/*Copyright (c) by PROGRESS SOFTWARE CORPORATION. 1993 - All Rights Reserved.*/


&IF (DEFINED(lstdef1) = 0)
&THEN

DEFINE VARIABLE lst_choice       AS CHARACTER NO-UNDO. /* value from widget */
DEFINE VARIABLE lst_pick_list    AS CHARACTER NO-UNDO. /* list in widget */
DEFINE VARIABLE lst_index        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lstup_i          AS INTEGER   NO-UNDO.

&SCOPED-DEFINE lstdef1 1
&ENDIF

