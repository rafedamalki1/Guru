/*----------------------------------------------------------------------------

File: lstdn1.i

Description:
    Shift an item "down" one in a select list.

Arguments:

   &choice       - THe current selection
   &pick_list    - The list.
   &choice_index - The index position of the user's choice
   
Author: David Lee

Date Created: 03/04/93
----------------------------------------------------------------------------*/
/*Copyright (c) by PROGRESS SOFTWARE CORPORATION. 1993 - All Rights Reserved.*/

DO:
  /* Pull out the selected string from its current slot */ 
  DO lstup_i = 1 TO NUM-ENTRIES({&choice},CHR(3)):
    ASSIGN
      ENTRY(LOOKUP(ENTRY(lstup_i,{&choice},CHR(3)),{&pick_list},CHR(3)),
        {&pick_list},CHR(3)) = ""
      {&pick_list} = REPLACE({&pick_list},CHR(3) + CHR(3),CHR(3)).
  END.

  /* And now add the chosen item back into the list */
  ASSIGN
    {&pick_list} = TRIM({&pick_list},CHR(3))
    {&choice_index} = MINIMUM({&choice_index},NUM-ENTRIES({&pick_list},CHR(3))).
    
    IF {&choice_index} = 0 THEN
      {&pick_list} = {&choice}.
    ELSE
      ENTRY({&choice_index},{&pick_list},CHR(3)) =
        ENTRY({&choice_index},{&pick_list},CHR(3)) + CHR(3) + {&choice}.
END.

/* lstdn.i - end of file */

