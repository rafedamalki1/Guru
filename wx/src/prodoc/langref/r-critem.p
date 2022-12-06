TRIGGER PROCEDURE FOR Create OF Item.

/* Automatically assign a unique item number using Next-Item-Num Sequence */

ASSIGN Item.Item-Num =
   NEXT-VALUE(Next-Item-Num).


