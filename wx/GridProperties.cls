 
 /*------------------------------------------------------------------------
    File        : GridProperties
    Purpose     : 
    Syntax      : ANVÄNDS INTE???
    Description : 
    Author(s)   : elpkl
    Created     : Thu May 19 16:13:02 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Controls.GridProperties: 

/*WHERE...*/
DEFINE PUBLIC PROPERTY filter AS CHARACTER
PUBLIC GET.
PUBLIC SET.

/*Pekare till TEMP-TABLE:HANDLE*/
DEFINE PUBLIC PROPERTY ttBufferHandle AS HANDLE
PUBLIC GET.
PUBLIC SET.

DEFINE PUBLIC VARIABLE RubrikLista AS Controls.GridRubrikLista.



END CLASS.