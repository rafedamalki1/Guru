 
 /*------------------------------------------------------------------------
    File        : ArrayList
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Mon May 23 13:44:28 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Controls.ArrayList: 
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
   DEFINE PRIVATE VARIABLE mainArray AS CLASS Progress.Lang.Object EXTENT NO-UNDO.
   DEFINE PRIVATE VARIABLE mainArrayLast AS INTEGER NO-UNDO INITIAL 0.
   DEFINE PRIVATE VARIABLE mainArraySize AS INTEGER NO-UNDO INITIAL 0.
   
		
	CONSTRUCTOR PUBLIC ArrayList (  ):
		SUPER ().
		
	END CONSTRUCTOR.
      
   METHOD PRIVATE VOID ensureCapacity(INPUT capacity AS INTEGER):
      DEFINE VARIABLE i        AS INTEGER NO-UNDO.
      DEFINE VARIABLE newSize  AS INTEGER NO-UNDO.
      DEFINE VARIABLE newArray AS CLASS   Progress.Lang.Object EXTENT NO-UNDO.
      IF capacity > mainArraySize THEN 
      DO:
         newSize = 2 * capacity + 10.
         EXTENT(newArray) = newSize.
         DO i = 1 TO mainArrayLast:
            newArray[i] = mainArray[i].
         END.
         EXTENT(mainArray) = ?.
         mainArray = newArray.
         mainArraySize = newSize.
      END.
   END METHOD.
   
   METHOD PUBLIC VOID ensureAvailable(INPUT howMuch AS INTEGER):
      ensureCapacity(mainArrayLast + howMuch).
   END METHOD.

/*   METHOD PUBLIC VOID append(INPUT element AS CLASS Progress.Lang.Object):*/
/*      ensureAvailable(1).                                                 */
/*      mainArrayLast = mainArrayLast + 1.                                  */
/*      mainArray[mainArrayLast] = element.                                 */
/*   END METHOD.                                                            */

   METHOD PUBLIC VOID AddObj(obj AS Progress.Lang.Object):
      ensureAvailable(1).
      mainArrayLast = mainArrayLast + 1.
      mainArray[mainArrayLast] = obj.      
   END METHOD.
         
   METHOD PUBLIC Progress.Lang.Object GetObj(pos AS INTEGER):
      DEFINE VARIABLE obj AS Progress.Lang.Object NO-UNDO.
      
      obj = mainArray[pos].
      
      RETURN obj.      
   END METHOD. 
   
   METHOD PUBLIC VOID RemoveAt(pos AS INTEGER):
   END METHOD.
         
   METHOD PUBLIC VOID ClearAll():
      
   END METHOD.

	DESTRUCTOR PUBLIC ArrayList ( ):

	END DESTRUCTOR.
   


END CLASS.