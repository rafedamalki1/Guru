/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* as4dict/load/loadkill.i - delete a file definition 

   Modified 95/01/31 to work with DB2/400 Utilities  
*/

FOR EACH as4dict.p__Index WHERE as4dict.p__Index._File-number = as4dict.p__File._File-number:
  FOR EACH as4dict.p__Idxfd WHERE as4dict.p__Idxfd._Idx-num =  as4dict.p__Index._Idx-num
                                                                AND as4dict.p__Idxfd._File-number = as4dict.p__Index._File-number:
    DELETE as4dict.p__Idxfd.
  END.
  DELETE as4dict.p__Index.
END.
FOR EACH as4dict.p__Trgfl WHERE as4dict.p__Trgfl._File-number = as4dict.p__File._File-number:
  DELETE as4dict.p__Trgfl.
END.
FOR EACH as4dict.p__Field WHERE as4dict.p__Field._File-number = as4dict.p__File._File-number:
  FOR EACH as4dict.p__Trgfd WHERE as4dict.p__Trgfd._File-number = as4dict.p__Field._File-number
                                                                 AND as4dict.p__Trgfd._Fld-number = as4dict.p__Field._File-number:
    DELETE as4dict.p__Trgfd.
  END.
  DELETE as4dict.p__Field.
END.
DELETE as4dict.p__File.
