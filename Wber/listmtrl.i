/*LISTMTRL.I DEFINITIONER TEMP-TABLE list_mtrl BERMTRL*/
   


DEFINE {&NEW} {&SHARED} TEMP-TABLE list_mtrl NO-UNDO 
   {LISTMTRLTT.I}
    
DEFINE TEMP-TABLE elist_mtrl NO-UNDO LIKE list_mtrl.
DEFINE TEMP-TABLE sklist_mtrl NO-UNDO LIKE list_mtrl.
DEFINE TEMP-TABLE ejlist_mtrl NO-UNDO LIKE list_mtrl.
DEFINE TEMP-TABLE eimlist_mtrl NO-UNDO LIKE list_mtrl.
 
   
