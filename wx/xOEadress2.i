
DEF VAR chNamespace AS COM-HANDLE NO-UNDO.
DEF VAR chOutlook AS COM-HANDLE NO-UNDO.
DEF VAR chFolder AS COM-HANDLE NO-UNDO.
DEF VAR chAddressEntry AS COM-HANDLE NO-UNDO.
DEF VAR chAddressList AS COM-HANDLE NO-UNDO.
DEF VAR chAddressLists AS COM-HANDLE NO-UNDO.
DEF VAR chAddressEntries AS COM-HANDLE NO-UNDO.
def var name as char no-undo.
def var name2 as char no-undo.
def var surname as char no-undo.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE adress AS CHARACTER NO-UNDO.

CREATE "Outlook.application.9" chOutlook.
ASSIGN
chNameSpace = chOutlook:GetNameSpace("MAPI":U).
chFolder = chNameSpace:GetDefaultFolder(6).
ChAddressLists = chNameSpace:AddressLists.
ChAddressList = chAddressLists:Item(1).
ChAddressEntries = ChAddressList:AddressEntries.
i = 1.


REPEAT WHILE i <= chAddressEntries:Count :
   ASSIGN chAddressEntry = chAddressEntries:Item(i).
   
   IF NUM-ENTRIES(chAddressEntry:Name,' ') = 1 THEN
   ASSIGN
   surname = chAddressEntry:Name
   name = "":U
   name2 = "":U.
   IF NUM-ENTRIES(chAddressEntry:Name,' ') = 2 THEN
   ASSIGN
   surname = ENTRY(2,chAddressEntry:Name,' ')
   name = ENTRY(1,chAddressEntry:Name,' ')
   name2 = "":U.
   IF NUM-ENTRIES(chAddressEntry:Name,' ') = 3 THEN
   ASSIGN
   surname = ENTRY(3,chAddressEntry:Name,' ')
   name = ENTRY(1,chAddressEntry:Name,' ')
   name2 = ENTRY(2,chAddressEntry:Name,' ').
   ASSIGN 
   adress = chAddressEntry:Address.
   MESSAGE adress VIEW-AS ALERT-BOX.
   i = i + 1.
END.

