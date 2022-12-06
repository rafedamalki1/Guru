/* e-clpmul.p */

CLIPBOARD:MULTIPLE = TRUE.
CLIPBOARD:ITEMS-PER-ROW = 11.

FOR EACH Customer:
    CLIPBOARD:VALUE = STRING(Cust-Num).
    CLIPBOARD:VALUE = Name.
    CLIPBOARD:VALUE = Address.
    CLIPBOARD:VALUE = Address2.
    CLIPBOARD:VALUE = City.
    CLIPBOARD:VALUE = State.
    CLIPBOARD:VALUE = Postal-Code.
    CLIPBOARD:VALUE = Country.
    CLIPBOARD:VALUE = Phone.
    CLIPBOARD:VALUE = STRING(Balance).
    CLIPBOARD:VALUE = STRING(Credit-Limit).
END.

CLIPBOARD:MULTIPLE = FALSE.