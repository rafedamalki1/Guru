/* p-block1.p */

FOR EACH Customer FIELDS (Balance):
    DEFINE VARIABLE balance-sum AS DECIMAL INITIAL 0.
    balance-sum = balance-sum + Balance.
END.

MESSAGE "Corporate Receivables Owed:" 
        STRING(balance-sum, "$>,>>>,>>>,>>9.99") VIEW-AS ALERT-BOX.