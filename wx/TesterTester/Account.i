    DEFINE TEMP-TABLE eAccountEntity FIELD AccountEntityField AS INTEGER INDEX AccountEntityIndex IS UNIQUE PRIMARY AccountEntityField.
    DEFINE DATASET dsAccountEntity FOR eAccountEntity.
    DEFINE DATA-SOURCE srcAccountEntity FOR eAccountEntity.