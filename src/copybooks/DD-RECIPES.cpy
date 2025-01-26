*> --- Copybook: crafting recipes ---

78 RECIPES-SHAPELESS-CAPACITY VALUE 1000.

01 RECIPES EXTERNAL.
    02 RECIPES-SHAPELESS.
        03 RECIPES-SHAPELESS-COUNT BINARY-LONG UNSIGNED.
        *> For efficient lookup, shapeless recipes store their ingredients sorted by item protocol ID (descending),
        *> where ingredients occuring multiple times are repeated, and everything is padded with trailing zeroes.
        03 RECIPE-SHAPELESS OCCURS RECIPES-SHAPELESS-CAPACITY TIMES.
            04 RECIPE-SHAPELESS-INPUTS.
                05 RECIPE-SHAPELESS-INPUT OCCURS 9 TIMES BINARY-LONG UNSIGNED.
            04 RECIPE-SHAPELESS-OUTPUT.
                05 RECIPE-SHAPELESS-OUTPUT-ID BINARY-LONG UNSIGNED.
                05 RECIPE-SHAPELESS-OUTPUT-COUNT BINARY-LONG UNSIGNED.
