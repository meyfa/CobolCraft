*> --- Copybook: crafting recipes ---

78 RECIPES-SHAPELESS-CAPACITY VALUE 1000.
78 RECIPES-SHAPED-CAPACITY VALUE 1000.
78 RECIPES-COMPLEX-CAPACITY VALUE 100.

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

    02 RECIPES-SHAPED.
        03 RECIPES-SHAPED-COUNT BINARY-LONG UNSIGNED.
        *> Shaped recipes store their ingredients in a 3x3 grid, with the top-left corner being the origin. Slots are
        *> enumerated from left to right, top to bottom. Recipes smaller than 3x3 (2x2, 1x2, 2x3, etc.) are padded with
        *> zeroes to the right and bottom.
        03 RECIPE-SHAPED OCCURS RECIPES-SHAPED-CAPACITY TIMES.
            04 RECIPE-SHAPED-INPUTS.
                05 RECIPE-SHAPED-INPUT OCCURS 9 TIMES BINARY-LONG UNSIGNED.
            04 RECIPE-SHAPED-OUTPUT.
                05 RECIPE-SHAPED-OUTPUT-ID BINARY-LONG UNSIGNED.
                05 RECIPE-SHAPED-OUTPUT-COUNT BINARY-LONG UNSIGNED.

    *> Some shaped recipes have too many combinatorial variations to store them all. For these, we cannot use the fast
    *> lookup table above (with one entry per recipe), but must instead store the recipe as a list of options per slot.
    *> For example, with 12 wood types, there are 20,736 ways to make a crafting table, or 429,981,696 for a barrel.
    02 RECIPES-COMPLEX.
        03 RECIPES-COMPLEX-COUNT BINARY-LONG UNSIGNED.
        03 RECIPE-COMPLEX OCCURS RECIPES-COMPLEX-CAPACITY TIMES.
            04 RECIPE-COMPLEX-INPUTS.
                05 RECIPE-COMPLEX-INPUT OCCURS 9 TIMES.
                    06 RECIPE-COMPLEX-INPUT-OPTIONS BINARY-LONG UNSIGNED.
                    06 RECIPE-COMPLEX-INPUT-ID OCCURS 128 TIMES BINARY-LONG UNSIGNED.
            04 RECIPE-COMPLEX-OUTPUT.
                05 RECIPE-COMPLEX-OUTPUT-ID BINARY-LONG UNSIGNED.
                05 RECIPE-COMPLEX-OUTPUT-COUNT BINARY-LONG UNSIGNED.
