*> --- Copybook: command parser registry names and property constants ---
*> See: https://minecraft.wiki/w/Minecraft_Wiki:Projects/wiki.vg_merge/Command_Data#Parsers
*> The numeric IDs can be obtained by looking at the "minecraft:command_argument_type" registry.

*> Note: When adding something here, update AddCommandArgument to verify the properties.

78 CMD-PARSER-STRING            VALUE "brigadier:string".
    78 CMD-STRING-SINGLE            VALUE X"00".
    78 CMD-STRING-QUOTABLE-PHRASE   VALUE X"01".
    78 CMD-STRING-GREEDY            VALUE X"02".

78 CMD-PARSER-GAME-PROFILE      VALUE "minecraft:game_profile".

78 CMD-PARSER-GAMEMODE          VALUE "minecraft:gamemode".
