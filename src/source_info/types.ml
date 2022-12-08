type 'a with_pos = 'a * (int * int)

type jmp_to_def = Occurence of string | Def of string

type info = Token of Parser.token | Line of {absolute:int; relative:(int * string) option} | Local_jmp of jmp_to_def

type infos = info with_pos list
