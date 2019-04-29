--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Application;
with League.String_Vectors;

with Meta.Read;
with Meta.Writes;

procedure Meta.Run is
   Class_List : Meta.Read.Class_Vectors.Vector;
begin
   Meta.Read.Read_AST
     (File_Name => League.Application.Arguments.Element (1),
      Result    => Class_List);

   Meta.Writes.Write_Elements (Class_List);
   Meta.Writes.Write_Elements_Body (Class_List);

   for J in 2 .. Class_List.Last_Index loop
      Meta.Writes.Write_One_Elements (Class_List (J));
   end loop;
end Meta.Run;
