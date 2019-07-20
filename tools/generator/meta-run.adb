--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Application;
with League.String_Vectors;

with Meta.Classes;
with Meta.Read;
with Meta.Writes;

procedure Meta.Run is
   Class_List : Meta.Read.Class_Vectors.Vector;
   Lists      : League.String_Vectors.Universal_String_Vector;
begin
   Meta.Read.Read_AST
     (File_Name => League.Application.Arguments.Element (1),
      Result    => Class_List);

   for J of Class_List loop
      for P of J.Properties loop
         if P.Capacity in
             Meta.Classes.Zero_Or_More .. Meta.Classes.One_Or_More
           and then Lists.Index (P.Type_Name) = 0
         then
            Lists.Append (P.Type_Name);
         end if;
      end loop;
   end loop;

   Meta.Writes.Write_Elements (Class_List);
   Meta.Writes.Write_Elements_Body (Class_List);
   Meta.Writes.Write_Visitors (Class_List);
   Meta.Writes.Write_Iterators (Class_List);
   Meta.Writes.Write_Factories (Class_List);
   Meta.Writes.Write_Factories (Class_List, Implicit => True);
   Meta.Writes.Write_Factories_Body (Class_List);
   Meta.Writes.Write_Factories_Body (Class_List, Implicit => True);

   for J in 2 .. Class_List.Last_Index loop
      Meta.Writes.Write_One_Element
        (Class_List (J),
         With_List => Lists.Index (Class_List (J).Name) > 0);

      if not Class_List (J).Is_Abstract then
         Meta.Writes.Write_One_Node (Class_List, Class_List (J));
         Meta.Writes.Write_One_Node_Body (Class_List, Class_List (J));
      end if;
   end loop;
end Meta.Run;
