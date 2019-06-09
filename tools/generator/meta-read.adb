--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with League.String_Vectors;

package body Meta.Read is

   function To_Name (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   --------------
   -- Read_AST --
   --------------

   procedure Read_AST
     (File_Name : League.Strings.Universal_String;
      Result    : out Class_Vectors.Vector)
   is
      Input : Ada.Wide_Wide_Text_IO.File_Type;
      Class : Meta.Classes.Class;
   begin
      Class.Initialize (League.Strings.Empty_Universal_String);

      Ada.Wide_Wide_Text_IO.Open
        (Input,
         Ada.Wide_Wide_Text_IO.In_File,
         File_Name.To_UTF_8_String);

      while not Ada.Wide_Wide_Text_IO.End_Of_File (Input) loop
         declare
            Line : constant League.Strings.Universal_String :=
              League.Strings.To_Universal_String
                (Ada.Wide_Wide_Text_IO.Get_Line (Input));
            List : constant League.String_Vectors.Universal_String_Vector :=
              Line.Split (' ', League.Strings.Skip_Empty);
         begin
            if Line.Starts_With ("##") then
               if not Class.Name.Is_Empty then
                  Result.Append (Class);
               end if;

               Class.Initialize
                 (Name        => To_Name (List (2)),
                  Is_Abstract => List.Element (2).Starts_With ("_"));
            elsif Line.Starts_With ("> ") then
               declare
                  Parents : constant
                    League.String_Vectors.Universal_String_Vector :=
                      List (2).Split (',');
               begin
                  for J in 1 .. Parents.Length loop
                     Class.Add_Parent (To_Name (Parents (J)));
                  end loop;
               end;
            elsif Line.Starts_With (" - ") then
               declare
                  Name : constant League.Strings.Universal_String := List (2);
                  Tipe : League.Strings.Universal_String := List (4);

                  Capacity  : Meta.Classes.Capacity_Kind :=
                    Meta.Classes.Just_One;
               begin
                  if Tipe.Ends_With ("?") then
                     Tipe := Tipe.Head_To (Tipe.Length - 1);
                     Capacity := Meta.Classes.Zero_Or_One;
                  elsif Tipe.Ends_With ("+") then
                     Tipe := Tipe.Head_To (Tipe.Length - 1);
                     Capacity := Meta.Classes.One_Or_More;
                  elsif Tipe.Ends_With ("*") then
                     Tipe := Tipe.Head_To (Tipe.Length - 1);
                     Capacity := Meta.Classes.Zero_Or_More;
                  end if;

                  Class.Add_Property
                    ((Name => Name,
                      Type_Name => To_Name (Tipe),
                      Capacity => Capacity));
               end;
            end if;
         end;
      end loop;

      Result.Append (Class);

      Ada.Wide_Wide_Text_IO.Close (Input);
   end Read_AST;

   -------------
   -- To_Name --
   -------------

   function To_Name (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String := Text;
   begin
      if Result.Starts_With ("_") then
         Result := Result.Tail_From (2);
      end if;

      if Result.Ends_With ("_") then
         Result := Result.Head_To (Result.Length - 1);
      end if;

      if Result.Starts_With ("[") then
         Result := Result.Tail_From (2);
      end if;

      if Result.Ends_With ("]") then
         Result := Result.Head_To (Result.Length - 1);
      end if;

      return Result;
   end To_Name;

end Meta.Read;
