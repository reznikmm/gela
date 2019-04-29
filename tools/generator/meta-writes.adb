--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Wide_Wide_Text_IO;
with Ada_Pretty;
with League.String_Vectors;
with League.Strings;
with Meta.Classes;

package body Meta.Writes is

   function "+" (T : Wide_Wide_String) return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   procedure Write_Header;

   --------------------
   -- Write_Elements --
   --------------------

   procedure Write_Elements (Vector : Meta.Read.Class_Vectors.Vector) is
      F : aliased Ada_Pretty.Factory;

      function Element_Classifications (Prefix : Ada_Pretty.Node_Access)
        return Ada_Pretty.Node_Access;

      function Is_Element
        (Name : League.Strings.Universal_String) return Boolean is
          (Name.Starts_With ("Element"));

      -----------------------------
      -- Element_Classifications --
      -----------------------------

      function Element_Classifications (Prefix : Ada_Pretty.Node_Access)
        return Ada_Pretty.Node_Access
      is
         use type League.Strings.Universal_String;

         function Get_Aspect
           (Item : Meta.Classes.Class) return Ada_Pretty.Node_Access;

         ----------------
         -- Get_Aspect --
         ----------------

         function Get_Aspect
           (Item : Meta.Classes.Class) return Ada_Pretty.Node_Access
         is
            use type Ada_Pretty.Node_Access;
            Name : constant League.Strings.Universal_String :=
              "Is_" & Item.Name;
            List : Ada_Pretty.Node_Access;
            Value : Ada_Pretty.Node_Access;
            Parents : constant League.String_Vectors.Universal_String_Vector :=
              Item.Parents;
         begin
            if Is_Element (Parents.Element (1)) then
               return null;
            end if;

            for J in 1 .. Parents.Length loop
               Value := F.New_Selected_Name
                 (+"Self.Is_" & Parents.Element (J));

               if List = null then
                  List := Value;
               else
                  List := F.New_List (List, F.New_Infix (+"or", Value));
               end if;
            end loop;

            return F.New_Aspect
              (Name  => F.New_Name (+"Post'Class"),
               Value => F.New_Parentheses
                 (F.New_If_Expression
                      (Condition  => F.New_Name (Name & "'Result"),
                       Then_Path  => List)));
         end Get_Aspect;

         Result : Ada_Pretty.Node_Access := Prefix;
      begin
         for J in 2 .. Vector.Last_Index loop
            declare
               Item : constant Meta.Classes.Class := Vector (J);

               Name : constant League.Strings.Universal_String :=
                 "Is_" & Item.Name;

               Funct : constant Ada_Pretty.Node_Access :=
                 F.New_Subprogram_Declaration
                   (F.New_Subprogram_Specification
                      (Is_Overriding => Ada_Pretty.False,
                       Name          => F.New_Name (Name),
                       Parameters    => F.New_Parameter
                         (Name            => F.New_Name (+"Self"),
                          Type_Definition => F.New_Name (+"Element")),
                       Result        => F.New_Name (+"Boolean")),
                    Aspects     => Get_Aspect (Item),
                    Is_Abstract => True);
            begin
               Result := F.New_List (Result, Funct);
            end;
         end loop;

         return Result;
      end Element_Classifications;

      Program_Elements : constant Ada_Pretty.Node_Access :=
        F.New_Selected_Name (+"Program.Elements");
      Pure : constant Ada_Pretty.Node_Access :=
        F.New_Pragma (F.New_Name (+"Pure"), Program_Elements);

      Element_Name : constant Ada_Pretty.Node_Access :=
        F.New_Name (+"Element");
      Element_Decl : constant Ada_Pretty.Node_Access :=
        F.New_Type
          (Element_Name,
           Definition => F.New_Interface (Is_Limited => True),
           Comment    => +("Element is a representation of a program " &
                 "construct with corresponding syntactic and semantic " &
                 "information available from the Ada environment."));

      Element_Class  : constant Ada_Pretty.Node_Access :=
        F.New_Name (+"Element'Class");
      Element_Access : constant Ada_Pretty.Node_Access :=
        F.New_Type
          (F.New_Name (+"Element_Access"),
           Definition => F.New_Access
             (Is_All => True,
              Target => Element_Class),
           Aspects => F.New_Aspect
             (Name  => F.New_Name (+"Storage_Size"),
              Value => F.New_Literal (0)));

      Public_Part_Prefix : constant Ada_Pretty.Node_Access :=
        F.New_List ((Pure, Element_Decl, Element_Access));
      Public_Part : constant Ada_Pretty.Node_Access :=
        Element_Classifications (Public_Part_Prefix);
      Root : constant Ada_Pretty.Node_Access :=
        F.New_Package (Program_Elements, Public_Part);
      Unit : constant Ada_Pretty.Node_Access := F.New_Compilation_Unit (Root);
   begin
      Write_Header;
      Ada.Wide_Wide_Text_IO.Put_Line
        (F.To_Text (Unit)
         .Join (Ada.Characters.Wide_Wide_Latin_1.LF)
         .To_Wide_Wide_String);
   end Write_Elements;

   procedure Write_Header is
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        ("--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>");
      Ada.Wide_Wide_Text_IO.Put_Line ("--");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("--  SPDX-License-Identifier: MIT");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("--  License-Filename: LICENSE");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("-------------------------------------------------------------");
      Ada.Wide_Wide_Text_IO.New_Line;
   end Write_Header;

end Meta.Writes;
