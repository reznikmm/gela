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

package body Meta.Writes is

   use type League.Strings.Universal_String;

   function "+" (T : Wide_Wide_String) return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   function Is_Element
     (Name : League.Strings.Universal_String) return Boolean is
       (Name.Starts_With ("Element"));

   procedure Write_Header (Output : Ada.Wide_Wide_Text_IO.File_Type);

   procedure Open_File
     (Output : out Ada.Wide_Wide_Text_IO.File_Type;
      Unit   : League.Strings.Universal_String;
      Spec   : Boolean := True);

   function Get_With_Clauses
     (F          : access Ada_Pretty.Factory;
      Vector     : Meta.Read.Class_Vectors.Vector;
      Is_Limited : Boolean) return Ada_Pretty.Node_Access;

   ----------------------
   -- Get_With_Clauses --
   ----------------------

   function Get_With_Clauses
     (F          : access Ada_Pretty.Factory;
      Vector     : Meta.Read.Class_Vectors.Vector;
      Is_Limited : Boolean) return Ada_Pretty.Node_Access
   is
      Result : Ada_Pretty.Node_Access;
   begin
      for J in 2 .. Vector.Last_Index loop
         declare
            Item : constant Meta.Classes.Class := Vector (J);
            Name : constant League.Strings.Universal_String := Item.Name;
            Package_Name : constant League.Strings.Universal_String :=
              "Program.Elements." & Name & "s";
            Next : constant Ada_Pretty.Node_Access :=
              F.New_With (F.New_Name (Package_Name), Is_Limited);
         begin
            Result := F.New_List (Result, Next);
         end;
      end loop;

      return Result;
   end Get_With_Clauses;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File
     (Output : out Ada.Wide_Wide_Text_IO.File_Type;
      Unit   : League.Strings.Universal_String;
      Spec   : Boolean := True)
   is
      Name : League.Strings.Universal_String :=
        Unit.To_Lowercase.Split ('.').Join ("-");
   begin
      if Spec then
         Name.Append (".ads");
      else
         Name.Append (".adb");
      end if;

      Ada.Wide_Wide_Text_IO.Create
        (Output, Name => Name.To_UTF_8_String);
      Write_Header (Output);
   end Open_File;

   --------------------
   -- Write_Elements --
   --------------------

   procedure Write_Elements (Vector : Meta.Read.Class_Vectors.Vector) is

      F : aliased Ada_Pretty.Factory;

      function Element_Classifications (Prefix : Ada_Pretty.Node_Access)
        return Ada_Pretty.Node_Access;

      function Element_Casts (Prefix : Ada_Pretty.Node_Access)
        return Ada_Pretty.Node_Access;

      -------------------
      -- Element_Casts --
      -------------------

      function Element_Casts (Prefix : Ada_Pretty.Node_Access)
        return Ada_Pretty.Node_Access
      is
         Result : Ada_Pretty.Node_Access := Prefix;
      begin
         for J in 2 .. Vector.Last_Index loop
            declare
               Item : constant Meta.Classes.Class := Vector (J);

               Name : constant League.Strings.Universal_String :=
                 "To_" & Item.Name;

               Type_Name : constant Ada_Pretty.Node_Access :=
                 F.New_Selected_Name
                 (+"Program.Elements." & Item.Name & "s." &
                    Item.Name & "_Access");

               Funct : constant Ada_Pretty.Node_Access :=
                 F.New_Subprogram_Declaration
                   (F.New_Subprogram_Specification
                      (Name          => F.New_Name (Name),
                       Parameters    => F.New_Parameter
                         (Name            => F.New_Name (+"Self"),
                          Type_Definition => F.New_Access
                            (Is_All => False,
                             Target => F.New_Name (+"Element'Class"))),
                       Result        => Type_Name),
                    Aspects     => F.New_Aspect
                      (Name  => F.New_Name (+"Pre"),
                       Value => F.New_Name ("Self.Is_" & Item.Name)));
            begin
               Result := F.New_List (Result, Funct);
            end;
         end loop;

         return Result;
      end Element_Casts;

      -----------------------------
      -- Element_Classifications --
      -----------------------------

      function Element_Classifications (Prefix : Ada_Pretty.Node_Access)
        return Ada_Pretty.Node_Access
      is
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
        Element_Casts (Element_Classifications (Public_Part_Prefix));

      Root : constant Ada_Pretty.Node_Access :=
        F.New_Package (Program_Elements, Public_Part);

      Unit : constant Ada_Pretty.Node_Access :=
        F.New_Compilation_Unit
          (Root,
           Get_With_Clauses (F'Access, Vector, Is_Limited => True));

      Output : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Open_File (Output, +"Program.Elements");

      Ada.Wide_Wide_Text_IO.Put_Line
        (Output,
         F.To_Text (Unit).Join (Ada.Characters.Wide_Wide_Latin_1.LF)
         .To_Wide_Wide_String);

      Ada.Wide_Wide_Text_IO.Close (Output);
   end Write_Elements;

   -------------------------
   -- Write_Elements_Body --
   -------------------------

   procedure Write_Elements_Body (Vector : Meta.Read.Class_Vectors.Vector) is
      F : aliased Ada_Pretty.Factory;

      function Element_Casts (Prefix : Ada_Pretty.Node_Access)
        return Ada_Pretty.Node_Access;

      -------------------
      -- Element_Casts --
      -------------------

      function Element_Casts (Prefix : Ada_Pretty.Node_Access)
        return Ada_Pretty.Node_Access
      is
         Result : Ada_Pretty.Node_Access := Prefix;
      begin
         for J in 2 .. Vector.Last_Index loop
            declare
               Item : constant Meta.Classes.Class := Vector (J);

               Name : constant League.Strings.Universal_String :=
                 "To_" & Item.Name;

               Type_Name : constant Ada_Pretty.Node_Access :=
                 F.New_Selected_Name
                 (+"Program.Elements." & Item.Name & "s." &
                    Item.Name & "_Access");

               Stmt  : constant Ada_Pretty.Node_Access :=
                 F.New_Return
                   (F.New_Apply
                      (Prefix    => Type_Name,
                       Arguments => F.New_Name (+"Self")));

               Funct : constant Ada_Pretty.Node_Access :=
                 F.New_Subprogram_Body
                   (F.New_Subprogram_Specification
                      (Name          => F.New_Name (Name),
                       Parameters    => F.New_Parameter
                         (Name            => F.New_Name (+"Self"),
                          Type_Definition => F.New_Access
                            (Is_All => False,
                             Target => F.New_Name (+"Element'Class"))),
                       Result        => Type_Name),
                    Statements => Stmt);
            begin
               Result := F.New_List (Result, Funct);
            end;
         end loop;

         return Result;
      end Element_Casts;

      Program_Elements : constant Ada_Pretty.Node_Access :=
        F.New_Selected_Name (+"Program.Elements");

      Root : constant Ada_Pretty.Node_Access :=
        F.New_Package_Body (Program_Elements, Element_Casts (null));

      Unit : constant Ada_Pretty.Node_Access :=
        F.New_Compilation_Unit
          (Root,
           Get_With_Clauses (F'Access, Vector, Is_Limited => False));

      Output : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Open_File (Output, +"Program.Elements", Spec => False);

      Ada.Wide_Wide_Text_IO.Put_Line
        (Output,
         F.To_Text (Unit).Join (Ada.Characters.Wide_Wide_Latin_1.LF)
         .To_Wide_Wide_String);

      Ada.Wide_Wide_Text_IO.Close (Output);
   end Write_Elements_Body;

   ------------------
   -- Write_Header --
   ------------------

   procedure Write_Header (Output : Ada.Wide_Wide_Text_IO.File_Type) is
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        (Output, "--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>");

      Ada.Wide_Wide_Text_IO.Put_Line (Output, "--");

      Ada.Wide_Wide_Text_IO.Put_Line
        (Output, "--  SPDX-License-Identifier: MIT");

      Ada.Wide_Wide_Text_IO.Put_Line
        (Output, "--  License-Filename: LICENSE");

      Ada.Wide_Wide_Text_IO.Put_Line
        (Output,
         "-------------------------------------------------------------");
      Ada.Wide_Wide_Text_IO.New_Line (Output);
   end Write_Header;

   ------------------------
   -- Write_One_Elements --
   ------------------------

   procedure Write_One_Elements (Item : Meta.Classes.Class) is

      F : aliased Ada_Pretty.Factory;

      function Get_Clauses return Ada_Pretty.Node_Access;
      function Get_Parents return Ada_Pretty.Node_Access;

      Program_Elements : constant Wide_Wide_String := "Program.Elements.";

      -----------------
      -- Get_Clauses --
      -----------------

      function Get_Clauses return Ada_Pretty.Node_Access is
         Parents : constant League.String_Vectors.Universal_String_Vector :=
           Item.Parents;
         Parent  : League.Strings.Universal_String;
         Result  : Ada_Pretty.Node_Access;
         Each    : Ada_Pretty.Node_Access;
      begin
         if Is_Element (Parents.Element (1)) then
            return null;
         end if;

         for J in 1 .. Parents.Length loop
            Parent := Parents (J);
            Each := F.New_With
              (F.New_Selected_Name (Program_Elements & Parent & "s"));
            Result := F.New_List (Result, Each);
         end loop;

         return Result;
      end Get_Clauses;

      -----------------
      -- Get_Parents --
      -----------------

      function Get_Parents return Ada_Pretty.Node_Access is
         Parents : constant League.String_Vectors.Universal_String_Vector :=
           Item.Parents;
         Parent  : League.Strings.Universal_String;
         Result  : Ada_Pretty.Node_Access;
         Each    : Ada_Pretty.Node_Access;
      begin
         for J in 1 .. Parents.Length loop
            Parent := Parents (J);

            if Is_Element (Parent) then
               Parent := +"Program.Elements.Element";
            else
               Parent := Program_Elements & Parent & "s." & Parent;
            end if;

            Each := F.New_Infix
              (+"and",
               F.New_Selected_Name (Parent));

            Result := F.New_List (Result, Each);
         end loop;

         return Result;
      end Get_Parents;

      Package_Name : constant League.Strings.Universal_String :=
        Program_Elements & Item.Name & "s";

      Element_Name : constant League.Strings.Universal_String :=
        (if Item.Name = +"Pragma" then +"Pragma_Element" else Item.Name);

      Type_Name : constant Ada_Pretty.Node_Access := F.New_Name (Element_Name);

      Type_Class  : constant Ada_Pretty.Node_Access :=
        F.New_Name (Element_Name & "'Class");

      Access_Name : constant Ada_Pretty.Node_Access :=
        F.New_Name (Item.Name & "_Access");

      Element_Access : constant Ada_Pretty.Node_Access :=
        F.New_Type
          (Access_Name,
           Definition => F.New_Access
             (Is_All => True,
              Target => Type_Class),
           Aspects => F.New_Aspect
             (Name  => F.New_Name (+"Storage_Size"),
              Value => F.New_Literal (0)));

      Pure : constant Ada_Pretty.Node_Access :=
        F.New_Pragma
          (F.New_Name (+"Pure"), F.New_Selected_Name (Package_Name));

      Type_Decl : constant Ada_Pretty.Node_Access :=
        F.New_Type
          (Type_Name,
           Definition => F.New_Interface
             (Is_Limited => True,
              Parents    => Get_Parents));

      Public_Part : constant Ada_Pretty.Node_Access :=
        F.New_List ((Pure, Type_Decl, Element_Access));

      Root : constant Ada_Pretty.Node_Access :=
        F.New_Package (F.New_Selected_Name (Package_Name), Public_Part);

      Unit : constant Ada_Pretty.Node_Access :=
        F.New_Compilation_Unit (Root, Get_Clauses);

      Output : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Open_File (Output, Package_Name);

      Ada.Wide_Wide_Text_IO.Put_Line
        (Output,
         F.To_Text (Unit).Join (Ada.Characters.Wide_Wide_Latin_1.LF)
         .To_Wide_Wide_String);

      Ada.Wide_Wide_Text_IO.Close (Output);
   end Write_One_Elements;

end Meta.Writes;
