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
   use all type Ada_Pretty.Access_Modifier;

   function "+" (T : Wide_Wide_String) return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   procedure Write_Header (Output : Ada.Wide_Wide_Text_IO.File_Type);

   procedure Open_File
     (Output : out Ada.Wide_Wide_Text_IO.File_Type;
      Unit   : League.Strings.Universal_String;
      Spec   : Boolean := True);

   function Get_With_Clauses
     (F             : access Ada_Pretty.Factory;
      Vector        : Meta.Read.Class_Vectors.Vector;
      Is_Limited    : Boolean;
      With_Abstract : Boolean := True;
      Skip          : Natural := 1) return Ada_Pretty.Node_Access;

   function Get_Package_Name
     (Type_Name : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Full_Record_Name
     (Type_Name : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Full_Access_Name
     (Type_Name : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Full_Vector_Name
     (Type_Name : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function To_Element_Name
     (Type_Name : League.Strings.Universal_String)
      return League.Strings.Universal_String is
        (if Type_Name = +"Pragma" then +"Pragma_Element" else Type_Name);

   Element : constant League.Strings.Universal_String := +"Element";

   Element_Vector : constant League.Strings.Universal_String :=
     +"Element_Vector";

   Element_Visitor : constant League.Strings.Universal_String :=
     +"Element_Visitor";

   Element_Iterator : constant League.Strings.Universal_String :=
     +"Element_Iterator";

   Lexical_Element : constant League.Strings.Universal_String :=
     +"Lexical_Element";

   Token : constant League.Strings.Universal_String := +"Token";

   function Is_Element
     (Name : League.Strings.Universal_String) return Boolean is
       (Name = Element);

   function Is_Token
     (Name : League.Strings.Universal_String) return Boolean is
       (Name = Token);

   function Is_Boolean
     (Name : League.Strings.Universal_String) return Boolean is
       (Name = +"Boolean");

   function Get_Props
     (F         : aliased in out Ada_Pretty.Factory;
      Item      : Meta.Classes.Class;
      Prefix    : Ada_Pretty.Node_Access) return Ada_Pretty.Node_Access;

   ----------------------
   -- Full_Record_Name --
   ----------------------

   function Full_Record_Name
     (Type_Name : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String := Get_Package_Name (Type_Name);
   begin
      if not Result.Is_Empty then
         Result.Append (".");
      end if;

      Result.Append (To_Element_Name (Type_Name));
      return Result;
   end Full_Record_Name;

   ----------------------
   -- Full_Vector_Name --
   ----------------------

   function Full_Vector_Name
     (Type_Name : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String;
   begin
      if Is_Element (Type_Name) then
         Result := Full_Record_Name (Element_Vector);
      else
         Result := Get_Package_Name (Type_Name);
         Result.Append (".");
         Result.Append (To_Element_Name (Type_Name));
         Result.Append ("_Vector");
      end if;

      return Result;
   end Full_Vector_Name;

   ----------------------
   -- Full_Access_Name --
   ----------------------

   function Full_Access_Name
     (Type_Name : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String := Get_Package_Name (Type_Name);
   begin
      Result.Append (".");
      Result.Append (Type_Name);
      Result.Append ("_Access");
      return Result;
   end Full_Access_Name;

   ---------------
   -- Get_Props --
   ---------------

   function Get_Props
     (F         : aliased in out Ada_Pretty.Factory;
      Item      : Meta.Classes.Class;
      Prefix    : Ada_Pretty.Node_Access) return Ada_Pretty.Node_Access
   is
      use all type Meta.Classes.Capacity_Kind;

      function Is_Not_A_Token (P : Classes.Property) return Boolean is
        (not Is_Token (P.Type_Name));

      function Skip_Tokens is new Classes.Generic_Filter (Is_Not_A_Token);

      Element_Name : constant League.Strings.Universal_String :=
        To_Element_Name (Item.Name);

      Next   : Ada_Pretty.Node_Access;
      R_Type : Ada_Pretty.Node_Access;
      Result : Ada_Pretty.Node_Access := Prefix;
      Props  : constant Meta.Classes.Property_Array := Item.Properties;
   begin
      for P of Skip_Tokens (Props) loop
         if Is_Boolean (P.Type_Name) then
            R_Type := F.New_Name (P.Type_Name);

         elsif P.Capacity in Just_One | Zero_Or_One then
            R_Type := F.New_Selected_Name (Full_Access_Name (P.Type_Name));

         elsif Is_Element (P.Type_Name) then
            R_Type := F.New_Selected_Name
              (+"Program.Element_Vectors.Element_Vector_Access");

         else
            R_Type := F.New_Selected_Name
              (Full_Vector_Name (P.Type_Name) & "_Access");

         end if;

         if P.Capacity /= Zero_Or_One
           and then not Is_Token (P.Type_Name)
           and then not Is_Boolean (P.Type_Name)
         then
            R_Type := F.New_Null_Exclusion (R_Type);
         end if;

         Next := F.New_Subprogram_Declaration
           (Specification => F.New_Subprogram_Specification
              (Is_Overriding => Ada_Pretty.False,
               Name          =>
                 F.New_Name (P.Name),
               Parameters    => F.New_Parameter
                 (Name            => F.New_Name (+"Self"),
                  Type_Definition => F.New_Name (Element_Name)),
               Result        => R_Type),
            Is_Abstract   => True);

         Result := F.New_List (Result, Next);
      end loop;

      return Result;
   end Get_Props;

   ----------------------
   -- Get_With_Clauses --
   ----------------------

   function Get_With_Clauses
     (F             : access Ada_Pretty.Factory;
      Vector        : Meta.Read.Class_Vectors.Vector;
      Is_Limited    : Boolean;
      With_Abstract : Boolean := True;
      Skip          : Natural := 1) return Ada_Pretty.Node_Access
   is
      Result : Ada_Pretty.Node_Access;
   begin
      for J in 1 + Skip .. Vector.Last_Index loop
         declare
            Item : constant Meta.Classes.Class := Vector (J);
            Name : constant League.Strings.Universal_String := Item.Name;
            Package_Name : constant League.Strings.Universal_String :=
              Get_Package_Name (Name);
            Next : constant Ada_Pretty.Node_Access :=
              F.New_With (F.New_Name (Package_Name), Is_Limited);
         begin
            if With_Abstract or else not Item.Is_Abstract then
               Result := F.New_List (Result, Next);
            end if;
         end;
      end loop;

      return Result;
   end Get_With_Clauses;

   ----------------------
   -- Get_Package_Name --
   ----------------------

   function Get_Package_Name
     (Type_Name : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String;
   begin
      if Is_Boolean (Type_Name) then
         return League.Strings.Empty_Universal_String;
      end if;

      Result.Append ("Program");

      if Type_Name not in
        Element_Vector | Token | Element_Visitor | Element_Iterator
          | Lexical_Element
      then
         Result.Append (".Elements");
      end if;

      if not Is_Element (Type_Name) then
         Result.Append (".");
         Result.Append (Type_Name);
         Result.Append ("s");
      end if;

      return Result;
   end Get_Package_Name;

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

      function Element_Classifications return Ada_Pretty.Node_Access;

      function Element_Casts return Ada_Pretty.Node_Access;

      -------------------
      -- Element_Casts --
      -------------------

      function Element_Casts return Ada_Pretty.Node_Access is
         Result : Ada_Pretty.Node_Access;
      begin
         for J in 2 .. Vector.Last_Index loop
            declare
               Item : constant Meta.Classes.Class := Vector (J);

               Name : constant League.Strings.Universal_String :=
                 "To_" & Item.Name;

               Type_Name : constant Ada_Pretty.Node_Access :=
                 F.New_Selected_Name (Full_Access_Name (Item.Name));

               Funct : constant Ada_Pretty.Node_Access :=
                 F.New_Subprogram_Declaration
                   (F.New_Subprogram_Specification
                      (Name          => F.New_Name (Name),
                       Parameters    => F.New_Parameter
                         (Name            => F.New_Name (+"Self"),
                          Type_Definition => F.New_Access
                            (Target => F.New_Name (+"Element'Class"))),
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

      function Element_Classifications return Ada_Pretty.Node_Access is
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

         Result : Ada_Pretty.Node_Access;
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
                          Type_Definition => F.New_Name (Element)),
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
             (Modifier => Access_All,
              Target   => Element_Class),
           Aspects => F.New_Aspect
             (Name  => F.New_Name (+"Storage_Size"),
              Value => F.New_Literal (0)));

      Classifications : constant Ada_Pretty.Node_Access :=
        Element_Classifications;

      Casts : constant Ada_Pretty.Node_Access := Element_Casts;

      Visit : constant Ada_Pretty.Node_Access :=
        F.New_Subprogram_Declaration
          (F.New_Subprogram_Specification
             (Is_Overriding => Ada_Pretty.False,
              Name          => F.New_Name (+"Visit"),
              Parameters    => F.New_List
                (F.New_Parameter
                     (Name            => F.New_Name (+"Self"),
                      Type_Definition => F.New_Null_Exclusion
                        (F.New_Access
                           (Target => Element_Name))),
                 F.New_Parameter
                   (Name            => F.New_Name (+"Visitor"),
                    Type_Definition => F.New_Selected_Name
                      (Full_Record_Name (Element_Visitor) & "'Class"),
                    Is_In => True,
                    Is_Out => True))),
           Is_Abstract => True);

      Each_Enclosing : constant Ada_Pretty.Node_Access :=
        F.New_Subprogram_Declaration
          (F.New_Subprogram_Specification
             (Name          => F.New_Name (+"Each_Enclosing_Element"),
              Parameters    => F.New_Parameter
                (Name            => F.New_Name (+"Self"),
                 Type_Definition => F.New_Null_Exclusion
                   (F.New_Access
                        (Target => Element_Class))),
              Result => F.New_Selected_Name
                (Get_Package_Name (Element_Iterator) &
                   ".Enclosing_Element_Iterator")));

      Each_Child : constant Ada_Pretty.Node_Access :=
        F.New_Subprogram_Declaration
          (F.New_Subprogram_Specification
             (Name          => F.New_Name (+"Each_Child"),
              Parameters    => F.New_Parameter
                (Name            => F.New_Name (+"Self"),
                 Type_Definition => F.New_Null_Exclusion
                   (F.New_Access
                        (Target => Element_Class))),
              Result => F.New_Selected_Name
                (Get_Package_Name (Element_Iterator) &
                   ".Child_Iterator")));

      Assigned : constant Ada_Pretty.Node_Access :=
        F.New_Subprogram_Declaration
          (F.New_Subprogram_Specification
             (Name          => F.New_Name (+"Assigned"),
              Parameters    => F.New_Parameter
                (Name            => F.New_Name (+"Self"),
                 Type_Definition => F.New_Access
                        (Target => Element_Class)),
              Result => F.New_Name (+"Boolean")),
           Expression => F.New_List
                (F.New_Name (+"Self"),
                 F.New_Infix (+"/=", F.New_Name (+"null"))));

      Public_Part : constant Ada_Pretty.Node_Access := F.New_List
        ((Pure,
         Element_Decl,
         Element_Access,
         Assigned,
         Classifications,
         Casts,
         Get_Props (F, Vector.First_Element, Visit),
         Each_Enclosing,
         Each_Child));

      Root : constant Ada_Pretty.Node_Access :=
        F.New_Package (Program_Elements, Public_Part);

      Unit : constant Ada_Pretty.Node_Access :=
        F.New_Compilation_Unit
          (Root,
           F.New_List
             ((Get_With_Clauses (F'Access, Vector, Is_Limited => True),
              F.New_With
                 (F.New_Selected_Name
                     (Get_Package_Name (Element_Visitor)),
                 Is_Limited => True),
              F.New_With
                 (F.New_Selected_Name
                     (Get_Package_Name (Element_Iterator))))));

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
                 F.New_Selected_Name (Full_Access_Name (Item.Name));

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
                            (Target => F.New_Name (+"Element'Class"))),
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

      Element_Class  : constant Ada_Pretty.Node_Access :=
        F.New_Name (+"Element'Class");

      Each_Enclosing : constant Ada_Pretty.Node_Access :=
        F.New_Subprogram_Declaration
          (F.New_Subprogram_Specification
             (Name          => F.New_Name (+"Each_Enclosing_Element"),
              Parameters    => F.New_Parameter
                (Name            => F.New_Name (+"Self"),
                 Type_Definition => F.New_Null_Exclusion
                   (F.New_Access
                        (Target => Element_Class))),
              Result => F.New_Selected_Name
                (Get_Package_Name (Element_Iterator) &
                   ".Enclosing_Element_Iterator")),
           Renamed => F.New_Selected_Name
                (Get_Package_Name (Element_Iterator) &
                   ".To_Enclosing_Element_Iterator"));

      Each_Child : constant Ada_Pretty.Node_Access :=
        F.New_Subprogram_Declaration
          (F.New_Subprogram_Specification
             (Name          => F.New_Name (+"Each_Child"),
              Parameters    => F.New_Parameter
                (Name            => F.New_Name (+"Self"),
                 Type_Definition => F.New_Null_Exclusion
                   (F.New_Access
                        (Target => Element_Class))),
              Result => F.New_Selected_Name
                (Get_Package_Name (Element_Iterator) &
                   ".Child_Iterator")),
           Renamed => F.New_Selected_Name
                (Get_Package_Name (Element_Iterator) &
                   ".To_Child_Iterator"));

      Root : constant Ada_Pretty.Node_Access :=
        F.New_Package_Body
          (Program_Elements,
           Element_Casts (F.New_List (Each_Enclosing, Each_Child)));

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

   ---------------------
   -- Write_Iterators --
   ---------------------

   procedure Write_Iterators (Vector : Meta.Read.Class_Vectors.Vector) is
      Package_Name : constant League.Strings.Universal_String :=
        +"Internal";

      F : aliased Ada_Pretty.Factory;

      Visitor : constant Ada_Pretty.Node_Access :=
        F.New_Type
          (Name          => F.New_Name (+"Visitor"),
           Definition    => F.New_Record
             (Parent     => F.New_Selected_Name
                (Full_Record_Name (Element_Visitor)),
              Components => F.New_Variable
                (Name            => F.New_Name (+"Result"),
                 Type_Definition => F.New_Access
                   (Modifier => Access_Constant,
                    Target   => F.New_Name (+"Getter_Array")))));

      function Method_Bodies return Ada_Pretty.Node_Access;

      function Method_Spec
        (Item : Classes.Class) return Ada_Pretty.Node_Access;

      function Child_Args
        (Item : Classes.Class;
         P    : Classes.Property) return Ada_Pretty.Node_Access;

      function Vector_Args
        (Item : Classes.Class;
         P    : Classes.Property) return Ada_Pretty.Node_Access;

      function Get_Inst
        (Item  : Classes.Class;
         P     : Classes.Property;
         Index : Positive) return Ada_Pretty.Node_Access;

      function Is_Vector (P : Classes.Property) return Boolean is
        (P.Capacity in Classes.Zero_Or_More | Classes.One_Or_More);

      function Get_Name (Index : Natural) return Wide_Wide_String;

      Count : Positive := 1;

      function Get_Name (Index : Natural) return Wide_Wide_String is
         Count_Image : constant Wide_Wide_String :=
           Integer'Wide_Wide_Image (Count);
         Index_Image : constant Wide_Wide_String :=
           Integer'Wide_Wide_Image (Index);

         Name : constant Wide_Wide_String :=
           "F" & Count_Image (2 .. Count_Image'Last);

      begin
         if Index = 0 then
            return Name;
         else
            return Name  & "_" & Index_Image (2 .. Index_Image'Last);
         end if;
      end Get_Name;

      ----------------
      -- Child_Args --
      ----------------

      function Child_Args
        (Item : Classes.Class;
         P    : Classes.Property) return Ada_Pretty.Node_Access
      is
         Args : constant Ada_Pretty.Node_Access_Array :=
           (F.New_Argument_Association
              (Choice => F.New_Name (+"Element"),
               Value  => F.New_Selected_Name
                 (Full_Record_Name (Item.Name))
              ),
            F.New_Argument_Association
              (Choice => F.New_Name (+"Child"),
               Value  => F.New_Selected_Name
                 (Full_Record_Name (P.Type_Name))
              ),
            F.New_Argument_Association
              (Choice => F.New_Name (+"Child_Access"),
               Value  => F.New_Selected_Name (Full_Access_Name (P.Type_Name))
              ),
            F.New_Argument_Association
              (Choice => F.New_Name (+"Get_Child"),
               Value  => F.New_Selected_Name
                 (Get_Package_Name (Item.Name) & "." & P.Name)
              )
           );
      begin
         return F.New_List (Args);
      end Child_Args;

      -----------------
      -- Vector_Args --
      -----------------

      function Vector_Args
        (Item : Classes.Class;
         P    : Classes.Property) return Ada_Pretty.Node_Access
      is
         Args : constant Ada_Pretty.Node_Access_Array :=
           (F.New_Argument_Association
              (Choice => F.New_Name (+"Parent"),
               Value  => F.New_Selected_Name
                 (Full_Record_Name (Item.Name))
              ),
            F.New_Argument_Association
              (Choice => F.New_Name (+"Vector"),
               Value  => F.New_Selected_Name
                 (Full_Vector_Name (P.Type_Name))
              ),
            F.New_Argument_Association
              (Choice => F.New_Name (+"Vector_Access"),
               Value  => F.New_Selected_Name
                 (Full_Vector_Name (P.Type_Name) & "_Access")
              ),
            F.New_Argument_Association
              (Choice => F.New_Name (+"Get_Vector"),
               Value  => F.New_Selected_Name
                 (Get_Package_Name (Item.Name) & "." & P.Name)
              )
           );
      begin
         return F.New_List (Args);
      end Vector_Args;

      --------------
      -- Get_Inst --
      --------------

      function Get_Inst
        (Item  : Classes.Class;
         P     : Classes.Property;
         Index : Positive) return Ada_Pretty.Node_Access
      is
         Template : constant Wide_Wide_String :=
           (if Is_Vector (P)
            then "Vector"
            else "Child");

         Result : constant Ada_Pretty.Node_Access :=
           (F.New_Apply
              (Prefix    => F.New_Name
                   (+"function " & Get_Name (Index) &
                        " is new Generic_" & Template),
               Arguments =>
                 (if Is_Vector (P) then Vector_Args (Item, P)
                  else Child_Args (Item, P))));
      begin
         return Result;
      end Get_Inst;

      -----------------
      -- Method_Spec --
      -----------------

      function Method_Spec
        (Item : Classes.Class) return Ada_Pretty.Node_Access
      is
         Element_Name : constant League.Strings.Universal_String :=
           To_Element_Name (Item.Name);

         Funct : constant Ada_Pretty.Node_Access :=
             F.New_Subprogram_Specification
                (Is_Overriding => Ada_Pretty.True,
                 Name          => F.New_Name (Element_Name),
                 Parameters    => F.New_List
                   (F.New_Parameter
                        (Name            => F.New_Name (+"Self"),
                         Is_In           => True,
                         Is_Out          => True,
                         Type_Definition => F.New_Name (+"Visitor")),
                    F.New_Parameter
                      (Name            => F.New_Name (Element),
                       Type_Definition => F.New_Null_Exclusion
                         (F.New_Selected_Name
                              (Full_Access_Name (Item.Name))))));
      begin
         return Funct;
      end Method_Spec;

      -------------------
      -- Method_Bodies --
      -------------------

      function Method_Bodies return Ada_Pretty.Node_Access is
         Result : Ada_Pretty.Node_Access;
         Bodies : Ada_Pretty.Node_Access;
         Map : constant array (Boolean) of Ada_Pretty.Node_Access :=
           (False => F.New_Name (+"False"),
            True  => F.New_Name (+"True"));
      begin
         for Item of Vector loop
            if not Item.Is_Abstract then
               declare
                  Props : constant Meta.Classes.Property_Array :=
                    Item.Properties;

                  Funct : constant Ada_Pretty.Node_Access :=
                    F.New_Subprogram_Body
                      (Specification => Method_Spec (Item),
                       Declarations  => F.New_Pragma
                         (Name      => F.New_Name (+"Unreferenced"),
                          Arguments => F.New_Name (+"Element")),
                       Statements    => F.New_Assignment
                            (Left  => F.New_Selected_Name (+"Self.Result"),
                             Right => F.New_Name (+Get_Name (0) & "'Access")));

                  Index : Positive := 1;

                  List : Ada_Pretty.Node_Access;
               begin
                  for P of Props loop
                     if not Is_Token (P.Type_Name) then
                        declare
                           Inst : constant Ada_Pretty.Node_Access :=
                             F.New_Subprogram_Declaration
                               (Get_Inst (Item, P, Index));
                           Data : constant Ada_Pretty.Node_Access_Array :=
                             (Map (Is_Vector (P)),
                              F.New_Name (P.Name),
                              F.New_Name (+Get_Name (Index) & "'Access"));
                        begin
                           Bodies := F.New_List (Bodies, Inst);

                           List := F.New_List
                             (List, F.New_Component_Association
                                (Choices => F.New_Literal (Index),
                                 Value   => F.New_Parentheses
                                   (F.New_List (Data))
                                 ));

                           Index := Index + 1;
                        end;
                     end if;
                  end loop;

                  if Index > 1 then
                     Result := F.New_List
                       (Result,
                        F.New_Subprogram_Declaration (Method_Spec (Item)));

                     Bodies := F.New_List
                       (Bodies, F.New_Variable
                          (Name            => F.New_Name (+Get_Name (0)),
                           Type_Definition => F.New_Name (+"Getter_Array"),
                           Initialization  => F.New_Parentheses (List),
                           Is_Constant     => True,
                           Is_Aliased      => True));

                     Bodies := F.New_List (Bodies, Funct);
                  end if;

                  Count := Count + 1;
               end;
            end if;
         end loop;

         return F.New_List (Result, Bodies);
      end Method_Bodies;

      Get : constant Ada_Pretty.Node_Access :=
        F.New_Subprogram_Body
          (F.New_Subprogram_Specification
             (Name => F.New_Name (+"Get"),
              Parameters => F.New_Parameter
                (Name            => F.New_Name (+"Parent"),
                 Type_Definition => F.New_Name
                   (Full_Record_Name (Element) & "_Access")),
              Result => F.New_Access
                (Modifier => Access_Constant,
                 Target   => F.New_Name (+"Getter_Array"))),
           Declarations => F.New_Variable
             (Name            => F.New_Name (+"V"),
              Type_Definition => F.New_Name (+"Visitor")),
           Statements => F.New_List
             (F.New_Statement
                (F.New_Apply
                     (Prefix    => F.New_Selected_Name (+"Parent.Visit"),
                      Arguments => F.New_Name (+"V"))),
              F.New_Return
                (F.New_Selected_Name (+"V.Result"))));

      Root : constant Ada_Pretty.Node_Access :=
        F.New_Subunit
          (Parent_Name => F.New_Selected_Name
             (Get_Package_Name (Element_Iterator)),
           Proper_Body => F.New_Package_Body
             (F.New_Selected_Name (Package_Name),
              F.New_List
                ((Visitor,
                 Method_Bodies,
                 Get))));

      Unit : constant Ada_Pretty.Node_Access :=
        F.New_Compilation_Unit
          (Root,
           F.New_List
             (Get_With_Clauses
                (F'Access, Vector, Is_Limited => False),
              F.New_With
                (F.New_Selected_Name
                     (Get_Package_Name (Element_Visitor)))));

      Output : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Open_File
        (Output, Get_Package_Name (Element_Iterator) & "." & Package_Name,
         Spec => False);

      Ada.Wide_Wide_Text_IO.Put_Line
        (Output,
         F.To_Text (Unit).Join (Ada.Characters.Wide_Wide_Latin_1.LF)
         .To_Wide_Wide_String);

      Ada.Wide_Wide_Text_IO.Close (Output);
   end Write_Iterators;

   ------------------------
   -- Write_One_Elements --
   ------------------------

   procedure Write_One_Element
     (Item      : Meta.Classes.Class;
      With_List : Boolean)
   is

      F : aliased Ada_Pretty.Factory;

      function Get_Clauses return Ada_Pretty.Node_Access;
      function Get_Parents return Ada_Pretty.Node_Access;
      function Append_Vector
        (Upper          : Ada_Pretty.Node_Access;
         Element_Access : Ada_Pretty.Node_Access)
          return Ada_Pretty.Node_Access;

      function Get_Text
        (Prefix, Element : Ada_Pretty.Node_Access)
         return Ada_Pretty.Node_Access;
      --  Append text related types and getters

      function Is_A_Token (P : Classes.Property) return Boolean is
        (Is_Token (P.Type_Name));

      function Only_Tokens is new Classes.Generic_Filter (Is_A_Token);

      -------------------
      -- Append_Vector --
      -------------------

      function Append_Vector
        (Upper          : Ada_Pretty.Node_Access;
         Element_Access : Ada_Pretty.Node_Access)
         return Ada_Pretty.Node_Access is
      begin
         if not With_List then
            return Upper;
         end if;

         declare
            Type_Name : constant Ada_Pretty.Node_Access :=
              F.New_Name (Item.Name & "_Vector");
            Type_Decl : constant Ada_Pretty.Node_Access :=
              F.New_Type
                (Type_Name,
                 Definition => F.New_Interface
                   (Is_Limited => True,
                    Parents    => F.New_Infix
                      (+"and",
                       F.New_Selected_Name
                         (+"Program.Element_Vectors.Element_Vector"))));

            Vector_Class  : constant Ada_Pretty.Node_Access :=
              F.New_Name (Item.Name & "_Vector'Class");

            Access_Name : constant Ada_Pretty.Node_Access :=
              F.New_Name (Item.Name & "_Vector_Access");

            Vector_Access : constant Ada_Pretty.Node_Access :=
              F.New_Type
                (Access_Name,
                 Definition => F.New_Access
                   (Modifier => Access_All,
                    Target   => Vector_Class),
                 Aspects => F.New_Aspect
                   (Name  => F.New_Name (+"Storage_Size"),
                    Value => F.New_Literal (0)));

            Getter : constant Ada_Pretty.Node_Access :=
              F.New_Subprogram_Declaration
                (Specification => F.New_Subprogram_Specification
                   (Is_Overriding => Ada_Pretty.True,
                    Name          => F.New_Name (Element),
                    Parameters    => F.New_List
                      (F.New_Parameter
                           (Name            => F.New_Name (+"Self"),
                            Type_Definition => Type_Name),
                       F.New_Parameter
                         (Name            => F.New_Name (+"Index"),
                          Type_Definition => F.New_Name (+"Positive"))),
                    Result        => F.New_Null_Exclusion
                      (Definition => F.New_Selected_Name
                           (+"Program.Elements.Element_Access"),
                       Exclude    => True)),
                 Is_Abstract   => True,
                 Aspects       => F.New_Aspect
                   (Name  => F.New_Name (+"Post'Class"),
                    Value => F.New_Selected_Name
                      ("Element'Result.Is_" & Item.Name)));

            Specific : constant Ada_Pretty.Node_Access :=
              F.New_Subprogram_Declaration
                (Specification => F.New_Subprogram_Specification
                   (Name          => F.New_Name ("To_" & Item.Name),
                    Parameters    => F.New_List
                      (F.New_Parameter
                           (Name            => F.New_Name (+"Self"),
                            Type_Definition => Vector_Class),
                       F.New_Parameter
                           (Name            => F.New_Name (+"Index"),
                            Type_Definition => F.New_Name (+"Positive"))),
                    Result        => F.New_Null_Exclusion
                      (Definition => Element_Access,
                       Exclude    => True)),
                 Expression      => F.New_Selected_Name
                   (Prefix   => F.New_Name (+"Self.Element (Index)"),
                    Selector => F.New_Name ("To_" & Item.Name)));
         begin
            return F.New_List
              ((Upper, Type_Decl, Vector_Access, Getter, Specific));
         end;
      end Append_Vector;

      -----------------
      -- Get_Clauses --
      -----------------

      function Get_Clauses return Ada_Pretty.Node_Access is
         use all type Meta.Classes.Capacity_Kind;

         procedure Append (Value : League.Strings.Universal_String);

         List    : League.String_Vectors.Universal_String_Vector;

         ------------
         -- Append --
         ------------

         procedure Append (Value : League.Strings.Universal_String) is
         begin
            if List.Index (Value) = 0 then
               List.Append (Value);
            end if;
         end Append;

         Parents : constant League.String_Vectors.Universal_String_Vector :=
           Item.Parents;
         Result  : Ada_Pretty.Node_Access;
         Each    : Ada_Pretty.Node_Access;
         Props   : constant Meta.Classes.Property_Array := Item.Properties;
      begin
         if With_List then
            Append (Element_Vector);
         end if;

         for J in 1 .. Parents.Length loop
            if not Is_Element (Parents.Element (J)) then
               Append (Parents.Element (J));
            end if;
         end loop;

         for P of Props loop
            if Is_Token (P.Type_Name) then
               Append (Lexical_Element);
            elsif P.Capacity in One_Or_More | Zero_Or_More
              and then Is_Element (P.Type_Name)
            then
               Append (Element_Vector);
            elsif not Is_Element (P.Type_Name) then
               Append (P.Type_Name);
            end if;
         end loop;

         for J in 1 .. List.Length loop
            Each := F.New_With
              (F.New_Selected_Name (Get_Package_Name (List (J))));
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
            Parent := Full_Record_Name (Parents (J));

            Each := F.New_Infix
              (+"and",
               F.New_Selected_Name (Parent));

            Result := F.New_List (Result, Each);
         end loop;

         return Result;
      end Get_Parents;

      --------------
      -- Get_Text --
      --------------

      function Get_Text
        (Prefix, Element : Ada_Pretty.Node_Access)
         return Ada_Pretty.Node_Access is
      begin
         if Item.Is_Abstract then
            return Prefix;
         end if;

         declare
            Text_Type : constant League.Strings.Universal_String :=
              Item.Name & "_Text";

            Type_Name : constant Ada_Pretty.Node_Access :=
              F.New_Name (Text_Type);

            Type_Class  : constant Ada_Pretty.Node_Access :=
              F.New_Name (Text_Type & "'Class");

            Access_Name : constant Ada_Pretty.Node_Access :=
              F.New_Name (Text_Type & "_Access");

            Type_Decl : constant Ada_Pretty.Node_Access :=
              F.New_Type
                (Type_Name,
                 Definition => F.New_Interface
                   (Is_Limited => True));

            Text_Access : constant Ada_Pretty.Node_Access :=
              F.New_Type
                (Access_Name,
                 Definition => F.New_Access
                   (Modifier => Access_All,
                    Target   => Type_Class),
                 Aspects => F.New_Aspect
                   (Name  => F.New_Name (+"Storage_Size"),
                    Value => F.New_Literal (0)));

            To_Text : constant Ada_Pretty.Node_Access :=
              F.New_Subprogram_Declaration
                (Specification => F.New_Subprogram_Specification
                   (Is_Overriding => Ada_Pretty.False,
                    Name          => F.New_Name ("To_" & Text_Type),
                    Parameters    => F.New_Parameter
                      (Name            => F.New_Name (+"Self"),
                       Type_Definition => Element,
                       Is_Aliased      => True),
                    Result        => Access_Name),
                 Is_Abstract   => True);

            R_Type : array (Boolean) of Ada_Pretty.Node_Access :=
              (False => F.New_Selected_Name
                 (Full_Access_Name (Lexical_Element)),
               True => null);

            Next : Ada_Pretty.Node_Access;

            Result : Ada_Pretty.Node_Access :=
              F.New_List ((Prefix, Type_Decl, Text_Access, To_Text));
         begin
            R_Type (True) := F.New_Null_Exclusion (R_Type (False));

            for P of Only_Tokens (Item.Properties) loop
               Next := F.New_Subprogram_Declaration
                 (Specification => F.New_Subprogram_Specification
                    (Is_Overriding => Ada_Pretty.False,
                     Name          => F.New_Name (P.Name),
                     Parameters    => F.New_Parameter
                       (Name            => F.New_Name (+"Self"),
                        Type_Definition => Type_Name),
                     Result        => R_Type (P.Capacity in Classes.Just_One)),
                  Is_Abstract   => True);

               Result := F.New_List (Result, Next);
            end loop;

            return Result;
         end;
      end Get_Text;

      Package_Name : constant League.Strings.Universal_String :=
        Get_Package_Name (Item.Name);

      Element_Name : constant League.Strings.Universal_String :=
        To_Element_Name (Item.Name);

      Type_Name : constant Ada_Pretty.Node_Access := F.New_Name (Element_Name);

      Type_Class  : constant Ada_Pretty.Node_Access :=
        F.New_Name (Element_Name & "'Class");

      Access_Name : constant Ada_Pretty.Node_Access :=
        F.New_Name (Item.Name & "_Access");

      Element_Access : constant Ada_Pretty.Node_Access :=
        F.New_Type
          (Access_Name,
           Definition => F.New_Access
             (Modifier => Access_All,
              Target   => Type_Class),
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
        Append_Vector
          (F.New_List
             ((Pure,
              Type_Decl,
              Get_Text (Get_Props (F, Item, Element_Access), Type_Name))),
           Element_Access => Access_Name);

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
   end Write_One_Element;

   --------------------
   -- Write_Visitors --
   --------------------

   procedure Write_Visitors (Vector : Meta.Read.Class_Vectors.Vector) is

      function Methods return Ada_Pretty.Node_Access;

      Package_Name : constant League.Strings.Universal_String :=
        Get_Package_Name (Element_Visitor);

      F : aliased Ada_Pretty.Factory;

      Pure : constant Ada_Pretty.Node_Access :=
        F.New_Pragma
          (F.New_Name (+"Pure"), F.New_Selected_Name (Package_Name));

      Type_Name : constant Ada_Pretty.Node_Access :=
        F.New_Name (Element_Visitor);

      Type_Decl : constant Ada_Pretty.Node_Access :=
        F.New_Type
          (Type_Name,
           Definition => F.New_Interface
             (Is_Limited => True));

      function Methods return Ada_Pretty.Node_Access is
         Result : Ada_Pretty.Node_Access;
      begin
         for Item of Vector loop
            if not Item.Is_Abstract then
               declare
                  Element_Name : constant League.Strings.Universal_String :=
                    To_Element_Name (Item.Name);

                  Funct : constant Ada_Pretty.Node_Access :=
                    F.New_Subprogram_Declaration
                      (F.New_Subprogram_Specification
                         (Name          => F.New_Name (Element_Name),
                          Parameters    => F.New_List
                            (F.New_Parameter
                              (Name            => F.New_Name (+"Self"),
                               Is_In           => True,
                               Is_Out          => True,
                               Type_Definition => F.New_Name
                                 (Element_Visitor)),
                             F.New_Parameter
                               (Name            => F.New_Name (Element),
                                Type_Definition => F.New_Null_Exclusion
                                  (F.New_Selected_Name
                                       (Full_Access_Name (Item.Name)))))),
                       Is_Null => True);
               begin
                  Result := F.New_List (Result, Funct);
               end;
            end if;
         end loop;

         return Result;
      end Methods;

      Public_Part : constant Ada_Pretty.Node_Access :=
        F.New_List ((Pure, Type_Decl, Methods));

      Root : constant Ada_Pretty.Node_Access :=
        F.New_Package (F.New_Selected_Name (Package_Name), Public_Part);

      Unit : constant Ada_Pretty.Node_Access :=
        F.New_Compilation_Unit
          (Root,
           Get_With_Clauses
             (F'Access, Vector, Is_Limited => False, With_Abstract => False));

      Output : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Open_File (Output, Package_Name);

      Ada.Wide_Wide_Text_IO.Put_Line
        (Output,
         F.To_Text (Unit).Join (Ada.Characters.Wide_Wide_Latin_1.LF)
         .To_Wide_Wide_String);

      Ada.Wide_Wide_Text_IO.Close (Output);
   end Write_Visitors;

end Meta.Writes;
