with Ada.Command_Line;
with Ada.Strings.Wide_Fixed;
with Ada.Wide_Text_IO;
with Ada.Wide_Wide_Text_IO;
with Ada.Containers.Generic_Array_Sort;

with Asis;
with Asis.Ada_Environments;
with Asis.Clauses;
with Asis.Compilation_Units;
with Asis.Declarations;
with Asis.Elements;
with Asis.Errors;
with Asis.Exceptions;
with Asis.Expressions;
with Asis.Implementation;
with Asis.Iterator;
with Asis.Text;

with League.Application;
with League.Strings;
with League.String_Vectors;

procedure Def_Name is
   procedure On_Unit (Unit : Asis.Compilation_Unit);
   procedure On_Identifier (Item : Asis.Identifier);

   function Less (Left, Right : Asis.Compilation_Unit) return Boolean;

   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Asis.List_Index,
      Element_Type => Asis.Compilation_Unit,
      Array_Type   => Asis.Compilation_Unit_List,
      "<"          => Less);

   procedure On_Element (Item : Asis.Element);

   procedure Pre_Operation
     (Element : in     Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out League.Strings.Universal_String);

   procedure Post_Operation
     (Element : in     Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out League.Strings.Universal_String) is null;

   procedure Iterate is new Asis.Iterator.Traverse_Element
     (State_Information => League.Strings.Universal_String,
      Pre_Operation     => Pre_Operation,
      Post_Operation    => Post_Operation);

   Result : League.Strings.Universal_String;
   Deep   : Boolean;

   ----------
   -- Less --
   ----------

   function Less (Left, Right : Asis.Compilation_Unit) return Boolean is
      use type Asis.Text.Line_Number;

      Left_Line : constant Asis.Text.Line_Number :=
        Asis.Text.First_Line_Number (Asis.Elements.Unit_Declaration (Left));
      Right_Line : constant Asis.Text.Line_Number :=
        Asis.Text.First_Line_Number (Asis.Elements.Unit_Declaration (Right));
      Left_Name : constant Asis.Program_Text :=
        Asis.Compilation_Units.Text_Name (Left);
      Right_Name : constant Asis.Program_Text :=
        Asis.Compilation_Units.Text_Name (Right);
   begin
      if Left_Name = Right_Name then
         return Left_Line < Right_Line;
      else
         return Left_Name < Right_Name;
      end if;
   end Less;

   ----------------
   -- On_Element --
   ----------------

   procedure On_Element (Item : Asis.Element) is
   begin
      case Asis.Elements.Expression_Kind (Item) is
         when Asis.An_Identifier =>
            On_Identifier (Item);
         when Asis.A_Selected_Component =>
            On_Identifier
              (Asis.Expressions.Selector (Item));
         when Asis.An_Operator_Symbol =>
            On_Identifier (Item);
         when others =>
            null;
      end case;
   end On_Element;

   -------------------
   -- On_Identifier --
   -------------------

   procedure On_Identifier (Item : Asis.Identifier) is
      Span  : Asis.Text.Span;
      Tipe  : Asis.Type_Definition;
      Decl  : Asis.Declaration;
      Unit  : Asis.Compilation_Unit;
      Def   : constant Asis.Defining_Name :=
        Asis.Expressions.Corresponding_Name_Definition (Item);
   begin
      Result.Append
        (League.Strings.From_UTF_16_Wide_String
           (Asis.Expressions.Name_Image (Item)));

      Span := Asis.Text.Element_Span (Item);
      Result.Append (Asis.ASIS_Natural'Wide_Wide_Image (Span.First_Line));
      Result.Append (Asis.ASIS_Natural'Wide_Wide_Image (Span.First_Column));
      Result.Append (" => ");

      if Asis.Elements.Is_Nil (Def) then
         null;
      elsif Asis.Elements.Is_Part_Of_Implicit (Def) then
         Result.Append ("Is_Part_Of_Implicit ");
         Decl := Asis.Elements.Enclosing_Element (Def);

         if Asis.Elements.Declaration_Kind (Decl) in
           Asis.A_Function_Declaration | Asis.A_Procedure_Declaration
         then
            Tipe := Asis.Declarations.Corresponding_Type (Decl);

            if not Asis.Elements.Is_Nil (Tipe) then
               Decl := Asis.Elements.Enclosing_Element (Tipe);
               declare
                  Names : constant Asis.Defining_Name_List :=
                    Asis.Declarations.Names (Decl);
               begin
                  Result.Append
                    (League.Strings.From_UTF_16_Wide_String
                       (Asis.Declarations.Defining_Name_Image
                            (Names (Names'First))));
               end;
            end if;
         end if;
      else
         Unit := Asis.Elements.Enclosing_Compilation_Unit (Def);
         Result.Append
           (League.Strings.From_UTF_16_Wide_String
              (Asis.Compilation_Units.Unit_Full_Name (Unit)));
         Span := Asis.Text.Element_Span (Def);
         Result.Append (Asis.ASIS_Natural'Wide_Wide_Image (Span.First_Line));
         Result.Append (Asis.ASIS_Natural'Wide_Wide_Image (Span.First_Column));
      end if;

      Result.Append (Wide_Wide_Character'Val (10));
   end On_Identifier;

   -------------
   -- On_Unit --
   -------------

   procedure On_Unit (Unit : Asis.Compilation_Unit) is
      Control : Asis.Traverse_Control := Asis.Continue;

      Withs   : constant Asis.Element_List :=
        Asis.Elements.Context_Clause_Elements (Unit);
   begin
      for J in Withs'Range loop
         case Asis.Elements.Clause_Kind (Withs (J)) is
            when Asis.A_With_Clause | Asis.A_Use_Package_Clause =>
               declare
                  Names : constant Asis.Element_List :=
                    Asis.Clauses.Clause_Names (Withs (J));
               begin
                  for K in Names'Range loop
                     On_Element (Names (K));
                  end loop;
               end;
            when others =>
               null;
         end case;
      end loop;

      if Deep then
         Iterate (Asis.Elements.Unit_Declaration (Unit), Control, Result);
      end if;
   end On_Unit;

   -------------------
   -- Pre_Operation --
   -------------------

   procedure Pre_Operation
     (Element : in     Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out League.Strings.Universal_String)
   is
      pragma Unreferenced (Control);
      pragma Unreferenced (State);
   begin
      On_Element (Element);
   end Pre_Operation;

   use type League.Hash_Type;

   Args     : League.String_Vectors.Universal_String_Vector;
   Last_Arg : League.Strings.Universal_String;
   Params   : League.Strings.Universal_String;
   Context  : Asis.Context;
   Hash     : League.Hash_Type;
begin
   for J in 1 .. League.Application.Arguments.Length - 1 loop
      Args.Append (League.Application.Arguments.Element (J));
   end loop;

   Last_Arg := League.Application.Arguments.Element
     (League.Application.Arguments.Length);

   Hash := League.Hash_Type'Wide_Wide_Value (Last_Arg.To_Wide_Wide_String);

   Deep := Last_Arg.Starts_With ("+");

   Params := Args.Join (' ');

   Asis.Implementation.Initialize ("");

   Asis.Ada_Environments.Associate
     (The_Context => Context,
      Name        => Asis.Ada_Environments.Default_Name,
      Parameters  => Params.To_UTF_16_Wide_String);

   Asis.Ada_Environments.Open (Context);

   declare
      Name : constant Wide_String := "/" &
        Args.Element (Args.Length).To_UTF_16_Wide_String;
      List : Asis.Compilation_Unit_List :=
        Asis.Compilation_Units.Compilation_Units (Context);
   begin
      Sort (List);

      for J in List'Range loop
         if Name = Ada.Strings.Wide_Fixed.Tail
                     (Source => Asis.Compilation_Units.Text_Name (List (J)),
                      Count  => Name'Length)
         then
            On_Unit (List (J));
         end if;
      end loop;
   end;

   Asis.Ada_Environments.Close        (Context);
   Asis.Ada_Environments.Dissociate   (Context);
   Asis.Implementation.Finalize       ("");

   if Hash /= Result.Hash then
      Ada.Wide_Wide_Text_IO.Put
        (League.Hash_Type'Wide_Wide_Image (Result.Hash));
      Ada.Wide_Wide_Text_IO.Put_Line ("  ");
      Ada.Wide_Wide_Text_IO.Put_Line (Result.To_Wide_Wide_String);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

exception
   when Asis.Exceptions.ASIS_Failed =>
      Ada.Wide_Text_IO.Put_Line
        ("ASIS_Failed status: " &
           Asis.Errors.Error_Kinds'Wide_Image
           (Asis.Implementation.Status));
      Ada.Wide_Text_IO.Put_Line (Asis.Implementation.Diagnosis);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Def_Name;
