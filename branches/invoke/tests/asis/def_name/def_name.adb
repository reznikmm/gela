with Ada.Command_Line;
with Ada.Strings.Wide_Fixed;
with Ada.Wide_Text_IO;
with Ada.Wide_Wide_Text_IO;

with Asis;
with Asis.Ada_Environments;
with Asis.Clauses;
with Asis.Compilation_Units;
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
         when others =>
            null;
      end case;
   end On_Element;

   -------------------
   -- On_Identifier --
   -------------------

   procedure On_Identifier (Item : Asis.Identifier) is
      Span  : Asis.Text.Span;
      Def   : constant Asis.Defining_Name :=
        Asis.Expressions.Corresponding_Name_Definition (Item);
   begin
      Result.Append
        (League.Strings.From_UTF_16_Wide_String
           (Asis.Expressions.Name_Image (Item)));

      Span := Asis.Text.Element_Span (Item);
      Result.Append (Asis.ASIS_Natural'Wide_Wide_Image (Span.First_Line));
      Result.Append (Asis.ASIS_Natural'Wide_Wide_Image (Span.First_Column));
      Result.Append (" =>");

      if not Asis.Elements.Is_Nil (Def) then
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
            when Asis.A_With_Clause =>
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

      Iterate (Asis.Elements.Unit_Declaration (Unit), Control, Result);
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

   Args    : League.String_Vectors.Universal_String_Vector;
   Params  : League.Strings.Universal_String;
   Context : Asis.Context;
   Hash    : League.Hash_Type;
begin
   for J in 1 .. League.Application.Arguments.Length - 1 loop
      Args.Append (League.Application.Arguments.Element (J));
   end loop;

   Hash := League.Hash_Type'Wide_Wide_Value
     (League.Application.Arguments.Element
        (League.Application.Arguments.Length).To_Wide_Wide_String);

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
      List : constant Asis.Compilation_Unit_List :=
        Asis.Compilation_Units.Compilation_Units (Context);
   begin
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
