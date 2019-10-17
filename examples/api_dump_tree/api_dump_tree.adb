with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.Strings;
with League.String_Vectors;

with Gela.Compilation_Unit_Sets;
with Gela.Compilation_Units;
with Gela.Context_Factories;
with Gela.Contexts;
with Gela.Elements;

with Element_Printers;

procedure API_Dump_Tree is
   
   procedure Process_Unit_Set
     (Unit_Set : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access);

   procedure Process_Unit
     (Unit : Gela.Compilation_Units.Compilation_Unit_Access);

   procedure Process_Element
     (Element : access Gela.Elements.Element'Class;
      Indent  : Natural);
   
   Context : Gela.Contexts.Context_Access;
   
   -------------------
   -- For_Each_Unit --
   -------------------

   procedure Process_Unit_Set
     (Unit_Set : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access)
   is
      Cursor : Gela.Compilation_Unit_Sets.Compilation_Unit_Cursor'Class :=
        Unit_Set.First;
   begin
      while Cursor.Has_Element loop
         Process_Unit (Unit => Cursor.Element);
         Cursor.Next;
      end loop;
   end Process_Unit_Set;
   
   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (Unit : Gela.Compilation_Units.Compilation_Unit_Access)
   is
      Unit_Name : League.Strings.Universal_String :=
        Context.Symbols.Image (Unit.Name);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        ("UNIT NAME: " & Unit_Name.To_Wide_Wide_String);
      Process_Element (Unit.Tree, Indent => 0);
   end Process_Unit;

   ---------------------
   -- Process_Element --
   ---------------------

   procedure Process_Element
     (Element : access Gela.Elements.Element'Class;
      Indent  : Natural)
   is
      Space    : constant Wide_Wide_String :=
        "                                                                  " &
        "                                                                  " &
        "                                                                  ";

      Printer  : Element_Printers.Element_Printer;
   begin
      if not Element.Assigned then
         Ada.Wide_Wide_Text_IO.Put_Line
           (Space (1 .. Indent) & "null");
         return;
      end if;
      
      Element.Visit (Printer);
      Ada.Wide_Wide_Text_IO.Put_Line
        (Space (1 .. Indent)
         & Printer.Image.To_Wide_Wide_String);

      for Item of Element.Nested_Items loop
         case Item.Kind is
            when Gela.Elements.Nested_Token =>
               null;
            when Gela.Elements.Nested_Element =>
               Process_Element (Item.Nested_Element, Indent + 2);
            when Gela.Elements.Nested_Sequence =>
               declare
                  Cursor : Gela.Elements.Element_Sequence_Cursor'Class :=
                    Item.Nested_Sequence.First;
               begin
                  Ada.Wide_Wide_Text_IO.Put_Line
                    (Space (1 .. Indent) & "[");
                  
                  while Cursor.Has_Element loop
                     Process_Element (Cursor.Element, Indent + 2);
                     Cursor.Next;
                  end loop;
                  
                  Ada.Wide_Wide_Text_IO.Put_Line
                    (Space (1 .. Indent) & "]");
               end;
         end case;
      end loop;
   end Process_Element;
   
   Env     : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("GELA_INCLUDE_PATH");
   Args    : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;

begin
   Context := Gela.Context_Factories.Create_Context
     (Args, League.Application.Environment.Value (Env));

   Process_Unit_Set (Context.Library_Unit_Declarations);
   Process_Unit_Set (Context.Compilation_Unit_Bodies);
end API_Dump_Tree;
