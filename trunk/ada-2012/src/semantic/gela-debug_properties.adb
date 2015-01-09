with Ada.Tags;
with Gela.Compilations;
with Gela.Elements.Defining_Names;
with Gela.Interpretations;
with Gela.Lexical_Types;
with Gela.Property_Visiters;
with Gela.Semantic_Types;

package body Gela.Debug_Properties is

   procedure Put_Line (Text : String);

   package Dump_Property is

      type Property is (Up, Down, Env_In, Env_Out);
      pragma Unreferenced (Env_In, Env_Out);

      type Property_Flags is array (Property) of Boolean;

      type Property_Visiter is new Gela.Property_Visiters.Property_Visiter with
      record
         Flags : Property_Flags := (others => False);
      end record;

      overriding procedure On_Down
        (Self    : in out Property_Visiter;
         Element : Gela.Elements.Element_Access;
         Value   : Gela.Interpretations.Interpretation_Index);

      overriding procedure On_Up
        (Self    : in out Property_Visiter;
         Element : Gela.Elements.Element_Access;
         Value   : Gela.Interpretations.Interpretation_Set_Index);

   end Dump_Property;

   package Dump_Interpretation is
      type Visiter is new Gela.Interpretations.Visiter with record
         Comp : not null Gela.Compilations.Compilation_Access;
      end record;

      overriding procedure On_Defining_Name
        (Self   : in out Visiter;
         Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
         Down   : Gela.Interpretations.Interpretation_Index_Array);

      overriding procedure On_Expression
        (Self   : in out Visiter;
         Tipe   : Gela.Semantic_Types.Type_Index;
         Down   : Gela.Interpretations.Interpretation_Index_Array);

      overriding procedure On_Attr_Function
        (Self   : in out Visiter;
         Kind   : Gela.Lexical_Types.Predefined_Symbols.Attribute;
         Down   : Gela.Interpretations.Interpretation_Index_Array);

      overriding procedure On_Tuple
        (Self  : in out Visiter;
         Value : Gela.Interpretations.Interpretation_Set_Index_Array;
         Down  : Gela.Interpretations.Interpretation_Index_Array);

   end Dump_Interpretation;

   package body Dump_Property is
      overriding procedure On_Down
        (Self    : in out Property_Visiter;
         Element : Gela.Elements.Element_Access;
         Value   : Gela.Interpretations.Interpretation_Index)
      is
         Comp : constant Gela.Compilations.Compilation_Access :=
           Element.Enclosing_Compilation;
         IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
           Comp.Context.Interpretation_Manager;
         IV : Dump_Interpretation.Visiter := (Comp => Comp);
      begin
         if Self.Flags (Down) = False then
            return;
         end if;

         Put_Line
           ("down:" &
              Gela.Interpretations.Interpretation_Index'Image (Value));
         IM.Visit (Value, IV);
      end On_Down;

      overriding procedure On_Up
        (Self    : in out Property_Visiter;
         Element : Gela.Elements.Element_Access;
         Value   : Gela.Interpretations.Interpretation_Set_Index)
      is
         Comp : constant Gela.Compilations.Compilation_Access :=
           Element.Enclosing_Compilation;
         IM  : constant Gela.Interpretations.Interpretation_Manager_Access :=
           Comp.Context.Interpretation_Manager;
         IV  : aliased Dump_Interpretation.Visiter := (Comp => Comp);
         Pos : Gela.Interpretations.Cursor'Class := IM.Get_Cursor (Value);
      begin
         if Self.Flags (Up) = False then
            return;
         end if;

         Put_Line
           ("up:" &
              Gela.Interpretations.Interpretation_Set_Index'Image (Value));

         while Pos.Has_Element loop
            Put_Line
              ("   INDEX:" &
                 Gela.Interpretations.Interpretation_Index'Image
                   (Pos.Get_Index));
            Pos.Visit (IV'Access);
            Pos.Next;
         end loop;
      end On_Up;

   end Dump_Property;

   package body Dump_Interpretation is

      overriding procedure On_Defining_Name
        (Self   : in out Visiter;
         Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
         Down   : Gela.Interpretations.Interpretation_Index_Array)
      is
         Symbol : constant Gela.Lexical_Types.Symbol := Name.Full_Name;
      begin
         Put_Line
           ("   Defining_Name " &
              Self.Comp.Context.Symbols.Image (Symbol).To_UTF_8_String);

         for J of Down loop
            Put_Line
              ("     DOWN" &
                 Gela.Interpretations.Interpretation_Index'Image (J));
         end loop;
      end On_Defining_Name;

      overriding procedure On_Expression
        (Self   : in out Visiter;
         Tipe   : Gela.Semantic_Types.Type_Index;
         Down   : Gela.Interpretations.Interpretation_Index_Array)
      is
         pragma Unreferenced (Self, Tipe);
      begin
         Put_Line
           ("   Expression ");

         for J of Down loop
            Put_Line
              ("     DOWN" &
                 Gela.Interpretations.Interpretation_Index'Image (J));
         end loop;
      end On_Expression;

      overriding procedure On_Attr_Function
        (Self   : in out Visiter;
         Kind   : Gela.Lexical_Types.Predefined_Symbols.Attribute;
         Down   : Gela.Interpretations.Interpretation_Index_Array) is null;

      overriding procedure On_Tuple
        (Self  : in out Visiter;
         Value : Gela.Interpretations.Interpretation_Set_Index_Array;
         Down  : Gela.Interpretations.Interpretation_Index_Array)
      is
         pragma Unreferenced (Self);
      begin
         Put_Line ("   Tuple");

         for J of Value loop
            Put_Line
              ("     " &
                 Gela.Interpretations.Interpretation_Set_Index'Image (J));
         end loop;

         for J of Down loop
            Put_Line
              ("     DOWN" &
                 Gela.Interpretations.Interpretation_Index'Image (J));
         end loop;
      end On_Tuple;

   end Dump_Interpretation;

   procedure Dump
     (Element : Gela.Elements.Element_Access;
      PV      : access Dump_Property.Property_Visiter;
      EV      : in out Gela.Property_Visiters.Visiter);

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Element : Gela.Elements.Element_Access;
      PV      : access Dump_Property.Property_Visiter;
      EV      : in out Gela.Property_Visiters.Visiter) is
   begin
      if not Element.Assigned then
         return;
      end if;

      declare
         N : constant Gela.Elements.Nested_Array := Element.Nested_Items;
      begin
         Put_Line (Ada.Tags.Expanded_Name (Element'Tag));
         Element.Visit (EV);

         for J of N loop
            case J.Kind is
               when Gela.Elements.Nested_Element =>
                  Dump (J.Nested_Element, PV, EV);
               when Gela.Elements.Nested_Sequence =>
                  declare
                     Pos : Gela.Elements.Element_Sequence_Cursor :=
                       J.Nested_Sequence.First;
                  begin
                     while Pos.Has_Element loop
                        Dump (Pos.Element, PV, EV);
                        Pos.Next;
                     end loop;
                  end;
               when Gela.Elements.Nested_Token =>
                  null;
            end case;
         end loop;
      end;
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Element : Gela.Elements.Element_Access;
      Debug   : League.Strings.Universal_String)
   is
      PV : aliased Dump_Property.Property_Visiter;
      EV : Gela.Property_Visiters.Visiter (PV'Access);
   begin
      for J in Dump_Property.Property loop
         if Debug.Index (Dump_Property.Property'Wide_Wide_Image (J)) > 0 then
            PV.Flags (J) := True;
         end if;
      end loop;

      Dump (Element, PV'Access, EV);
   end Dump;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Text : String) is
      procedure puts (Text : String);
      pragma Import (C, puts, "puts");
   begin
      puts (Text & Character'Val (0));
   end Put_Line;

end Gela.Debug_Properties;