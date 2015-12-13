with Ada.Containers.Hashed_Maps;

with League.Strings.Hash;

with Gela.A4G.Elements;
with Gela.A4G.Compilation_Units;
with Gela.Compilation_Unit_Sets;
with Gela.Compilation_Units;
with Gela.Contexts;
with Gela.Symbols;

with Asis.Compilation_Units;
with Asis.Elements;

package Gela.A4G.Contexts is

   type Context is limited new Gela.Contexts.Context with private;

   type Context_Access is access all Context'Class;
   for Context_Access'Storage_Size use 0;

   not overriding function Create_Compilation_Unit
     (Self : access Context;
      Unit : Asis.Compilation_Unit)
      return Gela.A4G.Compilation_Units.Compilation_Unit_Access;

   not overriding function Create_Symbol
     (Self  : access Context;
      Image : League.Strings.Universal_String)
      return Gela.Symbols.Symbol_Access;

   not overriding function Create_Element
     (Self    : access Context;
      Element : Asis.Element)
      return Gela.A4G.Elements.Element_Access;

   not overriding function Create_Element_List
     (Self : access Context;
      List : Asis.Element_List)
      return Gela.A4G.Elements.Element_Sequence_Access;

   procedure Initialize
     (Self       : in out Context;
      Parameters : Wide_String);

private

   package Symbol_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => League.Strings.Universal_String,
      Element_Type    => Gela.Symbols.Symbol_Access,
      Hash            => League.Strings.Hash,
      Equivalent_Keys => League.Strings."=",
      "="             => Gela.Symbols."=");

   function Hash
     (Unit : Asis.Compilation_Unit) return Ada.Containers.Hash_Type;

   package Unit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Asis.Compilation_Unit,
      Element_Type    => Gela.A4G.Compilation_Units.Compilation_Unit_Access,
      Hash            => Hash,
      Equivalent_Keys => Asis.Compilation_Units.Is_Identical,
      "="             => Gela.A4G.Compilation_Units."=");

   function Hash
     (Element : Asis.Element) return Ada.Containers.Hash_Type;

   package Element_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Asis.Element,
      Element_Type    => Gela.A4G.Elements.Element_Access,
      Hash            => Hash,
      Equivalent_Keys => Asis.Elements.Is_Identical,
      "="             => Gela.A4G.Elements."=");

   package Element_List_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Asis.Element,
      Element_Type    => Gela.A4G.Elements.Element_Sequence_Access,
      Hash            => Hash,
      Equivalent_Keys => Asis.Elements.Is_Identical,
      "="             => Gela.A4G.Elements."=");

   package Unit_Sets is

      type Compilation_Unit_Set
        (Context   : not null access Gela.A4G.Contexts.Context;
         Need_Body : Boolean)
      is
        new Gela.Compilation_Unit_Sets.Compilation_Unit_Set
      with null record;

      overriding function Is_Empty
        (Self : Compilation_Unit_Set) return Boolean;

      overriding function Length
        (Self : Compilation_Unit_Set) return Natural;

      overriding function Find
        (Self   : Compilation_Unit_Set;
         Symbol : not null Gela.Symbols.Symbol_Access)
         return Gela.Compilation_Units.Compilation_Unit_Access;

      overriding function Iterate
        (Self : Compilation_Unit_Set)
         return Gela.Compilation_Unit_Sets.Iterator_Interfaces
        .Forward_Iterator'Class;

      overriding function Context
        (Self : Compilation_Unit_Set) return Gela.Contexts.Context_Access;

   end Unit_Sets;

   type Context is limited new Gela.Contexts.Context with record
      Specs      : aliased Unit_Sets.Compilation_Unit_Set
        (Context'Unchecked_Access, False);
      Bodies     : aliased Unit_Sets.Compilation_Unit_Set
        (Context'Unchecked_Access, True);
      Context    : Asis.Context;
      Symbols    : Symbol_Maps.Map;
      Units      : Unit_Maps.Map;
      Elements   : Element_Maps.Map;
      Lists      : Element_List_Maps.Map;
      Body_Count : Natural := 0;
   end record;

   overriding function Library_Unit_Declarations
     (Self  : aliased Context)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;

   overriding function Compilation_Unit_Bodies
     (Self  : aliased Context)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;

   overriding function Symbol
     (Self  : aliased Context;
      Image : League.Strings.Universal_String)
      return Gela.Symbols.Symbol_Access;

end Gela.A4G.Contexts;
