with Ada.Containers.Hashed_Maps;

with League.Strings.Hash;

with Gela.Compilation_Units;
with Gela.Contexts;
with Gela.Symbols;

with Asis.Compilation_Units;

package Gela.A4G.Contexts is

   type Context is new Gela.Contexts.Context with private;

   type Context_Access is access all Context'Class;
   for Context_Access'Storage_Size use 0;

   overriding function Symbol
     (Self  : access Context;
      Image : League.Strings.Universal_String)
      return Gela.Symbols.Symbol_Access;

   not overriding function Get_Compilation_Unit
     (Self : access Context;
      Unit : Asis.Compilation_Unit)
      return Gela.Compilation_Units.Compilation_Unit_Access;

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
      Element_Type    => Gela.Compilation_Units.Compilation_Unit_Access,
      Hash            => Hash,
      Equivalent_Keys => Asis.Compilation_Units.Is_Identical,
      "="             => Gela.Compilation_Units."=");

   type Context is new Gela.Contexts.Context with record
      Symbols : Symbol_Maps.Map;
      Units   : Unit_Maps.Map;
   end record;

end Gela.A4G.Contexts;
