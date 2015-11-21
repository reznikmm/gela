with Ada.Strings.Wide_Hash;

with Gela.A4G.Compilation_Units;
with Gela.A4G.Symbols;

package body Gela.A4G.Contexts is

   --------------------------
   -- Get_Compilation_Unit --
   --------------------------

   not overriding function Get_Compilation_Unit
     (Self : access Context;
      Unit : Asis.Compilation_Unit)
      return Gela.Compilation_Units.Compilation_Unit_Access
   is
      Pos : constant Unit_Maps.Cursor := Self.Units.Find (Unit);

      Result : Gela.Compilation_Units.Compilation_Unit_Access;
   begin
      if Asis.Compilation_Units.Is_Nil (Unit) then
         return null;
      elsif Unit_Maps.Has_Element (Pos) then
         Result := Unit_Maps.Element (Pos);
      else
         Result := Gela.A4G.Compilation_Units.Create
           (Unit    => Unit,
            Context => Self);

         Self.Units.Insert (Unit, Result);
      end if;

      return Result;
   end Get_Compilation_Unit;

   ----------
   -- Hash --
   ----------

   function Hash
     (Unit : Asis.Compilation_Unit) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Wide_Hash
        (Asis.Compilation_Units.Unique_Name (Unit));
   end Hash;

   ------------
   -- Symbol --
   ------------

   overriding function Symbol
     (Self  : access Context;
      Image : League.Strings.Universal_String)
      return Gela.Symbols.Symbol_Access
   is
      Key : constant League.Strings.Universal_String :=
        Image.To_Simple_Casefold;
      Pos : constant Symbol_Maps.Cursor := Self.Symbols.Find (Key);
      Result : Gela.Symbols.Symbol_Access;
   begin
      if Symbol_Maps.Has_Element (Pos) then
         Result := Symbol_Maps.Element (Pos);
      else
         Result := Gela.A4G.Symbols.Create (Self, Image, Key);
         Self.Symbols.Insert (Key, Result);
      end if;

      return Result;
   end Symbol;

end Gela.A4G.Contexts;
