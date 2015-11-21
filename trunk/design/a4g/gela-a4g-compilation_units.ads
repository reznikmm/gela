with Gela.Compilation_Unit_Sets;
with Gela.Contexts;
with Gela.Symbols;
with Gela.Compilation_Units.Visiters;

with Asis;

package Gela.A4G.Compilation_Units is

   type Compilation_Unit is
     new Gela.Compilation_Units.Library_Unit_Declaration
       and Gela.Compilation_Units.Library_Unit_Body
       and Gela.Compilation_Units.Subunit
         with private;

   type Compilation_Unit_Access is access all Compilation_Unit'Class;

   function Create
     (Unit    : Asis.Compilation_Unit;
      Context : Gela.Contexts.Context_Access)
      return Gela.Compilation_Units.Compilation_Unit_Access;

private

   package Children_Sets is

      type Compilation_Unit_Set is
        new Gela.Compilation_Unit_Sets.Compilation_Unit_Set
      with record
         First  : Gela.Compilation_Units.Compilation_Unit_Access;
         Length : Natural := 0;
      end record;

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

   end Children_Sets;

   package Subunit_Sets is

      type Compilation_Unit_Set is
        new Children_Sets.Compilation_Unit_Set with null record;

      overriding function Iterate
        (Self : Compilation_Unit_Set)
         return Gela.Compilation_Unit_Sets.Iterator_Interfaces
        .Forward_Iterator'Class;

   end Subunit_Sets;

   type Compilation_Unit is
     new Gela.Compilation_Units.Library_Unit_Declaration
       and Gela.Compilation_Units.Library_Unit_Body
       and Gela.Compilation_Units.Subunit
   with record
      Unit         : Asis.Compilation_Unit;
      Name         : Gela.Symbols.Symbol_Access;
      Context      : Gela.Contexts.Context_Access;
      Next_Child   : Gela.Compilation_Units.Compilation_Unit_Access;
      Next_Subunit : Gela.Compilation_Units.Compilation_Unit_Access;
      Children     : aliased Children_Sets.Compilation_Unit_Set;
      Subunits     : aliased Subunit_Sets.Compilation_Unit_Set;
   end record;

   overriding function Name
     (Self : access Compilation_Unit)
      return Gela.Symbols.Symbol_Access;

   overriding function Context
     (Self : access Compilation_Unit)
      return Gela.Contexts.Context_Access;

   overriding function Parent
     (Self : access Compilation_Unit)
      return Gela.Compilation_Units.Compilation_Unit_Access;

   overriding function Is_Library_Unit_Declaration
     (Self : access Compilation_Unit) return Boolean;

   overriding function Is_Library_Unit_Body
     (Self : access Compilation_Unit) return Boolean;

   overriding function Is_Subunit
     (Self : access Compilation_Unit) return Boolean;

   overriding procedure Visit
     (Self    : access Compilation_Unit;
      Visiter : in out Gela.Compilation_Units.Visiters.Visiter'Class);

   overriding function Children
     (Self : access Compilation_Unit)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;

   overriding function Subunits
     (Self : access Compilation_Unit)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;

   overriding function Corresponding_Declaration
     (Self : access Compilation_Unit)
      return Gela.Compilation_Units.Library_Unit_Declaration_Access;

   overriding function Corresponding_Body
     (Self : access Compilation_Unit)
      return Gela.Compilation_Units.Library_Unit_Body_Access;

end Gela.A4G.Compilation_Units;
