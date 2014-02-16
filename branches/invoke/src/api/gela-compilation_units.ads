limited with Gela.Compilations;
limited with Gela.Contexts;
limited with Gela.Unit_Containers;
limited with Gela.Compilation_Unit_Sets;
with Gela.Lexical_Types;

package Gela.Compilation_Units is
   pragma Preelaborate;

   type Compilation_Unit is limited interface;
   type Compilation_Unit_Access is access all Compilation_Unit'Class;
   for Compilation_Unit_Access'Storage_Size use 0;

   not overriding function Name
     (Self : access Compilation_Unit)
      return Gela.Lexical_Types.Symbol is abstract;

   not overriding function Context
     (Self : access Compilation_Unit)
      return Gela.Contexts.Context_Access is abstract;

   not overriding function Container
     (Self : access Compilation_Unit)
      return Gela.Unit_Containers.Unit_Container_Access is abstract;

   not overriding function Compilation
     (Self : access Compilation_Unit)
      return Gela.Compilations.Compilation_Access is abstract;

   type Library_Item is limited interface and Compilation_Unit;

   type Package_Unit is tagged;
   type Package_Unit_Access is access all Package_Unit'Class;
   for Package_Unit_Access'Storage_Size use 0;

   not overriding function Parent
     (Self : access Library_Item)
      return Package_Unit_Access is abstract;

   type Body_Unit is limited interface and Library_Item;
   type Body_Unit_Access is access all Body_Unit'Class;
   for Body_Unit_Access'Storage_Size use 0;

   type Library_Unit_Declaration is limited interface and Library_Item;
   type Library_Unit_Declaration_Access is
     access all Library_Unit_Declaration'Class;
   for Library_Unit_Declaration_Access'Storage_Size use 0;

   not overriding function Corresponding_Declaration
     (Self : access Body_Unit) return Library_Unit_Declaration_Access
       is abstract;

   not overriding function Subunits
     (Self : access Body_Unit)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
        is abstract;

   not overriding function Corresponding_Body
     (Self : access Library_Unit_Declaration) return Body_Unit_Access
       is abstract;

   type Package_Unit is limited interface and Library_Unit_Declaration;

   not overriding function Corresponding_Childern
     (Self : access Package_Unit)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
        is abstract;
   --  Return library unit declarations for children and subprogram bodies
   --  that interpreted as both the declaration and the body of a library
   --  subprogram.

   type Subunit is limited interface and Compilation_Unit;
   type Subunit_Access is access all Subunit'Class;
   for Subunit_Access'Storage_Size use 0;

   not overriding function Subunits
     (Self : access Subunit)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
      is abstract;

   not overriding function Corresponding_Subunit_Parent_Body
     (Self : access Subunit) return Compilation_Unit_Access is abstract;

end Gela.Compilation_Units;
