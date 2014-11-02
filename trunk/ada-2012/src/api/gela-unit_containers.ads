--  This package provides Unit_Container interface and its methods.

with League.Strings;

limited with Gela.Compilation_Unit_Sets;
limited with Gela.Contexts;

package Gela.Unit_Containers is
   pragma Preelaborate;

   type Unit_Container is limited interface;
   --  Container for compilation units
   type Unit_Container_Access is access all Unit_Container'Class;
   for Unit_Container_Access'Storage_Size use 0;

   type Unit_Container_List is
     array (Positive range <>) of Unit_Container_Access;

   function Name
     (Self : Unit_Container)
      return League.Strings.Universal_String is abstract;
   --  Return Name of container

   function Context
     (Self : Unit_Container)
      return Gela.Contexts.Context_Access is abstract;
   --  Return correspondig context

   function Library_Unit_Declarations
     (Self  : Unit_Container)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
        is abstract;
   --  Return set of library unit declarations of container

   function Compilation_Unit_Bodies
     (Self  : Unit_Container)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
        is abstract;
   --  Return set of bodies and subunits of container

end Gela.Unit_Containers;
