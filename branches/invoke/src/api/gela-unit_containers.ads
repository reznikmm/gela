with League.Strings;

limited with Gela.Compilation_Unit_Sets;
limited with Gela.Contexts;

package Gela.Unit_Containers is
   pragma Preelaborate;

   type Unit_Container is limited interface;
   type Unit_Container_Access is access all Unit_Container'Class;
   for Unit_Container_Access'Storage_Size use 0;

   type Unit_Container_List is
     array (Positive range <>) of Unit_Container_Access;

   function Name
     (Self : Unit_Container)
      return League.Strings.Universal_String is abstract;

   function Context
     (Self : Unit_Container)
      return Gela.Contexts.Context_Access is abstract;

   function Library_Unit_Declarations
     (Self  : Unit_Container)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
        is abstract;

   function Compilation_Unit_Bodies
     (Self  : Unit_Container)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
        is abstract;

end Gela.Unit_Containers;
