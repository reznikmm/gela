limited with Gela.Compilation_Units;
limited with Gela.Element_Visiters;

package Gela.Elements is
   pragma Preelaborate;

   type Element is limited interface;
   --  Element of some compilation unit
   type Element_Access is access constant Element'Class;
   for Element_Access'Storage_Size use 0;

   function Assigned (Self : access Element'Class) return Boolean
     is (Self /= null);

   not overriding function Enclosing_Element
     (Self : aliased Element)
      return Gela.Elements.Element_Access is abstract;
   --  Return upper element if any

   not overriding function Compilation_Unit
     (Self : aliased Element)
      return Gela.Compilation_Units.Compilation_Unit_Access is abstract;
   --  Return corresponding compilation unit

   not overriding procedure Visit
     (Self    : aliased Element;
      Visiter : in out Gela.Element_Visiters.Visiter'Class) is abstract;
   --  Visit corresponding method of given Visiter

end Gela.Elements;
