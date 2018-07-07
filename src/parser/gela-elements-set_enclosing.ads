package Gela.Elements.Set_Enclosing is
   pragma Preelaborate;

   type Element is limited interface;
   type Element_Access is access all Element'Class;
   for Element_Access'Storage_Size use 0;

   not overriding procedure Set_Enclosing_Element
     (Self  : in out Element;
      Value : Gela.Elements.Element_Access) is abstract;
   --  Set upper element to Value.

end Gela.Elements.Set_Enclosing;
