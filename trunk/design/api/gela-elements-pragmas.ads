limited with Gela.Element_Sequences;
with Gela.Symbols;

package Gela.Elements.Pragmas is
   pragma Preelaborate;

   type Pragma_Element is limited interface and Gela.Elements.Element;
   --  Defining name element
   type Pragma_Element_Access is access constant Pragma_Element'Class;
   for Pragma_Element_Access'Storage_Size use 0;

   not overriding function Symbol
     (Self : aliased Pragma_Element)
      return Gela.Symbols.Symbol_Access is abstract;
   --  Return symbol of given pragma element

   not overriding function Arguments
     (Self : aliased Pragma_Element)
      return Gela.Element_Sequences.Element_Sequence_Access is abstract;
   --  Return argument associations of given pragma element

end Gela.Elements.Pragmas;
