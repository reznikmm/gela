with Gela.Lexical_Types;

limited with Gela.Compilation_Units;
limited with Gela.Compilations;
limited with Gela.Element_Visiters;

package Gela.Elements is
   pragma Preelaborate;

   type Element is limited interface;
   type Element_Access is access all Element'Class;
   for Element_Access'Storage_Size use 0;

   not overriding function Enclosing_Element
     (Self  : Element) return Element_Access is abstract;

   not overriding function Enclosing_Compilation
     (Self  : Element) return Gela.Compilations.Compilation_Access is abstract;

   not overriding function Enclosing_Compilation_Unit
     (Self  : Element)
      return Gela.Compilation_Units.Compilation_Unit_Access is abstract;

   not overriding function Is_Part_Of_Implicit
     (Self  : Element) return Boolean is abstract;

   not overriding function Is_Part_Of_Inherited
     (Self  : Element) return Boolean is abstract;

   not overriding function Is_Part_Of_Instance
     (Self  : Element) return Boolean is abstract;

   not overriding function First_Token
     (Self  : Element) return Gela.Lexical_Types.Token_Count is abstract;

   not overriding function Last_Token
     (Self  : Element) return Gela.Lexical_Types.Token_Count is abstract;

   not overriding procedure Visit
     (Self    : access Element;
      Visiter : in out Gela.Element_Visiters.Visiter) is abstract;

   generic
      type Item is limited interface and Element;
      type Item_Access is access all Item'Class;
   package Generic_Element_Sequences is
      type Sequence is limited interface;
      type Sequence_Cursor is interface;

      not overriding function Is_Empty
        (Self : Sequence) return Boolean is abstract;

      not overriding function Length
        (Self : Sequence) return Natural is abstract;

      not overriding procedure Append
        (Self : in out Sequence;
         Item : Item_Access) is abstract;

      not overriding procedure Prepend
        (Self : in out Sequence;
         Item : Item_Access) is abstract;

      not overriding function First
        (Self : Sequence) return Sequence_Cursor'Class is abstract;

      not overriding function Has_Element
        (Self : Sequence_Cursor)
      return Boolean is abstract;

      not overriding function Element
        (Self : Sequence_Cursor) return Item_Access is abstract;

      not overriding procedure Next
        (Self : in out Sequence_Cursor) is abstract;

   end Generic_Element_Sequences;

   package Element_Sequences is
     new Generic_Element_Sequences (Element, Element_Access);

   type Element_Sequence is limited interface and Element_Sequences.Sequence;
   type Element_Sequence_Access is access all Element_Sequence'Class;
   for Element_Sequence_Access'Storage_Size use 0;
   subtype Element_Sequence_Cursor is Element_Sequences.Sequence_Cursor'Class;

end Gela.Elements;
