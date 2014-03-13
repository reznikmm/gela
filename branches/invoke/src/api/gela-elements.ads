--  This package provides Element interface and their methods.
with Gela.Lexical_Types;

limited with Gela.Compilation_Units;
limited with Gela.Compilations;
limited with Gela.Element_Visiters;

package Gela.Elements is
   pragma Preelaborate;

   type Element is limited interface;
   --  Base type for any element in abstract syntax tree of compilation unit.

   type Element_Access is access all Element'Class;
   for Element_Access'Storage_Size use 0;

   not overriding function Enclosing_Element
     (Self  : Element) return Element_Access is abstract;
   --  Return upper element if any.

   not overriding function Enclosing_Compilation
     (Self  : Element) return Gela.Compilations.Compilation_Access is abstract;
   --  Return corresponding compilation.

   not overriding function Enclosing_Compilation_Unit
     (Self  : Element)
      return Gela.Compilation_Units.Compilation_Unit_Access is abstract;
   --  Return corresponding compilation unit.

   not overriding function Is_Part_Of_Implicit
     (Self  : Element) return Boolean is abstract;

   not overriding function Is_Part_Of_Inherited
     (Self  : Element) return Boolean is abstract;

   not overriding function Is_Part_Of_Instance
     (Self  : Element) return Boolean is abstract;

   not overriding function First_Token
     (Self  : Element) return Gela.Lexical_Types.Token_Count is abstract;
   --  Return first token index of given element.

   not overriding function Last_Token
     (Self  : Element) return Gela.Lexical_Types.Token_Count is abstract;
   --  Return last token index of given element.

   not overriding procedure Visit
     (Self    : access Element;
      Visiter : in out Gela.Element_Visiters.Visiter'Class) is abstract;
   --  Move Visiter over given element.

   type Element_Sequence is tagged;
   type Element_Sequence_Access is access all Element_Sequence'Class;
   for Element_Sequence_Access'Storage_Size use 0;

   type Nested_Kinds is (Nested_Token, Nested_Element, Nested_Sequence);
   type Nested (Kind : Nested_Kinds := Nested_Token) is record
      case Kind is
         when Nested_Token =>
            Nested_Token : Gela.Lexical_Types.Token_Index;
         when Nested_Element =>
            Nested_Element : Element_Access;
         when Nested_Sequence =>
            Nested_Sequence : Element_Sequence_Access;
      end case;
   end record;

   type Nested_Array is array (Positive range <>) of Nested;

   not overriding function Nested_Items
     (Self  : Element) return Nested_Array is abstract;
   --  Return nested elements.

   package Element_Sequences is
      type Sequence is limited interface;
      --  Sequence containing Elements
      type Sequence_Cursor is interface;
      --  Cursor in sequence of Element

      not overriding function Is_Empty
        (Self : Sequence) return Boolean is abstract;

      not overriding function Length
        (Self : Sequence) return Natural is abstract;

      not overriding function First
        (Self : Sequence)
         return Sequence_Cursor'Class is abstract;

      not overriding function Has_Element
        (Self : Sequence_Cursor) return Boolean is abstract;

      not overriding function Element
        (Self : Sequence_Cursor) return Element_Access is abstract;

      not overriding procedure Next
        (Self : in out Sequence_Cursor) is abstract;

   end Element_Sequences;

   type Element_Sequence is limited interface and Element_Sequences.Sequence;
   subtype Element_Sequence_Cursor is Element_Sequences.Sequence_Cursor'Class;

   generic
      type Item is limited interface and Element;
      type Item_Access is access all Item'Class;
   package Generic_Element_Sequences is
      type Sequence is limited interface and Element_Sequence;
      --  Sequence containing given Item-s
      type Sequence_Cursor is interface;
      --  Cursor in sequence of Item

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

end Gela.Elements;
