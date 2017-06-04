--  This package provides Element interface and their methods.

with Ada.Containers;
with Ada.Iterator_Interfaces;

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

   function Assigned (Self : access Element'Class) return Boolean
     is (Self /= null);

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

   not overriding procedure Set_Part_Of_Implicit
     (Self  : in out Element) is abstract;

   not overriding function Is_Part_Of_Inherited
     (Self  : Element) return Boolean is abstract;

   not overriding procedure Set_Part_Of_Inherited
     (Self : in out Element) is abstract;

   not overriding function Is_Part_Of_Instance
     (Self  : Element) return Boolean is abstract;

   not overriding procedure Set_Part_Of_Instance
     (Self  : in out Element) is abstract;

   not overriding function First_Token
     (Self  : Element) return Gela.Lexical_Types.Token_Count is abstract;
   --  Return first token index of given element.

   not overriding function Last_Token
     (Self  : Element) return Gela.Lexical_Types.Token_Count is abstract;
   --  Return last token index of given element.

   not overriding function Hash
     (Self  : Element) return Ada.Containers.Hash_Type is abstract;
   --  Return hash of given element.

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
            Nested_Token : Gela.Lexical_Types.Token_Count;
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
      type Sequence is limited interface with
        Constant_Indexing => Constant_Reference,
        Variable_Indexing => Reference,
        Default_Iterator  => Each_Element,
        Iterator_Element  => Gela.Elements.Element'Class;
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

      --  Iterating syntax sugar stuff
      function Has_Some
        (Self : Sequence_Cursor'Class) return Boolean is (Self.Has_Element);

      package Iterators is new Ada.Iterator_Interfaces
        (Sequence_Cursor'Class, Has_Some);

      not overriding function Each_Element
        (Self : Sequence) return Iterators.Forward_Iterator'Class is abstract;

      type Constant_Reference_Type
        (Item : not null access constant Gela.Elements.Element'Class) is
          null record with Implicit_Dereference => Item;

      function Constant_Reference
        (Self     : Sequence'Class;
         Position : Sequence_Cursor'Class)
         return Constant_Reference_Type is (Item => Position.Element);

      type Reference_Type
        (Item : not null access Gela.Elements.Element'Class) is
          null record with Implicit_Dereference => Item;

      function Reference
        (Self     : in out Sequence'Class;
         Position : Sequence_Cursor'Class)
         return Reference_Type is (Item => Position.Element);

   end Element_Sequences;

   type Element_Sequence is limited interface and Element_Sequences.Sequence;
   subtype Element_Sequence_Cursor is Element_Sequences.Sequence_Cursor'Class;

   generic
      type Item is limited interface and Element;
      type Item_Access is access all Item'Class;
   package Generic_Element_Sequences is
      type Sequence is limited interface and Element_Sequence with
        Default_Iterator  => Iterate,
        Iterator_Element  => Item'Class;
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
        (Self : Sequence_Cursor) return Boolean is abstract;

      not overriding function Element
        (Self : Sequence_Cursor) return Item_Access is abstract;

      not overriding procedure Next
        (Self : in out Sequence_Cursor) is abstract;

      --  Iterating syntax sugar stuff
      function Has_Some
        (Self : Sequence_Cursor'Class) return Boolean is (Self.Has_Element);

      package Iterators is new Ada.Iterator_Interfaces
        (Sequence_Cursor'Class, Has_Some);

      not overriding function Iterate
        (Self : Sequence) return Iterators.Forward_Iterator'Class is abstract;

      function Constant_Reference
        (Self     : Sequence'Class;
         Position : Element_Sequences.Sequence_Cursor'Class)
         return Element_Sequences.Constant_Reference_Type
           is (Item => Position.Element);

      type Constant_Reference_Type
        (Item : not null access constant Gela.Elements.Element'Class) is
          null record with Implicit_Dereference => Item;

      function Constant_Reference
        (Self     : Sequence'Class;
         Position : Sequence_Cursor'Class)
         return Constant_Reference_Type is (Item => Position.Element);

      function Reference
        (Self     : in out Sequence'Class;
         Position : Element_Sequences.Sequence_Cursor'Class)
         return Element_Sequences.Reference_Type
           is (Item => Position.Element);

      type Reference_Type
        (Item : not null access Gela.Elements.Element'Class) is
          null record with Implicit_Dereference => Item;

      function Reference
        (Self     : in out Sequence'Class;
         Position : Sequence_Cursor'Class)
         return Reference_Type is (Item => Position.Element);

   end Generic_Element_Sequences;

end Gela.Elements;
