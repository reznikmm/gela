with Ada.Iterator_Interfaces;

with Gela.Elements;
with Gela.Declarations;
with Gela.Elements.Defining_Names;

package Gela.Element_Sequences is
   pragma Preelaborate;

   type Element_Sequence is tagged;
   --  Sequence of Elements
   type Element_Sequence_Access is access constant Element_Sequence'Class;
   for Element_Sequence_Access'Storage_Size use 0;

   type Base_Element_Sequence is limited interface;
   --  Common ancestor for all Sequences of elements

   not overriding function To_Element_Sequence
     (Self : aliased Base_Element_Sequence) return Element_Sequence_Access
       is abstract;
   --  Convert to Element_Sequence

   package Element_Sequences is
      type Sequence is limited interface and Base_Element_Sequence
      with
        Constant_Indexing => Constant_Indexing,
        Default_Iterator  => Iterate,
        Iterator_Element  => Gela.Elements.Element'Class;
      --  Sequence of Elements

--        type Sequence_Access is access constant Sequence'Class;
--        for Sequence_Access'Storage_Size use 0;

      not overriding function Is_Empty
        (Self : aliased Sequence) return Boolean is abstract;
      --  Check is set is empty

      not overriding function Length
        (Self : aliased Sequence) return Natural is abstract;
      --  Return length of set

      function Assigned (Self : Gela.Elements.Element_Access) return Boolean is
        (Gela.Elements."/=" (Self, null));
      pragma Inline (Assigned);

      package Iterator_Interfaces is new Ada.Iterator_Interfaces
        (Cursor       => Gela.Elements.Element_Access,
         Has_Element  => Assigned);

      not overriding function Iterate
        (Self : aliased Sequence)
         return Iterator_Interfaces.Forward_Iterator'Class is abstract;
      --  Return iterator over the set.

      type Reference_Type
        (Element : not null access constant Gela.Elements.Element'Class) is
          null record
            with Implicit_Dereference => Element;

      function Constant_Indexing
        (Self   : Sequence'Class;
         Cursor : not null Gela.Elements.Element_Access)
         return Reference_Type is (Element => Cursor);

      pragma Inline (Constant_Indexing);

   end Element_Sequences;

   type Element_Sequence is limited interface and Element_Sequences.Sequence;

   generic
      type Item is limited interface and Gela.Elements.Element;
      type Item_Access is access constant Item'Class;
      type Parent_Sequence_Access;
   package Generic_Element_Sequences is
      type Sequence is limited interface and Base_Element_Sequence
      with
        Constant_Indexing => Constant_Indexing,
        Default_Iterator  => Iterate,
        Iterator_Element  => Item'Class;

      --  Set of items.

      not overriding function Is_Empty
        (Self : aliased Sequence) return Boolean is abstract;
      --  Check is set is empty

      not overriding function Length
        (Self : aliased Sequence) return Natural is abstract;
      --  Return length of set

      not overriding function To_Parent_Sequence
        (Self : aliased Sequence) return Parent_Sequence_Access
          is abstract;
      --  Convert to Element_Sequence

      function Assigned (Self : Item_Access) return Boolean is (Self /= null);
      pragma Inline (Assigned);

      package Iterator_Interfaces is new Ada.Iterator_Interfaces
        (Cursor       => Item_Access,
         Has_Element  => Assigned);

      not overriding function Iterate
        (Self : aliased Sequence)
         return Iterator_Interfaces.Forward_Iterator'Class is abstract;
      --  Return iterator over the set.

      type Reference_Type
        (Element : not null access constant Item'Class) is null record
          with Implicit_Dereference => Element;

      function Constant_Indexing
        (Self   : Sequence'Class;
         Cursor : not null Item_Access) return Reference_Type is
           (Element => Cursor);

      pragma Inline (Constant_Indexing);

   end Generic_Element_Sequences;

   package Defining_Name_Sequences is
     new Gela.Element_Sequences.Generic_Element_Sequences
       (Item        => Gela.Elements.Defining_Names.Defining_Name,
        Item_Access => Gela.Elements.Defining_Names.Defining_Name_Access,
        Parent_Sequence_Access => Element_Sequence_Access);

   subtype Defining_Name_Sequence is Defining_Name_Sequences.Sequence;

   type Defining_Name_Sequence_Access is
     access constant Defining_Name_Sequence'Class;
   for Defining_Name_Sequence_Access'Storage_Size use 0;

   package Declaration_Sequences is
     new Gela.Element_Sequences.Generic_Element_Sequences
       (Item                   => Gela.Declarations.Declaration,
        Item_Access            => Gela.Declarations.Declaration_Access,
        Parent_Sequence_Access => Element_Sequence_Access);

   subtype Declaration_Sequence is Declaration_Sequences.Sequence;

   type Declaration_Sequence_Access is
     access constant Declaration_Sequence'Class;
   for Declaration_Sequence_Access'Storage_Size use 0;

end Gela.Element_Sequences;
