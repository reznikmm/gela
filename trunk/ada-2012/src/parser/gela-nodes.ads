with Gela.Elements;
with Gela.Elements.Set_Enclosing;
with Gela.Compilations;
with Gela.Compilation_Units;
with Gela.LARL_Parsers_Nodes;
with Gela.Lexical_Types;
with Ada.Containers.Doubly_Linked_Lists;

package Gela.Nodes is
   pragma Preelaborate;

   type Node (Length : Positive) is
     abstract limited new Gela.Elements.Element
     and Gela.Elements.Set_Enclosing.Element with private;
   type Node_Access is access all Node'Class;

   type Nested_Kind_Array is
     array (Positive range <>) of Gela.Elements.Nested_Kinds;

   not overriding function Nested
     (Self : Node) return Nested_Kind_Array is abstract;

   overriding procedure Set_Part_Of_Implicit (Self  : in out Node);
   overriding procedure Set_Part_Of_Inherited (Self : in out Node);
   overriding procedure Set_Part_Of_Instance (Self  : in out Node);

   generic
      with package Generic_Element_Sequences is new
        Gela.Elements.Generic_Element_Sequences (<>);
      type Item_Sequence is limited interface
        and Generic_Element_Sequences.Sequence;
   package Node_Sequences is

      type Sequence is limited new Item_Sequence
        and Gela.Elements.Set_Enclosing.Element with private;
      type Sequence_Access is access all Sequence;

   private
      package Lists is new Ada.Containers.Doubly_Linked_Lists
        (Element_Type => Generic_Element_Sequences.Item_Access,
         "="          => Generic_Element_Sequences."=");

      type Sequence is limited new Item_Sequence
        and Gela.Elements.Set_Enclosing.Element
      with record
         List : Lists.List;
      end record;

      overriding function Is_Empty (Self : Sequence) return Boolean;

      overriding function Length (Self : Sequence) return Natural;

      overriding procedure Append
        (Self : in out Sequence;
         Item : Generic_Element_Sequences.Item_Access);

      overriding procedure Prepend
        (Self : in out Sequence;
         Item : Generic_Element_Sequences.Item_Access);

      overriding function First
        (Self : Sequence)
         return Generic_Element_Sequences.Sequence_Cursor'Class;

      overriding function First
        (Self : Sequence)
         return Gela.Elements.Element_Sequences.Sequence_Cursor'Class;

      overriding procedure Set_Enclosing_Element
        (Self  : in out Sequence;
         Value : Gela.Elements.Element_Access);

      overriding function Each_Element (Self : Sequence)
        return Gela.Elements.Element_Sequences.Iterators
          .Forward_Iterator'Class;

      overriding function Iterate (Self : Sequence)
        return Generic_Element_Sequences.Iterators.Forward_Iterator'Class;

      type Sequence_Cursor is new Generic_Element_Sequences.Sequence_Cursor
        and Gela.Elements.Element_Sequences.Sequence_Cursor
      with record
         Data : Lists.Cursor;
      end record;

      overriding function Has_Element (Self : Sequence_Cursor) return Boolean;

      overriding function Element
        (Self : Sequence_Cursor) return Generic_Element_Sequences.Item_Access;

      overriding function Element
        (Self : Sequence_Cursor) return Gela.Elements.Element_Access;

      overriding procedure Next (Self : in out Sequence_Cursor);

   end Node_Sequences;

private

   type Node (Length : Positive) is abstract limited new Gela.Elements.Element
     and Gela.Elements.Set_Enclosing.Element
   with record
      Enclosing_Element     : Gela.Elements.Element_Access;
      Enclosing_Compilation : Gela.Compilations.Compilation_Access;
      Is_Part_Of_Implicit   : Boolean;
      Is_Part_Of_Inherited  : Boolean;
      Is_Part_Of_Instance   : Boolean;
      Children              : Gela.LARL_Parsers_Nodes.Node_Array (1 .. Length);
   end record;

   overriding function Enclosing_Element
     (Self  : Node) return Gela.Elements.Element_Access;

   overriding function Enclosing_Compilation
     (Self  : Node) return Gela.Compilations.Compilation_Access;

   overriding function Enclosing_Compilation_Unit
     (Self  : Node) return Gela.Compilation_Units.Compilation_Unit_Access;

   overriding function Is_Part_Of_Implicit (Self  : Node) return Boolean;
   overriding function Is_Part_Of_Inherited (Self : Node) return Boolean;
   overriding function Is_Part_Of_Instance (Self  : Node) return Boolean;

   overriding function First_Token
     (Self  : Node) return Gela.Lexical_Types.Token_Count;

   overriding function Last_Token
     (Self  : Node) return Gela.Lexical_Types.Token_Count;

   overriding function Nested_Items
     (Self  : Node) return Gela.Elements.Nested_Array;

   overriding procedure Set_Enclosing_Element
     (Self  : in out Node;
      Value : Gela.Elements.Element_Access);

   overriding function Hash (Self  : Node) return Ada.Containers.Hash_Type;

end Gela.Nodes;
