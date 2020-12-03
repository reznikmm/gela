--  SPDX-FileCopyrightText: 2019-2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Iterator_Interfaces;

with Program.Elements;
limited with Program.Element_Vectors;

package Program.Element_Iterators is
   pragma Pure;

   type Child_Iterator is tagged;

   type Property_Name is
     (Aborted_Tasks,
      Actual_Parameter,
      Ancestor,
      Arguments,
      Aspect_Definition,
      Aspect_Mark,
      Aspects,
      Associated_Message,
      Attribute_Designator,
      Called_Name,
      Choice_Parameter,
      Choices,
      Clause_Name,
      Clause_Names,
      Clause_Range,
      Component_Clauses,
      Component_Definition,
      Components,
      Condition,
      Constraint,
      Declarations,
      Default_Expression,
      Definition,
      Delta_Expression,
      Digits_Expression,
      Discriminant,
      Discriminant_Part,
      Discriminants,
      Element_Iterator,
      Else_Expression,
      Else_Statements,
      Elsif_Paths,
      Enclosing_Element,
      End_Name,
      End_Statement_Identifier,
      Entry_Barrier,
      Entry_Family_Definition,
      Entry_Index,
      Entry_Index_Subtype,
      Entry_Name,
      Exception_Handlers,
      Exception_Name,
      Exit_Loop_Name,
      Expression,
      Expressions,
      Formal_Parameter,
      Formal_Parameters,
      Generalized_Iterator,
      Generic_Function_Name,
      Generic_Package_Name,
      Generic_Procedure_Name,
      Goto_Label,
      Guard,
      Index_Subtypes,
      Initialization_Expression,
      Iterable_Name,
      Iterator_Name,
      Left,
      Literals,
      Loop_Parameter,
      Lower_Bound,
      Mod_Clause_Expression,
      Modulus,
      Name,
      Names,
      Object_Subtype,
      Operand,
      Operator,
      Parameter,
      Parameters,
      Parameter_Subtype,
      Parent,
      Paths,
      Position,
      Predicate,
      Prefix,
      Private_Declarations,
      Progenitors,
      Protected_Operations,
      Qualified_Expression,
      Raised_Exception,
      Range_Attribute,
      Ranges,
      Real_Range,
      Real_Range_Constraint,
      Record_Definition,
      Renamed_Exception,
      Renamed_Function,
      Renamed_Object,
      Renamed_Package,
      Renamed_Procedure,
      Result_Expression,
      Result_Subtype,
      Return_Object,
      Right,
      Selecting_Expression,
      Selector,
      Selector_Names,
      Slice_Range,
      Statement_Identifier,
      Statements,
      Subpool_Name,
      Subprogram_Default,
      Subtype_Indication,
      Subtype_Mark,
      Then_Abort_Statements,
      Then_Expression,
      Then_Statements,
      Upper_Bound,
      Variable_Name,
      Variants,
      Visible_Declarations);

   package Cursors is

      type Enclosing_Element_Cursor is record
         Element : Program.Elements.Element_Access;
         Level   : Positive;
      end record;

      function Has_Enclosing_Element
        (Self : Enclosing_Element_Cursor) return Boolean;

      type Child_Cursor is tagged private;

      function Has_Element (Self : Child_Cursor) return Boolean;

      function Element
        (Self : Child_Cursor) return Program.Elements.Element_Access;

      function Property (Self : Child_Cursor) return Property_Name;
      --  Property name for element pointed by the cursor

      function Index (Self : Child_Cursor) return Positive;
      --  Index if the cursor in the middle of Element_Vector or 1 for single
      --  element.

      function Total (Self : Child_Cursor) return Positive;
      --  Vector length if the cursor in the middle of Element_Vector or 1 for
      --  single element.

      package Internal is

         procedure Step
           (Self   : Child_Iterator'Class;
            Cursor : in out Child_Cursor;
            Reset  : Boolean);

      end Internal;

   private

      type Child_Cursor is tagged record
         Element      : Program.Elements.Element_Access;
         Property     : Property_Name;
         Getter_Index : Positive;
         Item_Index   : Positive;
         Total_Items  : Positive;
      end record;

   end Cursors;

   package Enclosing_Element_Iterators is new Ada.Iterator_Interfaces
     (Cursors.Enclosing_Element_Cursor, Cursors.Has_Enclosing_Element);

   type Enclosing_Element_Iterator is
     new Enclosing_Element_Iterators.Forward_Iterator with private;

   function To_Enclosing_Element_Iterator
     (Parent : not null Program.Elements.Element_Access)
         return Enclosing_Element_Iterator;

   overriding function First
     (Self : Enclosing_Element_Iterator)
      return Cursors.Enclosing_Element_Cursor;

   overriding function Next
     (Self     : Enclosing_Element_Iterator;
      Position : Cursors.Enclosing_Element_Cursor)
      return Cursors.Enclosing_Element_Cursor;

   package Child_Iterators is
     new Ada.Iterator_Interfaces (Cursors.Child_Cursor, Cursors.Has_Element);

   type Child_Iterator is new Child_Iterators.Forward_Iterator with private;

   overriding function First
     (Self : Child_Iterator) return Cursors.Child_Cursor;

   overriding function Next
     (Self     : Child_Iterator;
      Position : Cursors.Child_Cursor) return Cursors.Child_Cursor;

   type Cursor_Checker is access
     function (Cursor : Cursors.Child_Cursor) return Boolean;

   function Only_If
     (Parent : Child_Iterator;
      Filter : not null Cursor_Checker)
      return Child_Iterator;

   type Element_Checker is access function
     (Element : not null Program.Elements.Element_Access) return Boolean;

   function Only_If
     (Parent : Child_Iterator;
      Filter : not null Element_Checker)
      return Child_Iterator;

   function To_Child_Iterator
     (Parent : not null Program.Elements.Element_Access;
      Filter : Element_Checker := null)
      return Child_Iterator;

private

   type Getter (Is_Vector : Boolean := False) is record

      Property : Property_Name;

      case Is_Vector is
         when False =>
            Get_Child : access function
              (Self : not null Program.Elements.Element_Access)
                return Program.Elements.Element_Access;

         when True =>
            Get_Vector : access function
              (Self : not null Program.Elements.Element_Access)
                return Program.Element_Vectors.Element_Vector_Access;

      end case;
   end record;

   type Getter_Array is array (Positive range <>) of Getter;
   Empty : aliased constant Getter_Array := (1 .. 0 => <>);

   type Enclosing_Element_Iterator is
     new Enclosing_Element_Iterators.Forward_Iterator with
   record
      First : Program.Elements.Element_Access;
   end record;

   type Checker_Chain (Is_Cursor : Boolean := False) is record
      case Is_Cursor is
         when True =>
            Cursor_Filter  : Cursor_Checker;
         when False =>
            Element_Filter : Element_Checker;
      end case;
   end record;

   type Checker_Chain_Array is array (Positive range <>) of Checker_Chain;

   function Check
     (Cursor : Cursors.Child_Cursor;
      List   : Checker_Chain_Array) return Boolean;

   type Child_Iterator is new Child_Iterators.Forward_Iterator with record
      Parent  : Program.Elements.Element_Access;
      Getters : access constant Getter_Array;
      Filter  : Checker_Chain_Array (1 .. 3);
      Last    : Natural := 0;
   end record;

end Program.Element_Iterators;
