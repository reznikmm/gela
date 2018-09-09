with Ada.Iterator_Interfaces;

--  This package provides Interpretation_Manager interface and its methods.
with Gela.Elements.Defining_Names;
with Gela.Lexical_Types;
with Gela.Semantic_Types;
with Gela.Types.Visitors;

package Gela.Interpretations is
   pragma Preelaborate;

   type Interpretation_Index is new Natural;
   --  Index of interpretation inside an instance of manager

   type Interpretation_Set_Index is new Natural;
   --  Index of set of interpretation inside an instance of manager

   type Interpretation_Tuple_Index is new Natural;
   --  Index of tuple of interpretation inside an instance of manager

   type Interpretation_Tuple_List_Index is new Natural;
   --  Index of interpretation tuple list (tuple of tuples) inside an instance

   type Interpretation_Index_Array is array (Positive range <>) of
      Interpretation_Index;

   type Interpretation_Set_Index_Array is array (Positive range <>) of
      Interpretation_Set_Index;

   type Interpretation_Tuple_Index_Array is array (Positive range <>) of
      Interpretation_Tuple_Index;

   type Interpretation_Kinds is
     (Unknown,
      --  Interpretation of Identifier
      Identifier,
      --  Interpretation of Auxiliary_Apply
      Function_Call,
      Type_Convertion,
      Indexed_Component,
      --  Interpretation of Composite_Constraint
      Index_Constraint,
      Discriminant_Constraint,
      --  Interpretation of number_declaration
      Integer_Number,
      Real_Number);

   subtype Auxiliary_Apply_Kinds is Interpretation_Kinds
     range Function_Call .. Indexed_Component;

   subtype Unknown_Auxiliary_Apply_Kinds is Interpretation_Kinds
     range Unknown .. Indexed_Component;

   subtype Constraint_Kinds is Interpretation_Kinds
     range Index_Constraint .. Discriminant_Constraint;

   subtype Number_Declaration_Kinds is Interpretation_Kinds
     range Integer_Number .. Real_Number;

   type Interpretation_Manager is limited interface;
   --  This object keeps sets of possible interpretations
   type Interpretation_Manager_Access is
     access all Interpretation_Manager'Class;
   for Interpretation_Manager_Access'Storage_Size use 0;

   type Type_Matcher is limited interface and Gela.Types.Visitors.Type_Visitor;
   type Type_Matcher_Access is access all Type_Matcher'Class;
   for Type_Matcher_Access'Storage_Size use 0;

   not overriding function Is_Matched
     (Self : Type_Matcher) return Boolean is abstract;

   not overriding procedure Add_Symbol
     (Self   : in out Interpretation_Manager;
      Symbol : Gela.Lexical_Types.Symbol;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
        is abstract;
   --  Extend Result with new interpretation as symbol

   not overriding procedure Add_Defining_Name
     (Self   : in out Interpretation_Manager;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
        is abstract;
   --  Extend Result with new interpretation of defining Name

   not overriding procedure Add_Expression
     (Self   : in out Interpretation_Manager;
      Tipe   : Gela.Semantic_Types.Type_View_Index;
      Kind   : Gela.Interpretations.Unknown_Auxiliary_Apply_Kinds :=
        Gela.Interpretations.Unknown;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
        is abstract;
   --  Extend Result with new interpretation of expression with given Type

   not overriding procedure Add_Expression_Category
     (Self   : in out Interpretation_Manager;
      Match  : not null Gela.Interpretations.Type_Matcher_Access;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
        is abstract;
   --  Extend Result with new interpretation of expression in given categories

   not overriding procedure Add_Attr_Function
     (Self   : in out Interpretation_Manager;
      Tipe   : Gela.Semantic_Types.Type_View_Index;
      Kind   : Gela.Lexical_Types.Predefined_Symbols.Attribute;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
        is abstract;
   --  Extend Result with new interpretation of attribute denoting function

   not overriding procedure Add_Tuple
     (Self   : in out Interpretation_Manager;
      Left   : Gela.Interpretations.Interpretation_Set_Index;
      Right  : Gela.Interpretations.Interpretation_Tuple_Index;
      Result : out Gela.Interpretations.Interpretation_Tuple_Index)
        is abstract;
   --  If Right = 0 then create new tuple with single element <Left>.
   --  Otherwise create new interpretation tuple as <Left, Right-tuple>

   not overriding procedure Add_Tuple_List
     (Self   : in out Interpretation_Manager;
      Left   : Gela.Interpretations.Interpretation_Tuple_Index;
      Right  : Gela.Interpretations.Interpretation_Tuple_List_Index;
      Result : out Gela.Interpretations.Interpretation_Tuple_List_Index)
        is abstract;
   --  If Right = 0 then create new tuple with single element <Left>.
   --  Otherwise create new interpretation tuple as <Left, Right-tuple>

   not overriding procedure Get_Tuple_Index
     (Self   : in out Interpretation_Manager;
      Left   : Gela.Interpretations.Interpretation_Index;
      Right  : Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Interpretations.Interpretation_Index) is abstract;
   --  Register chosen tuple interpretation

   not overriding procedure Get_Defining_Name_Index
     (Self   : in out Interpretation_Manager;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Result : out Gela.Interpretations.Interpretation_Index) is abstract;
   --  Register chosen defining_name interpretation

   not overriding procedure Get_Expression_Index
     (Self   : in out Interpretation_Manager;
      Tipe   : Gela.Semantic_Types.Type_View_Index;
      Result : out Gela.Interpretations.Interpretation_Index) is abstract;
   --  Register chosen expression interpretation

   type Placeholder_Kind is (Absent);

   not overriding procedure Add_Placeholder
     (Self   : in out Interpretation_Manager;
      Kind   : Gela.Interpretations.Placeholder_Kind;
      Result : in out Gela.Interpretations.Interpretation_Set_Index)
        is abstract;
   --  Placeholder interpretation represents some syntax construct.
   --  This eliminates need to traverse syntax tree in some situation

   type Down_Visiter is limited interface;

   not overriding procedure On_Defining_Name
     (Self   : in out Down_Visiter;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Down   : Gela.Interpretations.Interpretation_Index_Array) is null;
   --  Called for each defining name interpretation

   not overriding procedure On_Expression
     (Self   : in out Down_Visiter;
      Tipe   : Gela.Semantic_Types.Type_View_Index;
      Kind   : Gela.Interpretations.Unknown_Auxiliary_Apply_Kinds;
      Down   : Gela.Interpretations.Interpretation_Index_Array) is null;
   --  Called for each expression interpretation

   not overriding procedure On_Expression_Category
     (Self   : in out Down_Visiter;
      Match  : not null Gela.Interpretations.Type_Matcher_Access;
      Down   : Gela.Interpretations.Interpretation_Index_Array) is null;
   --  Called for each category of expression interpretation

   not overriding procedure On_Attr_Function
     (Self   : in out Down_Visiter;
      Tipe   : Gela.Semantic_Types.Type_View_Index;
      Kind   : Gela.Lexical_Types.Predefined_Symbols.Attribute;
      Down   : Gela.Interpretations.Interpretation_Index_Array) is null;
   --  Called for each attribute denoting function

   not overriding procedure On_Placeholder
     (Self   : in out Down_Visiter;
      Kind   : Gela.Interpretations.Placeholder_Kind;
      Down   : Gela.Interpretations.Interpretation_Index_Array) is null;
   --  Called for each placeholder

   not overriding procedure On_Tuple
     (Self   : in out Down_Visiter;
      Down   : Gela.Interpretations.Interpretation_Index_Array) is null;
   --  Called for each tuple. FIXME: replace with (head, tail) ???

   not overriding procedure Visit
     (Self   : in out Interpretation_Manager;
      Index  : Gela.Interpretations.Interpretation_Index;
      Target : in out Down_Visiter'Class) is abstract;
   --  For interpretation with given persistent Index apply Target visiter

   not overriding function Get_Tuple
     (Self   : in out Interpretation_Manager;
      Index  : Gela.Interpretations.Interpretation_Tuple_Index)
      return Gela.Interpretations.Interpretation_Set_Index_Array is abstract;
   --  Get tuple elements

   not overriding function Get_Tuple_List
     (Self   : in out Interpretation_Manager;
      Index  : Gela.Interpretations.Interpretation_Tuple_List_Index)
      return Gela.Interpretations.Interpretation_Tuple_Index_Array is abstract;
   --  Get tuple list elements

   not overriding procedure Get_Down_Interpretation
     (Self     : in out Interpretation_Manager;
      Value    : Gela.Interpretations.Interpretation_Index;
      Index    : Positive;
      Result   : out Gela.Interpretations.Interpretation_Index) is abstract;
   --  Return interpretation from which Value was derived

   not overriding procedure Get_Defining_Name
     (Self   : in out Interpretation_Manager;
      Value  : Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Elements.Defining_Names.Defining_Name_Access)
        is abstract;

   --  Base type for iterating cursors
   type Abstract_Cursor is interface;

   not overriding function Has_Element
     (Self : Abstract_Cursor) return Boolean is abstract;

   not overriding procedure Next (Self : in out Abstract_Cursor) is abstract;
   --  Go to next interpretation in set under cursor

   not overriding function Get_Index
     (Self : Abstract_Cursor)
      return Gela.Interpretations.Interpretation_Index is abstract;
   --  Request persistent index for current interpretation

   --  Iterating over symbol interpretation
   type Symbol_Cursor is interface and Abstract_Cursor;

   not overriding function Symbol
     (Self : Symbol_Cursor)
      return Gela.Lexical_Types.Symbol is abstract;

   function Has_Some
     (Self : Symbol_Cursor'Class) return Boolean is (Self.Has_Element);

   package Symbol_Iterators is new Ada.Iterator_Interfaces
     (Symbol_Cursor'Class, Has_Some);

   not overriding function Symbols
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
        return Symbol_Iterators.Forward_Iterator'Class is abstract;

   --  Iterating over defining name interpretation
   type Defining_Name_Cursor is interface and Abstract_Cursor;

   not overriding function Defining_Name
     (Self : Defining_Name_Cursor)
      return Gela.Elements.Defining_Names.Defining_Name_Access is abstract;

   function Has_Some
     (Self : Defining_Name_Cursor'Class) return Boolean is (Self.Has_Element);

   package Defining_Name_Iterators is new Ada.Iterator_Interfaces
     (Defining_Name_Cursor'Class, Has_Some);

   not overriding function Defining_Names
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
        return Defining_Name_Iterators.Forward_Iterator'Class is abstract;

   --  Iterating over expression interpretation
   type Expression_Cursor is interface and Abstract_Cursor;

   not overriding function Expression_Type
     (Self : Expression_Cursor)
      return Gela.Semantic_Types.Type_View_Index is abstract;

   function Has_Some
     (Self : Expression_Cursor'Class) return Boolean is (Self.Has_Element);

   package Expression_Iterators is new Ada.Iterator_Interfaces
     (Expression_Cursor'Class, Has_Some);

   not overriding function Expressions
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
        return Expression_Iterators.Forward_Iterator'Class is abstract;

   --  Iterating over expression category interpretation
   type Category_Cursor is interface and Abstract_Cursor;

   not overriding function Matcher
     (Self : Category_Cursor)
        return Gela.Interpretations.Type_Matcher_Access is abstract;

   function Has_Some
     (Self : Category_Cursor'Class) return Boolean is (Self.Has_Element);

   package Category_Iterators is new Ada.Iterator_Interfaces
     (Category_Cursor'Class, Has_Some);

   not overriding function Categories
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
        return Category_Iterators.Forward_Iterator'Class is abstract;

   --  Iterating over attribute denoting subprogram interpretation
   type Profile_Cursor is interface and Abstract_Cursor;

   not overriding function Corresponding_Type
     (Self : Profile_Cursor) return Gela.Semantic_Types.Type_View_Index
      is abstract;

   not overriding function Attribute_Kind
     (Self : Profile_Cursor)
        return Gela.Lexical_Types.Predefined_Symbols.Attribute is abstract;

   function Has_Some
     (Self : Profile_Cursor'Class) return Boolean is (Self.Has_Element);

   package Profile_Iterators is new Ada.Iterator_Interfaces
     (Profile_Cursor'Class, Has_Some);

   not overriding function Profiles
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
        return Profile_Iterators.Forward_Iterator'Class is abstract;

   --  Iterating over any interpretation
   type Any_Cursor is interface and Abstract_Cursor;

   function Has_Some
     (Self : Any_Cursor'Class) return Boolean is (Self.Has_Element);

   not overriding function Is_Symbol
     (Self : Any_Cursor) return Boolean is abstract;

   not overriding function Is_Defining_Name (Self : Any_Cursor)
      return Boolean is abstract;

   not overriding function Is_Expression (Self : Any_Cursor)
      return Boolean is abstract;

   not overriding function Is_Expression_Category (Self : Any_Cursor)
      return Boolean is abstract;

   not overriding function Is_Profile (Self : Any_Cursor)
      return Boolean is abstract;

   not overriding function Symbol
     (Self : Any_Cursor) return Gela.Lexical_Types.Symbol is abstract;

   not overriding function Defining_Name (Self : Any_Cursor)
      return Gela.Elements.Defining_Names.Defining_Name_Access is abstract;

   not overriding function Expression_Type (Self : Any_Cursor)
      return Gela.Semantic_Types.Type_View_Index is abstract;

   not overriding function Matcher (Self : Any_Cursor)
        return Gela.Interpretations.Type_Matcher_Access is abstract;

   not overriding function Corresponding_Type
     (Self : Any_Cursor) return Gela.Semantic_Types.Type_View_Index
        is abstract;

   not overriding function Attribute_Kind (Self : Any_Cursor)
        return Gela.Lexical_Types.Predefined_Symbols.Attribute is abstract;

   package Any_Iterators is new Ada.Iterator_Interfaces
     (Any_Cursor'Class, Has_Some);

   not overriding function Each
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
        return Any_Iterators.Forward_Iterator'Class is abstract;
   --  Get cursor to iterate over all interpretations in Set

end Gela.Interpretations;