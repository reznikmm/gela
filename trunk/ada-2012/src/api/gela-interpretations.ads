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

   type Expression_Flags is (No_Flag, Function_Call, Indexed_Component);

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
      Tipe   : Gela.Semantic_Types.Type_Index;
      Flag   : Gela.Interpretations.Expression_Flags :=
        Gela.Interpretations.No_Flag;
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
      Tipe   : Gela.Semantic_Types.Type_Index;
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
      Tipe   : Gela.Semantic_Types.Type_Index;
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
      Tipe   : Gela.Semantic_Types.Type_Index;
      Flag   : Gela.Interpretations.Expression_Flags;
      Down   : Gela.Interpretations.Interpretation_Index_Array) is null;
   --  Called for each expression interpretation

   not overriding procedure On_Expression_Category
     (Self   : in out Down_Visiter;
      Match  : not null Gela.Interpretations.Type_Matcher_Access;
      Down   : Gela.Interpretations.Interpretation_Index_Array) is null;
   --  Called for each category of expression interpretation

   not overriding procedure On_Attr_Function
     (Self   : in out Down_Visiter;
      Tipe   : Gela.Semantic_Types.Type_Index;
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

   type Cursor is limited interface;
   type Up_Visiter is tagged;

   not overriding function Has_Element (Self : Cursor) return Boolean is
     abstract;
   --  Check if cursor points to an interpretation

   not overriding procedure Next (Self : in out Cursor) is abstract;
   --  Go to next interpretation in set under cursor

   not overriding procedure Visit
     (Self   : Cursor;
      Target : access Up_Visiter'Class) is abstract;
   --  For current interpretation for cursor apply Target visiter

   not overriding function Get_Index
     (Self : Cursor) return Gela.Interpretations.Interpretation_Index
        is abstract;
   --  Request persistent index for current interpretation

   type Up_Visiter is limited interface;

   not overriding procedure On_Defining_Name
     (Self   : in out Up_Visiter;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Cursor : Gela.Interpretations.Cursor'Class) is null;
   --  Called for each defining name interpretation

   not overriding procedure On_Expression
     (Self   : in out Up_Visiter;
      Tipe   : Gela.Semantic_Types.Type_Index;
      Cursor : Gela.Interpretations.Cursor'Class) is null;
   --  Called for each expression interpretation

   not overriding procedure On_Expression_Category
     (Self   : in out Up_Visiter;
      Match  : not null Gela.Interpretations.Type_Matcher_Access;
      Cursor : Gela.Interpretations.Cursor'Class) is null;
   --  Called for each category of expression interpretation

   not overriding procedure On_Attr_Function
     (Self   : in out Up_Visiter;
      Tipe   : Gela.Semantic_Types.Type_Index;
      Kind   : Gela.Lexical_Types.Predefined_Symbols.Attribute;
      Cursor : Gela.Interpretations.Cursor'Class) is null;
   --  Called for each attribute denoting function

   not overriding procedure On_Placeholder
     (Self   : in out Up_Visiter;
      Kind   : Gela.Interpretations.Placeholder_Kind;
      Cursor : Gela.Interpretations.Cursor'Class) is null;
   --  Called for each placeholder

   not overriding procedure On_Symbol
     (Self   : in out Up_Visiter;
      Symbol : Gela.Lexical_Types.Symbol;
      Cursor : Gela.Interpretations.Cursor'Class) is null;
   --  Called for each symbol

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

   not overriding function Get_Cursor
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
      return Gela.Interpretations.Cursor'Class is abstract;
   --  Get cursor to iterate over all interpretations in Set

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

end Gela.Interpretations;
