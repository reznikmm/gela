with Ada.Iterator_Interfaces;

with Gela.Elements.Defining_Names;
with Gela.Lexical_Types;
with Gela.Semantic_Types;

package Gela.Plain_Int_Sets.Cursors is
   pragma Preelaborate;

   generic
      type Cursor is interface;
      with procedure Next (Self : in out Cursor) is abstract;
      type Some_Cursor is new Cursor with private;
      with package Iterators is new Ada.Iterator_Interfaces
        (Cursor'Class, Has_Element => <>);
   package Generic_Iterators is
      type Iterator is new Iterators.Forward_Iterator with record
         Cursor : Some_Cursor;
      end record;

      overriding function First (Self : Iterator) return Cursor'Class;

      overriding function Next
        (Self     : Iterator;
         Position : Cursor'Class) return Cursor'Class;

   end Generic_Iterators;

   type Category_Cursor is
     new Gela.Interpretations.Category_Cursor with private;

   not overriding procedure Initialize
     (Self  : out Category_Cursor;
      Set   : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Next (Self : in out Category_Cursor);

   type Defining_Name_Cursor is
     new Gela.Interpretations.Defining_Name_Cursor with private;

   not overriding procedure Initialize
     (Self  : out Defining_Name_Cursor;
      Set   : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Next (Self : in out Defining_Name_Cursor);

   overriding function Defining_Name
     (Self : Defining_Name_Cursor)
         return Gela.Elements.Defining_Names.Defining_Name_Access;

   type Expression_Cursor is
     new Gela.Interpretations.Expression_Cursor with private;

   not overriding procedure Initialize
     (Self  : out Expression_Cursor;
      Set   : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index);

   overriding function Expression_Type
     (Self : Expression_Cursor) return Gela.Semantic_Types.Type_Index;

   overriding procedure Next (Self : in out Expression_Cursor);

   type Symbol_Cursor is
     new Gela.Interpretations.Symbol_Cursor with private;

   not overriding procedure Initialize
     (Self  : out Symbol_Cursor;
      Set   : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Next (Self : in out Symbol_Cursor);

   type Profile_Cursor is
     new Gela.Interpretations.Profile_Cursor with private;

   not overriding procedure Initialize
     (Self  : out Profile_Cursor;
      Set   : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Next (Self : in out Profile_Cursor);

   type Any_Cursor is
     new Gela.Interpretations.Any_Cursor with private;

   not overriding procedure Initialize
     (Self  : out Any_Cursor;
      Set   : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Next (Self : in out Any_Cursor);

private

   type Base_Cursor is abstract new Gela.Interpretations.Abstract_Cursor
   with record
      Set : access Interpretation_Set;
      Pos : Int_Lists.Cursor := Int_Lists.No_Element;
   end record;

   overriding function Has_Element (Self : Base_Cursor) return Boolean;

   overriding function Get_Index
     (Self : Base_Cursor) return Gela.Interpretations.Interpretation_Index;

   type Category_Cursor is new Base_Cursor
     and Gela.Interpretations.Category_Cursor with null record;

   overriding function Matcher
     (Self : Category_Cursor)
         return Gela.Interpretations.Type_Matcher_Access;

   type Defining_Name_Cursor is new Base_Cursor
     and Gela.Interpretations.Defining_Name_Cursor with null record;

   type Expression_Cursor is new Base_Cursor
     and Gela.Interpretations.Expression_Cursor with null record;

   type Symbol_Cursor is new Base_Cursor
     and Gela.Interpretations.Symbol_Cursor with null record;

   overriding function Symbol
     (Self : Symbol_Cursor) return Gela.Lexical_Types.Symbol;

   type Profile_Cursor is new Base_Cursor
     and Gela.Interpretations.Profile_Cursor with null record;

   overriding function Corresponding_Type
     (Self : Profile_Cursor) return Gela.Semantic_Types.Type_Index;

   overriding function Attribute_Kind
     (Self : Profile_Cursor)
         return Gela.Lexical_Types.Predefined_Symbols.Attribute;

   type Any_Cursor is new Base_Cursor
     and Gela.Interpretations.Any_Cursor with null record;

   overriding function Is_Symbol (Self : Any_Cursor) return Boolean;

   overriding function Is_Defining_Name (Self : Any_Cursor) return Boolean;

   overriding function Is_Expression (Self : Any_Cursor) return Boolean;

   overriding function Is_Expression_Category (Self : Any_Cursor)
      return Boolean;

   overriding function Is_Profile (Self : Any_Cursor) return Boolean;

   overriding function Symbol
     (Self : Any_Cursor) return Gela.Lexical_Types.Symbol;

   overriding function Defining_Name (Self : Any_Cursor)
      return Gela.Elements.Defining_Names.Defining_Name_Access;

   overriding function Expression_Type (Self : Any_Cursor)
      return Gela.Semantic_Types.Type_Index;

   overriding function Matcher (Self : Any_Cursor)
        return Gela.Interpretations.Type_Matcher_Access;

   overriding function Corresponding_Type
     (Self : Any_Cursor) return Gela.Semantic_Types.Type_Index;

   overriding function Attribute_Kind (Self : Any_Cursor)
        return Gela.Lexical_Types.Predefined_Symbols.Attribute;

end Gela.Plain_Int_Sets.Cursors;
