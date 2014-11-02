with Gela.Lexical_Types;
with Gela.Elements.Defining_Names;
with Gela.Peristent_Lists;
with Gela.Defining_Name_Cursors;

package Gela.Name_List_Managers is
   pragma Preelaborate;

   type Name_List_Manager is tagged private;

   type List is private;
   --  List contains (Symbol, Name) pairs
   function Empty_List (Self : Name_List_Manager) return List;

   procedure Append
     (Self   : in out Name_List_Manager;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Input  : List;
      Output : out List);
   --  Add (Symbol, Name) to Input return result as Output

   type Defining_Name_Cursor is
     new Gela.Defining_Name_Cursors.Defining_Name_Cursor with private;
   --  Cursor over names in a List with some Symbol

   function Symbol
     (Self : Defining_Name_Cursor) return Gela.Lexical_Types.Symbol;

   function Find
     (Self   : access Name_List_Manager'Class;
      Input  : List;
      Symbol : Gela.Lexical_Types.Symbol) return Defining_Name_Cursor;
   --  Find names in a Input list with given Symbol

   type Map is private;
   --  Map is mapping Name -> List
   function Empty_Map (Self : Name_List_Manager) return Map;

   procedure Append
     (Self   : in out Name_List_Manager;
      Key    : Gela.Elements.Defining_Names.Defining_Name_Access;
      Value  : List;
      Input  : Map;
      Output : out Map);
   --  Add (Key, Value) to Input return result as Output

private

   type Pair is record
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
   end record;

   package Pair_Peristent_Lists is new Gela.Peristent_Lists (Pair);

   type List is record
      Index : Pair_Peristent_Lists.Count_Type;
   end record;

   type Map is null record;

   type Name_List_Manager is tagged record
      Pair_List : Pair_Peristent_Lists.Container;
   end record;

   type Defining_Name_Cursor is
     new Gela.Defining_Name_Cursors.Defining_Name_Cursor with
      record
         Set    : access Name_List_Manager'Class;
         Name   : Pair_Peristent_Lists.Count_Type := 0;
      end record;

   overriding function Has_Element
     (Self : Defining_Name_Cursor) return Boolean;

   overriding function Element
     (Self : Defining_Name_Cursor)
         return Gela.Elements.Defining_Names.Defining_Name_Access;

   overriding procedure Next
     (Self : in out Defining_Name_Cursor);

   procedure Internal_Next
     (Self   : in out Defining_Name_Cursor;
      Symbol : Gela.Lexical_Types.Symbol);

end Gela.Name_List_Managers;
