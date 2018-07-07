--  This package provides Defining_Name_Cursor interface and its methods.
with Gela.Elements.Defining_Names;

package Gela.Defining_Name_Cursors is
   pragma Preelaborate;

   type Defining_Name_Cursor is interface;
   --  Cursor to iterate over defining name set.
   type Defining_Name_Cursor_Access is access all Defining_Name_Cursor'Class;
   for Defining_Name_Cursor_Access'Storage_Size use 0;

   not overriding function Has_Element
     (Self : Defining_Name_Cursor) return Boolean is abstract;
   --  Check if cursor pointing to item.

   not overriding function Element
     (Self : Defining_Name_Cursor)
      return Gela.Elements.Defining_Names.Defining_Name_Access is abstract;
   --  Return item pointed by cursor.

   not overriding procedure Next
     (Self : in out Defining_Name_Cursor) is abstract;
   --  Shift cursor to next item in set.

end Gela.Defining_Name_Cursors;
