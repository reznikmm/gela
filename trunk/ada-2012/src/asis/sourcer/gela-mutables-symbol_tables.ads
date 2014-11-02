------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Containers.Vectors;

with Gela.Defining_Name_Cursors;
with Gela.Nodes;
with Gela.Symbol_Tables;
with Gela.Types;

package Gela.Mutables.Symbol_Tables is
   pragma Preelaborate;

   type Symbol_Table (Compilation : Mutable_Compilation_Access)
     is limited new Gela.Symbol_Tables.Abstract_Symbol_Table with private;

   not overriding procedure Create_Empty_Table
     (Self    : in out Symbol_Table;
      Payload : out Gela.Types.Payload);

   not overriding procedure Clone_Table
     (Self    : in out Symbol_Table;
      Payload : in out Gela.Types.Payload);

   not overriding procedure Append_Direct_Visible
     (Self    : in out Symbol_Table;
      Payload : Gela.Types.Payload;
      Symbol  : Gela.Types.Symbol;
      Value   : Gela.Nodes.Defining_Name);

   not overriding procedure Append_Region_Visible
     (Self    : in out Symbol_Table;
      Payload : Gela.Types.Payload;
      Region  : Gela.Nodes.Declarative_Region;
      Symbol  : Gela.Types.Symbol;
      Value   : Gela.Nodes.Defining_Name);

   not overriding procedure Append_Denote
     (Self      : in out Symbol_Table;
      Payload   : Gela.Types.Payload;
      Type_Name : Gela.Nodes.Defining_Name;
      Value     : Gela.Nodes.Defining_Name);

private

   type Direct_Visible_Cursor (Table : access Symbol_Table) is
     new Gela.Defining_Name_Cursors.Abstract_Defining_Name_Cursor
       with null record;

   overriding function Element
     (Self    : access Direct_Visible_Cursor;
      Payload : Gela.Types.Payload)
      return Gela.Nodes.Defining_Name;

   overriding function Next
     (Self    : access Direct_Visible_Cursor;
      Payload : Gela.Types.Payload)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor;

   type Region_Visible_Cursor (Table : access Symbol_Table) is
     new Gela.Defining_Name_Cursors.Abstract_Defining_Name_Cursor
       with null record;

   overriding function Element
     (Self    : access Region_Visible_Cursor;
      Payload : Gela.Types.Payload)
      return Gela.Nodes.Defining_Name;

   overriding function Next
     (Self    : access Region_Visible_Cursor;
      Payload : Gela.Types.Payload)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor;

   type Direct_Visible_Item_Index is new Natural;
   type Region_Visible_Item_Index is new Natural;
   type Denote_Item_Index is new Natural;

   type Direct_Visible_Item is record
      Next   : Direct_Visible_Item_Index;
      Symbol : Gela.Types.Symbol;
      Name   : Gela.Nodes.Defining_Name;
   end record;

   type Region_Visible_Item is record
      Next   : Region_Visible_Item_Index;
      Region : Gela.Nodes.Declarative_Region;
      Symbol : Gela.Types.Symbol;
      Name   : Gela.Nodes.Defining_Name;
   end record;

   type Denote_Item is record
      Next      : Denote_Item_Index;
      Type_Name : Gela.Nodes.Defining_Name;
      Name      : Gela.Nodes.Defining_Name;
   end record;

   package Direct_Visible_Item_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Direct_Visible_Item_Index,
      Element_Type => Direct_Visible_Item);

   package Region_Visible_Item_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Region_Visible_Item_Index,
      Element_Type => Region_Visible_Item);

   package Denote_Item_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Denote_Item_Index,
      Element_Type => Denote_Item);

   type Head_Record is record
      Direct : Direct_Visible_Item_Index;
      Region : Region_Visible_Item_Index;
      Denote : Denote_Item_Index;
   end record;

   package Head_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Head_Record);

   type Symbol_Table (Compilation : Mutable_Compilation_Access)
     is limited new Gela.Symbol_Tables.Abstract_Symbol_Table with
   record
      Direct_Cursor        : aliased Direct_Visible_Cursor
                                (Symbol_Table'Unchecked_Access);
      Region_Cursor        : aliased Region_Visible_Cursor
                                (Symbol_Table'Unchecked_Access);
      Free_Head            : Positive := 1;
      Head                 : Head_Vectors.Vector;
      Direct_Visible_Store : Direct_Visible_Item_Vectors.Vector;
      Region_Visible_Store : Region_Visible_Item_Vectors.Vector;
      Denote_Store         : Denote_Item_Vectors.Vector;
   end record;

   overriding function Direct_Visible
     (Self    : access Symbol_Table;
      Payload : Gela.Types.Payload;
      Symbol  : Gela.Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor;

   overriding function Visible
     (Self    : access Symbol_Table;
      Payload : Gela.Types.Payload;
      Region  : Gela.Nodes.Declarative_Region;
      Symbol  : Gela.Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor;

   overriding function Denote
     (Self      : access Symbol_Table;
      Payload   : Gela.Types.Payload;
      Type_Name : Gela.Nodes.Defining_Name)
      return Gela.Nodes.Defining_Name;

end Gela.Mutables.Symbol_Tables;
