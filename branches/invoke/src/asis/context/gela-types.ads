------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------
--  Package with common types

limited with Gela.Compilation_Unit_Lists;
limited with Gela.Compilation_Units;
limited with Gela.Compilations;
limited with Gela.Contexts;
limited with Gela.Symbol_Sets;
limited with Gela.Symbol_Tables;
limited with Gela.Tokens;
limited with Gela.Unit_Containers;
limited with Gela.Unit_Fabrics;

package Gela.Types is
   pragma Preelaborate;

   type Payload is mod 2 ** 32;
   --  Represent external payload for fly-weight objects

   type Payload_Array is array (Positive range <>) of Payload;

   type Context_Access is access all Gela.Contexts.Context'Class;

   type Container_Access is
     access all Gela.Unit_Containers.Unit_Container'Class;

   type Unit_Fabric_Access is access all Gela.Unit_Fabrics.Unit_Fabric'Class;

   type Compilation_Access is
     access all Gela.Compilations.Abstract_Compilation'Class;

   type Compilation_Unit_Access is
     access all Gela.Compilation_Units.Abstract_Compilation_Unit'Class;

   type Compilation_Unit_List_Access is access all
     Gela.Compilation_Unit_Lists.Abstract_Compilation_Unit_List'Class;

   type Compilation_Unit is record
      Object  : Compilation_Unit_Access;
      Payload : Gela.Types.Payload;
   end record;

   type Compilation_Unit_List is record
      Object  : Compilation_Unit_List_Access;
      Payload : Gela.Types.Payload;
   end record;

   type Symbol is mod 2 ** 31;
   --  Represent a record in symbol table. Reserve one bit for red/black flag

   type Abstract_Element is interface;
   type Element_Access is access all Abstract_Element'Class;

   type Element is record
      Object  : Element_Access;
      Payload : Gela.Types.Payload;
   end record;

   type Abstract_Defining_Name is interface and Abstract_Element;

   type Defining_Name_Access is access all Abstract_Defining_Name'Class;

   type Defining_Name is record
      Object  : Defining_Name_Access;
      Payload : Gela.Types.Payload;
   end record;

   type Symbol_Table_Access is
     access all Gela.Symbol_Tables.Abstract_Symbol_Table'Class;

   type Symbol_Table is record
      Object  : Symbol_Table_Access;
      Payload : Gela.Types.Payload;
   end record;

   type Token_Access is access all Gela.Tokens.Token'Class;

   type Token is record
      Object  : Token_Access;
      Payload : Gela.Types.Payload;
   end record;

   type Symbol_Set_Access is access all Gela.Symbol_Sets.Symbol_Set'Class;

   type Traverse_Control is
     (Continue,               --  Continues the normal depth-first traversal.
      Abandon_Children,       --  Prevents traversal of the current element's
                              --  children.
      Abandon_Siblings,       --  Prevents traversal of the current element's
                              --  children and remaining siblings.
      Terminate_Immediately); --  Does exactly that.

end Gela.Types;
