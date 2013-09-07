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

   type Compilation_Unit_Cursor_Access is access all
     Gela.Compilation_Unit_Lists.Abstract_Compilation_Unit_Cursor'Class;

   type Compilation_Unit is record
      Object  : Compilation_Unit_Access;
      Payload : Gela.Types.Payload;
   end record;

   type Compilation_Unit_List is record
      Object  : Compilation_Unit_List_Access;
      Payload : Gela.Types.Payload;
   end record;

   type Compilation_Unit_Cursor is record
      Object  : Compilation_Unit_Cursor_Access;
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

   type Unit_Kinds is
     (A_Procedure,
      A_Function,
      A_Package,

      A_Generic_Procedure,
      A_Generic_Function,
      A_Generic_Package,

      A_Procedure_Instance,
      A_Function_Instance,
      A_Package_Instance,

      A_Procedure_Renaming,
      A_Function_Renaming,
      A_Package_Renaming,

      A_Generic_Procedure_Renaming,
      A_Generic_Function_Renaming,
      A_Generic_Package_Renaming,

      A_Procedure_Body,    --  A unit interpreted only as the completion
                           --  of a procedure, or a unit interpreted as
                           --  both the declaration and body of a library
                           --  procedure. Reference Manual 10.1.4(4)

      A_Function_Body,     --  A unit interpreted only as the completion
                           --  of a function, or a unit interpreted as
                           --  both the declaration and body of a library
                           --  function. Reference Manual 10.1.4(4)

      A_Package_Body,

      A_Procedure_Body_Subunit,
      A_Function_Body_Subunit,
      A_Package_Body_Subunit,
      A_Task_Body_Subunit,
      A_Protected_Body_Subunit,

      A_Nonexistent_Declaration,    --  A unit that does not exist but is:
                                    --  1) mentioned in a with clause of
                                    --     another unit or,
                                    --  2) a required corresponding
                                    --     library_unit_declaration

      A_Nonexistent_Body,           --  A unit that does not exist but is:
                                    --  1) known to be a corresponding
                                    --     subunit or,
                                    --  2) a required corresponding
                                    --     library_unit_body

      A_Configuration_Compilation,  --  Corresponds to the whole content of a
                                    --  compilation with no compilation_unit,
                                    --  but possibly containing comments,
                                    --  configuration pragmas, or both.
                                    --  A Context is not limited to the number
                                    --  of units of A_Configuration_Compilation
                                    --  kind. A unit of
                                    --  A_Configuration_Compilation does not
                                    --  have a name. This unit
                                    --  represents configuration pragmas that
                                    --  are "in effect". The only interface
                                    --   that returns this unit kind is
                                    --  Enclosing_Compilation_Unit when given
                                    --  A_Pragma element obtained from
                                    --  Configuration_Pragmas.

      An_Unknown_Unit);             --  An indeterminable or proprietary unit

   type Unit_Classes is
     (A_Public_Declaration,   --  library_unit_declaration or
                              --  library_unit_renaming_declaration.

      A_Public_Body,          --  library_unit_body interpreted only as a
                              --  completion.  Its declaration is public.

      A_Public_Declaration_And_Body,
                              --  subprogram_body interpreted as both a
                              --  declaration and body of a library
                              --  subprogram - Reference Manual 10.1.4(4).

      A_Private_Declaration,  --  private library_unit_declaration or
                              --  private library_unit_renaming_declaration.

      A_Private_Body,         --  library_unit_body interpreted only as a
                              --  completion.  Its declaration is private.

      A_Separate_Body);       --  separate (parent_unit_name) proper_body.

   type Unit_Origins is
      (A_Predefined_Unit,     --  Ada predefined language environment units
                              --  listed in Annex A(2).  These include
                              --  Standard and the three root library
                              --  units: Ada, Interfaces, and System,
                              --  and their descendants.  i.e., Ada.Text_Io,
                              --  Ada.Calendar, Interfaces.C, etc.

      An_Implementation_Unit,
                              --  Implementation specific library units,
                              --  e.g., runtime support packages, utility
                              --  libraries, etc. It is not required
                              --  that any implementation supplied units
                              --  have this origin.  This is a suggestion.
                              --  Implementations might provide, for
                              --  example, precompiled versions of public
                              --  domain software that could have
                              --  An_Application_Unit origin.

      An_Application_Unit);   --  Neither A_Predefined_Unit or
                              --  An_Implementation_Unit
end Gela.Types;
