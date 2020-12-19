--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Compilation_Unit_Vectors;
with Program.Contexts;
with Program.Source_Buffers;
with Program.Symbols;
with Program.Visibility;

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Vectors;
private with Program.Compilation_Units;
private with Program.Compilations;
private with Program.Unit_Naming;
private with Program.Library_Environments;
private with Program.Symbol_Lists;
private with Program.Symbols.Tables;

package Program.Plain_Contexts is
   pragma Preelaborate;

   type Context is limited new Program.Contexts.Context with private;

   procedure Initialize (Self : in out Context'Class);

   procedure Add_Search_Directory
     (Self : in out Context'Class;
      Path : Program.Text);

   procedure Find_Or_Create_Symbol
     (Self   : in out Context'Class;
      Buffer : not null Program.Source_Buffers.Source_Buffer_Access;
      Span   : Program.Source_Buffers.Span;
      Result : out Program.Symbols.Symbol);

   function Find
     (Self  : Context'Class;
      Value : Program.Text) return Program.Symbols.Symbol;

   procedure Parse_File
     (Self      : aliased in out Context'Class;
      Text_Name : Text);

   procedure Complete_Analysis (Self : in out Context'Class);

   overriding function Library_Unit_Declarations (Self : Context)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access;

   overriding function Compilation_Unit_Bodies (Self : Context)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access;

   function Immediate_Visible
     (Self : in out Context'Class;
      Unit : Program.Text;
      Name : Program.Text) return Program.Visibility.View_Array;

private

   type Symbol_Table is new Program.Symbols.Tables.Symbol_Table with record
      Lists : Program.Symbol_Lists.Symbol_List_Table
        (Symbol_Table'Unchecked_Access);
   end record;

   type Unit_Status is (Parsed, Loading, Analysed);
   --  Unit analysis status.
   --  Parsed - unit has been parsed
   --  Loading - loading of dependency units
   --  Analysed - unit has been analysed

   type Unit_Map_Item is record
      Status : Unit_Status;
      Unit   : Program.Compilation_Units.Compilation_Unit_Access;
   end record;

   package Unit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Program.Symbol_Lists.Symbol_List,
      Element_Type    => Unit_Map_Item,
      Hash            => Program.Symbol_Lists.Hash,
      Equivalent_Keys => Program.Symbol_Lists."=");

   package Unit_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Program.Symbol_Lists.Symbol_List,
      "="          => Program.Symbol_Lists."=");

   type Unit_Vector (Context : access Program.Plain_Contexts.Context) is
     limited new Program.Compilation_Unit_Vectors.Compilation_Unit_Vector with
   record
      Map  : Unit_Maps.Map;
      List : Unit_Vectors.Vector;
   end record;

   overriding function Get_Length (Self : Unit_Vector) return Positive;

   overriding function Element
     (Self  : Unit_Vector;
      Index : Positive)
        return not null Program.Compilation_Units.Compilation_Unit_Access;

   overriding function Find_Unit
     (Self  : Unit_Vector;
      Name  : Text)
        return Program.Compilation_Units.Compilation_Unit_Access;

   function Find_Unit
     (Self  : Unit_Vector'Class;
      Name  : Text) return Unit_Maps.Cursor;

   package Compilation_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Program.Compilations.Compilation_Access,
      "="          => Program.Compilations."=");

   type Context is limited new Program.Contexts.Context with record
      Symbols      : Symbol_Table;
      Library_Env  : aliased Program.Library_Environments.Library_Environment;
      Visible      : aliased Program.Visibility.Context;
      Declarations : aliased Unit_Vector (Context'Unchecked_Access);
      Bodies       : aliased Unit_Vector (Context'Unchecked_Access);
      Compilations : Compilation_Vectors.Vector;
      Naming       : Program.Unit_Naming.Unit_Naming_Schema_Access;
   end record;

   procedure Append_Unit
     (Self : in out Context'Class;
      Unit : Program.Compilation_Units.Compilation_Unit_Access);

end Program.Plain_Contexts;
