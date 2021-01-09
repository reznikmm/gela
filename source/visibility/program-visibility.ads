--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------
--
--  This package is about Ada Visibility Rules as they defined in the Reference
--  Manual (Section 8).
--
--  The package provides Context type. The user populates context by creating
--  named entities and declarative regions. The user also queries the context
--  to find the view corresponding to given symbol (identifier, operator or
--  character literal).
--
--  View provides access to defining name nodes, entity kind and properties.

with Ada.Iterator_Interfaces;
private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Vectors;

with Program.Elements.Defining_Names;
with Program.Symbols;

package Program.Visibility is
   pragma Preelaborate;

   subtype Defining_Name is
     Program.Elements.Defining_Names.Defining_Name_Access;
   --  Defining name AST node

   subtype Symbol is Program.Symbols.Symbol;
   --  A representation of an identifier, operator or character literal

   function Standard return Symbol renames Program.Symbols.Standard;
   --  Symbol of Standard Ada package

   type View_Kind is
     (Unresolved_View,
      Enumeration_Type_View,
      Signed_Integer_Type_View,
      Modular_Type_View,
      Float_Point_Type_View,
      Array_Type_View,
      Implicit_Type_View,
      Enumeration_Literal_View,
      Character_Literal_View,
      Subtype_View,
      Exception_View,
      Parameter_View,
      Procedure_View,
      Package_View);
   --  Kind of entity view

   subtype Type_View_Kind is View_Kind
     range Enumeration_Type_View .. Implicit_Type_View;
   --  Kind of type view

   type View (Kind : View_Kind := Unresolved_View) is private;
   --  An information about a program entity

   type View_Array is array (Positive range <>) of View;
   --  Array of views

   function Name (Self : View) return Defining_Name;
   --  Get defining name of the entity

   function Has_Region (Self : View) return Boolean;
   --  Check if given entity could contain nested declarative region

   function Enumeration_Type (Self : View) return View
     with Pre => Self.Kind in
       Enumeration_Literal_View | Character_Literal_View;
   --  Return enumeration type for given enumeration or character literal

   function Enumeration_Literals (Self : View) return View_Array
     with Pre => Self.Kind = Enumeration_Type_View;
   --  Return enumeration or character literals for given enumeration type

   function Is_Character_Type (Self : View) return Boolean
     with Pre => Self.Kind = Enumeration_Type_View;
   --  If given enumeration type is a character type

   function Subtype_Mark (Self : View) return View
     with Pre => Self.Kind in Subtype_View | Parameter_View;
   --  Return type of subtype, parameter declaration

   function Has_Constraint (Self : View) return Boolean
     with Pre => Self.Kind = Subtype_View;
   --  If given subtype has a constraint

   function Indexes (Self : View) return View_Array
     with Pre => Self.Kind = Array_Type_View;
   --  Return index types for given array type

   function Component (Self : View) return View
     with Pre => Self.Kind = Array_Type_View;
   --  Return component type for given array type

   type Parameter_Mode is (In_Mode, In_Out_Mode, Out_Mode);

   function Mode (Self : View) return Parameter_Mode
     with Pre => Self.Kind = Parameter_View;
   --  Return Mode of parameter declaration

   function Has_Default (Self : View) return Boolean
     with Pre => Self.Kind = Parameter_View;
   --  Check if parameter has a default value

   function Parameters (Self : View) return View_Array
     with Pre => Self.Kind = Procedure_View;
   --  Return parameters for given subprogram

   type View_Cursor is private;
   --  A cursor to iterate over visible views

   function Get_View (Self : View_Cursor) return View;
   --  Get a view corresponding to the cursor

   function "+" (Self : View_Cursor) return View renames Get_View;
   --  Get a view corresponding to the cursor

   function Has_Element (Self : View_Cursor) return Boolean;
   --  Check if cursor still points to a view

   package Iterators is new Ada.Iterator_Interfaces (View_Cursor, Has_Element);
   subtype View_Iterator is Iterators.Forward_Iterator'Class;

   function Immediate_Visible
     (Self   : View;
      Symbol : Program.Visibility.Symbol)
        return View_Iterator
          with Pre => Has_Region (Self);
   --  Return iterator of views for immediate visible names with given symbol

   function Region_Items (Self : View) return View_Array
     with Pre => Has_Region (Self);
   --  Return array of views for immediate visible names

   type Snapshot is tagged limited private;
   --  Snapshot keeps state of a context. We save snapshots for private
   --  and public parts of entities.

   type Snapshot_Access is access all Snapshot'Class
     with Storage_Size => 0;

   type Context is tagged limited private;
   --  A context keeps map from symbol to its view. It also tracks set of
   --  snapshots.

   type Context_Access is access all Program.Visibility.Context'Class
     with Storage_Size => 0;

   not overriding procedure Create_Empty_Context
     (Self : in out Context);
   --  Initialize a context to empty state before loading Standard package

   not overriding function Create_Snapshot
     (Self    : in out Context) return Snapshot_Access;
   --  Store state of the context into a snapshot

   not overriding procedure Restore_Snapshot
     (Self     : in out Context;
      Snapshot : not null Snapshot_Access);
   --  Restore snapshot. For example before leaving a package, restore
   --  the snapshot of its public part.

   not overriding procedure Enter_Snapshot
     (Self     : in out Context;
      Snapshot : not null Snapshot_Access);
   --  Take topmost element of the snapshot and enter its declarative region.
   --  Use-case example:
   --
   --  declare
   --     package P is
   --        type T is private;
   --        procedure Proc (X : T);
   --     private                     -->  Public_Snap := Create_Snapshot;
   --        type T is new Integer;
   --     end P;                      -->  Private_Snap := Create_Snapshot;
   --
   --  -->  Restore_Snapshot (Public_Snap); Leave_Declaration;
   --  V : P.T;
   --     package body P is  -->  Enter_Snapshot (Private_Snap);
   --

--   not overriding procedure Start_Private_Part
--     (Self     : in out Context;
--      Snapshot : not null Snapshot_Access);
   --  Make private declarations visible. Current "point of view" should be
   --  in a public part of a library unit. Snapshot should be taken from the
   --  parent of the current unit.
   --  Use-case example:
   --
   --  package P is
   --     type T is private;
   --  private                     -->  Public_Snap := Create_Snapshot;
   --     type T is new Integer;
   --  end P;                      -->  Private_Snap := Create_Snapshot;
   --
   --  -->  Restore_Snapshot (Public_Snap); Leave_Declaration;
   --  package P.Q is              -->  Create_Package (P.Q)
   --     V : T;
   --  private                     -->  Start_Private_Part (Private_Snap);
   --  -->  Now we see that T is integer and its oprerations like "+", "/"
   --

   not overriding procedure Create_Implicit_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add an implicit type view to the context. Don't create a region.

   not overriding procedure Create_Enumeration_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add an enumeration type view to the context. Don't create a region.

   not overriding procedure Create_Enumeration_Literal
     (Self             : in out Context;
      Symbol           : Program.Visibility.Symbol;
      Name             : Defining_Name;
      Enumeration_Type : View)
        with Pre => Enumeration_Type.Kind = Enumeration_Type_View;
   --  Add an enumeration literal view to the context. Don't create a region.

   not overriding procedure Create_Character_Literal
     (Self             : in out Context;
      Symbol           : Program.Visibility.Symbol;
      Name             : Defining_Name;
      Enumeration_Type : View)
        with Pre => Enumeration_Type.Kind = Enumeration_Type_View;
   --  Add a character literal view to the context. Don't create a region.

   type Meta_Character_Literal_Kind is
     (Meta_Character, Meta_Wide_Character, Meta_Wide_Wide_Character);
   --  Meta character literal matches any character name in its class.
   --  We use them to avoid a million of defining names in the context.

   not overriding procedure Create_Character_Literal
     (Self             : in out Context;
      Symbol           : Program.Visibility.Symbol;
      Name             : Defining_Name;
      Meta_Character   : Meta_Character_Literal_Kind;
      Enumeration_Type : View)
        with Pre => Enumeration_Type.Kind = Enumeration_Type_View;
   --  Add a meta character literal view to the context. Don't create a region.

   not overriding procedure Create_Signed_Integer_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add a signed integer type view to the context. Create a region.

   not overriding procedure Create_Modular_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add a unsigned integer type view to the context. Create a region.

   not overriding procedure Create_Float_Point_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add a float point type view to the context. Create a region.

   not overriding procedure Create_Array_Type
     (Self      : in out Context;
      Symbol    : Program.Visibility.Symbol;
      Name      : Defining_Name;
      Indexes   : View_Array;
      Component : View)
        with Pre => Component.Kind in Type_View_Kind;
   --  Add an array type view to the context. Create a region.

   not overriding procedure Create_Subtype
     (Self           : in out Context;
      Symbol         : Program.Visibility.Symbol;
      Name           : Defining_Name;
      Subtype_Mark   : View;
      Has_Constraint : Boolean)
        with Pre => Subtype_Mark.Kind in Type_View_Kind;
   --  Add a subtype view to the context. Create a region.

   not overriding procedure Create_Package
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add an empty package view to the context. Create a region.

   not overriding procedure Create_Procedure
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add a procedure view to the context. Create declarative region.

   not overriding procedure Create_Parameter
     (Self        : in out Context;
      Symbol      : Program.Visibility.Symbol;
      Name        : Defining_Name;
      Mode        : Parameter_Mode;
      Has_Default : Boolean);
   --  Add a parameter view to the context and to the topmost subprogram
   --  declaration. Create declarative region.

   not overriding procedure Set_Parameter_Type
     (Self       : in out Context;
      Definition : View);
   --  Assign given subtype to the topmost parameter declaration

   not overriding procedure Create_Exception
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add an exception view to the context. Don't create a region.

   not overriding procedure Leave_Declarative_Region (Self : in out Context);
   --  Leave current declarative region the context.

   not overriding procedure Add_Use_Package
     (Self : in out Context;
      Pkg  : View);
   --  Add use package clause to the context.

   not overriding function Immediate_Visible
     (Self   : Context;
      Symbol : Program.Visibility.Symbol)
        return View_Iterator;
   --  Return iterator of views for immediate visible names with given symbol

   not overriding function Use_Visible
     (Self   : Context;
      Symbol : Program.Visibility.Symbol)
        return View_Iterator;
   --  Return iterator of views for use visible names with given symbol

   not overriding function Latest_View (Self : Context) return View;
   --  View that was added to the context

   not overriding function Get_Name_View
     (Self : Context;
      Name : not null Program.Elements.Element_Access) return View;

private

   type Entity_Identifier is range 1 .. Integer'Last;
   type Region_Identifier is range 1 .. Integer'Last;

   type Entity_Reference is record
      Region    : Region_Identifier;
      Entity_Id : Entity_Identifier;
   end record;

   package Entity_References is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Entity_Reference);

   subtype Has_Region_Kind is View_Kind range Procedure_View .. Package_View;

   type Entity (Kind : View_Kind := Package_View) is record
      Symbol    : Program.Visibility.Symbol;
      Name      : Defining_Name;
      --  Entity_Id : Entity_Identifier;
      --  The index of the item from which we copied this item

      case Kind is
         when Has_Region_Kind =>
            Region : Region_Identifier;
            --  If Item has nested region, it's region index
         when others =>
            case Kind is
               when Enumeration_Type_View =>
                  Is_Character_Type : Boolean;
                  First_Literal     : Entity_Identifier;
                  Last_Literal      : Entity_Identifier;
                  --  indexes of its enumeration literals
               when Enumeration_Literal_View =>
                  Enumeration_Type : Entity_Identifier;
               when Character_Literal_View =>
                  Character_Type : Entity_Identifier;
               when Subtype_View =>
                  Subtype_Mark   : Entity_Reference;
                  Has_Constraint : Boolean;
               when Array_Type_View =>
                  Indexes   : Entity_References.Vector;
                  Component : Entity_Reference;
               when Parameter_View =>
                  Param_Def   : Entity_Reference;
                  Mode        : Parameter_Mode;
                  Has_Default : Boolean;
               when others =>
                  null;
            end case;
      end case;
   end record;

   package Entity_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Entity_Identifier,
      Element_Type => Entity);

   package Region_Id_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Region_Identifier);

   type Snapshot is tagged limited record
      Region_Id : Region_Identifier; --  Ignore Region_Id if Entities is empty
      Entities  : Entity_Vectors.Vector;
      Uses      : Region_Id_Vectors.Vector;
   end record;

   function Hash
     (Value : Program.Elements.Defining_Names.Defining_Name_Access)
       return Ada.Containers.Hash_Type;

   package Defining_Name_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Program.Elements.Defining_Names.Defining_Name_Access,
      Element_Type    => Entity_Reference,
      Hash            => Hash,
      Equivalent_Keys => Program.Elements.Defining_Names."=",
      "="             => "=");

   type Region is record
      Enclosing : Region_Identifier'Base;
      Entities  : Entity_Vectors.Vector;
      Uses      : Region_Id_Vectors.Vector;
   end record;

   package Region_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Region_Identifier,
      Element_Type => Region);

   type Context is tagged limited record
      Data : Region_Vectors.Vector;
      --  All items are stored here
      Top  : Region_Identifier;
      --  Current region
      Xref : Defining_Name_Maps.Map;
      --  For each defining name a corresponding reference
   end record;

   type Constant_Context_Access is access constant Context;

   type View (Kind : View_Kind := Unresolved_View) is record
      Env   : Constant_Context_Access;
      Index : Entity_Reference;
   end record;

   type View_Cursor is record
      Region : Region_Identifier;
      Entity : Entity_Identifier'Base;
      Use_Id : Positive;
      View   : Program.Visibility.View;
   end record;

end Program.Visibility;
