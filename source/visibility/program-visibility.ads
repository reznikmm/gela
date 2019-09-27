--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
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
     (Enumeration_Type_View,
      Signed_Integer_Type_View,
      Modular_Type_View,
      Float_Point_Type_View,
      Array_Type_View,
      Implicit_Type_View,
      Enumeration_Literal_View,
      Character_Literal_View,
      Subtype_View,
      Exception_View,
      Package_View);
   --  Kind of entity view

   subtype Type_View_Kind is View_Kind
     range Enumeration_Type_View .. Implicit_Type_View;
   --  Kind of type view

   type View (Kind : View_Kind := Package_View) is private;
   --  An information about a program entity

   type View_Array is array (Positive range <>) of  View;
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
     with Pre => Self.Kind = Subtype_View;
   --  Return type of subtype declaration

   function Has_Constraint (Self : View) return Boolean
     with Pre => Self.Kind = Subtype_View;
   --  If given subtype has a constraint

   function Indexes (Self : View) return View_Array
     with Pre => Self.Kind = Array_Type_View;
   --  Return index types for given array type

   function Component (Self : View) return View
     with Pre => Self.Kind = Array_Type_View;
   --  Return component type for given array type

   function Immediate_Visible
     (Self   : View;
      Symbol : Program.Visibility.Symbol) return View_Array
        with Pre => Has_Region (Self);
   --  Return array of views for immediate visible names with given symbol

   type Snapshot is tagged limited private;
   --  Snapshot keeps state of a context. We save snapshots for private
   --  and public parts of entities.

   type Snapshot_Access is access all Snapshot'Class
     with Storage_Size => 0;

   type Context is tagged limited private;
   --  A context keeps map from symbol to its view. It also tracks set of
   --  snapshots.

   not overriding procedure Create_Empty_Context
     (Self : aliased in out Context);
   --  Initialize a context to empty state before loading Standard package

   not overriding function Create_Snapshot
     (Self    : aliased in out Context) return Snapshot_Access;
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
   --  Add an implicit type view to the context.

   not overriding procedure Create_Enumeration_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add an enumeration type view to the context.

   not overriding procedure Create_Enumeration_Literal
     (Self             : in out Context;
      Symbol           : Program.Visibility.Symbol;
      Name             : Defining_Name;
      Enumeration_Type : View)
        with Pre => Enumeration_Type.Kind = Enumeration_Type_View;
   --  Add an enumeration literal view to the context.

   not overriding procedure Create_Character_Literal
     (Self             : in out Context;
      Symbol           : Program.Visibility.Symbol;
      Name             : Defining_Name;
      Enumeration_Type : View)
        with Pre => Enumeration_Type.Kind = Enumeration_Type_View;
   --  Add a character literal view to the context.

   type Meta_Character_Literal_Kind is
     (Meta_Character, Meta_Wide_Character, Meta_Wide_Wide_Character);
   --  Meta character literal matches any character name in its class.
   --  We use them to avoid a million of defining names in the context.

   not overriding procedure Create_Character_Literal
     (Self             : in out Context;
      Symbol           : Program.Visibility.Symbol;
      Meta_Character   : Meta_Character_Literal_Kind;
      Enumeration_Type : View)
        with Pre => Enumeration_Type.Kind = Enumeration_Type_View;
   --  Add a meta character literal view to the context.

   not overriding procedure Create_Signed_Integer_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add a signed integer type view to the context.

   not overriding procedure Create_Modular_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add a unsigned integer type view to the context.

   not overriding procedure Create_Float_Point_Type
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add a float point type view to the context.

   not overriding procedure Create_Array_Type
     (Self      : in out Context;
      Symbol    : Program.Visibility.Symbol;
      Name      : Defining_Name;
      Indexes   : View_Array;
      Component : View)
        with Pre => Component.Kind in Type_View_Kind;
   --  Add an array type view to the context.

   not overriding procedure Create_Subtype
     (Self           : in out Context;
      Symbol         : Program.Visibility.Symbol;
      Name           : Defining_Name;
      Subtype_Mark   : View;
      Has_Constraint : Boolean)
        with Pre => Subtype_Mark.Kind in Type_View_Kind;
   --  Add a subtype view to the context.

   not overriding procedure Create_Package
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add an empty package view to the context

   not overriding procedure Create_Exception
     (Self   : in out Context;
      Symbol : Program.Visibility.Symbol;
      Name   : Defining_Name);
   --  Add an exception view to the context.

   not overriding procedure Leave_Declarative_Region (Self : in out Context);
   --  Leave current declarative region the context.

   not overriding function Immediate_Visible
     (Self   : aliased Context;
      Symbol : Program.Visibility.Symbol) return View_Array;
   --  Return array of views for immediate visible names with given symbol

private

   type Item_Offset is range 0 .. 2 ** 31 - 1;
   --  kind of pointer to an Item. Zero means no value
   subtype Item_Offset_Positive is Item_Offset range 1 .. Item_Offset'Last;

   type Entity_Identifier is mod 2 ** 32;

   package Item_Offset_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Item_Offset_Positive);

   subtype Has_Region_Kind is View_Kind range Package_View .. Package_View;

   type Item (Kind :  View_Kind := Package_View) is record
      Symbol    : Program.Visibility.Symbol;
      Name      : Defining_Name;
      Entity_Id : Entity_Identifier;
      --  The index of the item from which we copied this item

      case Kind is
         when Has_Region_Kind =>
            Region : Item_Offset_Vectors.Vector;
            --  If Item has nested region, this is an indexes of its elements
         when others =>
            case Kind is
               when Enumeration_Type_View =>
                  Is_Character_Type    : Boolean;
                  Enumeration_Literals : Item_Offset_Vectors.Vector;
                  --  indexes of its enumeration literals
               when Enumeration_Literal_View =>
                  Enumeration_Type : Item_Offset_Positive;
               when Character_Literal_View =>
                  Character_Type : Item_Offset_Positive;
               when Subtype_View =>
                  Subtype_Mark   : Item_Offset_Positive;
                  Has_Constraint : Boolean;
               when Array_Type_View =>
                  Indexes   : Item_Offset_Vectors.Vector;
                  Component : Item_Offset_Positive;
               when others =>
                  null;
            end case;
      end case;
   end record;

   type Region is record
      Enclosing_Item : Item_Offset_Positive;
      --  Item, that represents a declarative region
   end record;

   package Region_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Region);

   package Item_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Item_Offset_Positive,
      Element_Type => Item);

   type Snapshot is tagged limited record
      Stack   : Region_Vectors.Vector;
      --  Context is a stack of regions
      Data    : Item_Vectors.Vector;
   end record;

   type Context is tagged limited record
      Last_Entity : Entity_Identifier := 0;
      --  Unique entity counter
      Data  : Item_Vectors.Vector;
      --  All items are stored here
      Stack : Region_Vectors.Vector;
      --  Context is a stack of regions
   end record;

   type View (Kind : View_Kind := Package_View) is record
      Env   : access constant Context;
      Index : Item_Offset_Positive;
   end record;

end Program.Visibility;
