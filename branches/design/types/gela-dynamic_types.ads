------------------------------------------------------------------------------
--                  G E L A   D Y N A M I C  T Y P E S                      --
--        Library for dealing with types at runtime for Gela project,       --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------
--
--  This package provides ability to create type hierarchy at run-time

with Ada.Tags;
with System.Storage_Elements;
with System.Storage_Pools.Subpools;

package Gela.Dynamic_Types is

   type Type_Identifier is private;
   --  Type identifier created by registering new types
   type Property_Identifier is private;
   --  Property identifier created by registering new property for a type
   type Node_Reference is private;
   --  Node is record of property values
   type Abstract_Property is abstract tagged null record;
   --  Root for tagger properties

   No_Type : constant Type_Identifier;
   Nil     : constant Node_Reference;

   function Create_Node
     (Type_Id : Type_Identifier;
      Subpool : System.Storage_Pools.Subpools.Subpool_Handle)
      return Node_Reference;
   --  Allocate new node of given type in Subpool

   function Type_Id (Self : Node_Reference) return Type_Identifier;
   --  Return type of given node

   function Get
     (Self     : Node_Reference;
      Property : Property_Identifier)
      return Boolean;
   --  Return boolean property of given node

   function Get
     (Self     : Node_Reference;
      Property : Property_Identifier)
      return Integer;
   --  Return integer property of given node

   function Get
     (Self     : Node_Reference;
      Property : Property_Identifier)
      return Node_Reference;
   --  Return property referencing a node

   function Get
     (Self     : Node_Reference;
      Property : Property_Identifier)
      return Abstract_Property'Class;
   --  Return tagged property of given node

   procedure Set
     (Self     : Node_Reference;
      Property : Property_Identifier;
      Value    : Boolean);
   --  Assign boolean property of given node

   procedure Set
     (Self     : Node_Reference;
      Property : Property_Identifier;
      Value    : Integer);
   --  Assign integer property of given node

   procedure Set
     (Self     : Node_Reference;
      Property : Property_Identifier;
      Value    : Node_Reference);
   --  Assign reference property of given node

   procedure Set
     (Self     : Node_Reference;
      Property : Property_Identifier;
      Value    : Abstract_Property'Class);
   --  Assign tagged property of given node

   subtype Storage_Array is System.Storage_Elements.Storage_Array
     (System.Storage_Elements.Storage_Count'Range);

   function Decode
     (Self : not null access Storage_Array)
      return Abstract_Property is abstract;
   --  Decoder of user-defined tagged property from Storage_Array

   procedure Encode
     (Self  : Abstract_Property;
      Value : in out System.Storage_Elements.Storage_Array) is abstract;
   --  Encoder of user-defined tagged property into Storage_Array

   procedure Register_Type
     (Result : out Type_Identifier;
      Parent : Type_Identifier := No_Type);
   --  Register new type. New Type_Identifier is allocated into Result.
   --  Any property declared for Parent type are inherited by new one

   procedure Register_Boolean_Property
     (Target : Type_Identifier;
      Result : out Property_Identifier);
   --  Register boolean property in given Target type

   procedure Register_Integer_Property
     (Target : Type_Identifier;
      Result : out Property_Identifier);
   --  Register integer property in given Target type

   procedure Register_Reference_Property
     (Target : Type_Identifier;
      Result : out Property_Identifier);
   --  Register reference property in given Target type

   procedure Register_Property
     (Target : Type_Identifier;
      Tag    : Ada.Tags.Tag;
      Size   : System.Storage_Elements.Storage_Count;
      Result : out Property_Identifier);
   --  Register tagged property (with Tag) with Size elements of Storage_Array
   --  in given Target type

   procedure Registration_Complete;
   --  No Create_Node allowed before call of this procedure.
   --  No more registration allowed after call of this procedure
private

   type Type_Descriptor;

   type Type_Identifier is access all Type_Descriptor;

   type Property_Identifier is new Natural;

   type Property_Descriptor is record
      Offset : System.Storage_Elements.Storage_Count;
      --  Offset in Data array of Node
      Length : System.Storage_Elements.Storage_Count;
      --  Length in Data array of Node
      Tag    : Ada.Tags.Tag;
      --  Tag for tagged properties
      Mask   : System.Storage_Elements.Storage_Element;
      --  Bit mask for boolean properties
   end record;

   type Property_Descriptor_Array is array (Property_Identifier range <>)
     of Property_Descriptor;

   type Property_Descriptor_Array_Access is
     access all Property_Descriptor_Array;

   type Type_Descriptor is record
      Parent     : Type_Identifier;
      Length     : System.Storage_Elements.Storage_Offset;
      List       : Property_Descriptor_Array_Access;
   end record;

   type Node is record
      Type_Id : Type_Identifier;
      Data    : Storage_Array;
   end record;

   type Node_Reference is access all Node;

   Nil : constant Node_Reference := null;
   No_Type : constant Type_Identifier := null;

end Gela.Dynamic_Types;
