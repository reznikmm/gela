------------------------------------------------------------------------------
--                  G E L A   D Y N A M I C  T Y P E S                      --
--        Library for dealing with types at runtime for Gela project,       --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Tags.Generic_Dispatching_Constructor;
with System.Address_To_Access_Conversions;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

package body Gela.Dynamic_Types is

   use type Ada.Tags.Tag;
   use type System.Storage_Elements.Storage_Element;
   use type System.Storage_Elements.Storage_Offset;

   package ATAC is new System.Address_To_Access_Conversions (Node);

   type Integer_Access is access all Integer;
   type Storage_Array_Access is access all Storage_Array;
   type Node_Reference_Access is access all Node_Reference;
   type Storage_Element_Access is
     access all System.Storage_Elements.Storage_Element;

   function Integer_Convert is new Ada.Unchecked_Conversion
     (Storage_Element_Access, Integer_Access);

   function Reference_Convert is new Ada.Unchecked_Conversion
     (Storage_Element_Access, Node_Reference_Access);

   function Storage_Array_Convert is new Ada.Unchecked_Conversion
     (Storage_Element_Access, Storage_Array_Access);

   function Constructor is new Ada.Tags.Generic_Dispatching_Constructor
     (Abstract_Property, Storage_Array, Decode);

   function Less (Left, Right : Type_Identifier) return Boolean;
   --  for ordered maps

   subtype Property_Identifier_Positive is
     Property_Identifier range 1 .. Property_Identifier'Last;

   type Property_Registration is record
      Prop_Id : Property_Identifier;
      Prop    : Property_Descriptor;
   end record;

   package Property_Registration_Vectors is new Ada.Containers.Vectors
     (Property_Identifier_Positive, Property_Registration);

   function Less (Left, Right : Property_Registration) return Boolean;
   --  for sorting procedure

   package Property_Sorting is
     new Property_Registration_Vectors.Generic_Sorting (Less);

   type Type_Registration is record
      Complete : Boolean;
      Props    : Property_Registration_Vectors.Vector;
   end record;

   package Type_Register_Maps is new Ada.Containers.Ordered_Maps
     (Type_Identifier, Type_Registration, Less);

   procedure Complete_Type (Type_Id : Type_Identifier);

   procedure Inherit_Properties (Type_Id : Type_Identifier);

   Type_Register : Type_Register_Maps.Map;

   -------------------
   -- Complete_Type --
   -------------------

   procedure Complete_Type (Type_Id : Type_Identifier) is
      Data   : Type_Registration renames Type_Register (Type_Id);
      Offset : System.Storage_Elements.Storage_Count := 0;
      Mask   : System.Storage_Elements.Storage_Element := 1;
   begin
      if Data.Complete then
         return;
      end if;

      Type_Id.List :=
        new Property_Descriptor_Array (1 .. Data.Props.Last_Index);

      for J in Type_Id.List'Range  loop
         Type_Id.List (J) := Data.Props (J).Prop;
      end loop;

      --  Sort properties in right order
      Property_Sorting.Sort (Data.Props);

      --  Calculate offsets and bit masks
      for X of Data.Props loop
         declare
            Value : Property_Descriptor renames Type_Id.List (X.Prop_Id);
         begin
            Value.Offset := Offset;
            Offset := Offset + X.Prop.Length;

            if X.Prop.Mask = 1 then  --  if Boolean
               Value.Mask := Mask;

               if Mask > System.Storage_Elements.Storage_Element'Last / 2 then
                  Mask := 1;
                  Offset := Offset + 1;
               else
                  Mask := Mask * 2;
               end if;
            end if;
         end;
      end loop;

      if Mask /= 1 then
         --  Skip unfinished boolean storage element
         Offset := Offset + 1;
      end if;

      Type_Id.Length := Offset;
      Data.Complete := True;
   end Complete_Type;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Type_Id : Type_Identifier;
      Subpool : System.Storage_Pools.Subpools.Subpool_Handle)
      return Node_Reference
   is
      use System.Storage_Pools.Subpools;

      Address : System.Address;
      Result  : Node_Reference;
   begin
      Pool_Of_Subpool (Subpool).Allocate_From_Subpool
        (Storage_Address          => Address,
         Size_In_Storage_Elements => Type_Identifier'Size /
           System.Storage_Elements.Storage_Element'Size + Type_Id.Length,
         Alignment                => Type_Identifier'Alignment,
         Subpool                  => Subpool);
      Result := Node_Reference (ATAC.To_Pointer (Address));
      Result.Type_Id := Type_Id;
      Result.Data (0 .. Type_Id.Length - 1) := (others => 0);

      return Result;
   end Create_Node;

   ---------
   -- Get --
   ---------

   function Get
     (Self     : Node_Reference;
      Property : Property_Identifier)
      return Boolean
   is
      Prop : Property_Descriptor renames Self.Type_Id.List (Property);
   begin
      pragma Assert (Prop.Mask /= 0);

      return (Self.Data (Prop.Offset) and Prop.Mask) /= 0;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Self     : Node_Reference;
      Property : Property_Identifier)
      return Integer
   is
      Prop : Property_Descriptor renames Self.Type_Id.List (Property);
   begin
      pragma Assert (Prop.Mask = 0 and Prop.Tag = Ada.Tags.No_Tag);

      return Integer_Convert (Self.Data (Prop.Offset)'Access).all;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Self     : Node_Reference;
      Property : Property_Identifier)
      return Node_Reference
   is
      Prop : Property_Descriptor renames Self.Type_Id.List (Property);
   begin
      pragma Assert (Prop.Mask = 0 and Prop.Tag = Ada.Tags.No_Tag);

      return Reference_Convert (Self.Data (Prop.Offset)'Access).all;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Self     : Node_Reference;
      Property : Property_Identifier)
      return Abstract_Property'Class
   is
      Prop : Property_Descriptor renames Self.Type_Id.List (Property);
      Buffer : constant Storage_Array_Access := Storage_Array_Convert
        (Self.Data (Prop.Offset)'Access);
   begin
      pragma Assert (Prop.Tag /= Ada.Tags.No_Tag);

      return Constructor (Prop.Tag, Buffer);
   end Get;

   ------------------------
   -- Inherit_Properties --
   ------------------------

   procedure Inherit_Properties (Type_Id : Type_Identifier) is
   begin
      if Type_Id.Parent /= null then
         declare
            Child  : Type_Registration renames Type_Register (Type_Id);
            Parent : Type_Registration renames Type_Register (Type_Id.Parent);
         begin
            Complete_Type (Type_Id.Parent);
            Child.Props := Parent.Props;
         end;
      end if;
   end Inherit_Properties;

   ----------
   -- Less --
   ----------

   function Less (Left, Right : Type_Identifier) return Boolean is
      use System;
   begin
      return Left.all'Address < Right.all'Address;
   end Less;

   ----------
   -- Less --
   ----------

   function Less (Left, Right : Property_Registration) return Boolean is
   begin
      if Left.Prop.Tag /= Ada.Tags.No_Tag and
        Right.Prop.Tag /= Ada.Tags.No_Tag
      then
         --  When both properties are tagged order them by id
         return Left.Prop_Id < Right.Prop_Id;
      elsif Left.Prop.Tag = Ada.Tags.No_Tag and
        Right.Prop.Tag /= Ada.Tags.No_Tag
      then
         --  Place untagged before tagged
         return True;
      elsif Left.Prop.Tag /= Ada.Tags.No_Tag and
        Right.Prop.Tag = Ada.Tags.No_Tag
      then
         --  Place tagged after untagged
         return False;
      elsif Left.Prop.Length /= Left.Prop.Length then
         --  Order untagged by size
         return Left.Prop.Length < Left.Prop.Length;
      else
         --  Order untagged with equal size by id
         return Left.Prop_Id < Right.Prop_Id;
      end if;
   end Less;

   -------------------------------
   -- Register_Boolean_Property --
   -------------------------------

   procedure Register_Boolean_Property
     (Target : Type_Identifier;
      Result : out Property_Identifier)
   is
      Data  : Type_Registration renames Type_Register (Target);
      Props : Property_Registration_Vectors.Vector renames Data.Props;
   begin
      if Props.Is_Empty then
         Inherit_Properties (Target);
      end if;

      Result := Props.Last_Index + 1;

      Props.Append
        ((Prop_Id => Result,
          Prop    => (Offset => 0,
                      Length => 0,
                      Tag    => Ada.Tags.No_Tag,
                      Mask   => 1)));
   end Register_Boolean_Property;

   ---------------------------
   -- Registration_Complete --
   ---------------------------

   procedure Registration_Complete is
   begin
      for J in Type_Register.Iterate loop
         Complete_Type (Type_Register_Maps.Key (J));
      end loop;
   end Registration_Complete;

   -------------------------------
   -- Register_Integer_Property --
   -------------------------------

   procedure Register_Integer_Property
     (Target : Type_Identifier;
      Result : out Property_Identifier)
   is
      Size : constant :=
        Integer'Size / System.Storage_Elements.Storage_Element'Size;

      Data  : Type_Registration renames Type_Register (Target);
      Props : Property_Registration_Vectors.Vector renames Data.Props;
   begin
      if Props.Is_Empty then
         Inherit_Properties (Target);
      end if;

      Result := Props.Last_Index + 1;

      Props.Append
        ((Prop_Id => Result,
          Prop    => (Offset => 0,
                      Length => Size,
                      Tag    => Ada.Tags.No_Tag,
                      Mask   => 0)));
   end Register_Integer_Property;

   -----------------------
   -- Register_Property --
   -----------------------

   procedure Register_Property
     (Target : Type_Identifier;
      Tag    : Ada.Tags.Tag;
      Size   : System.Storage_Elements.Storage_Count;
      Result : out Property_Identifier)
   is
      Data  : Type_Registration renames Type_Register (Target);
      Props : Property_Registration_Vectors.Vector renames Data.Props;
   begin
      if Props.Is_Empty then
         Inherit_Properties (Target);
      end if;

      Result := Props.Last_Index + 1;

      Props.Append
        ((Prop_Id => Result,
          Prop    => (Offset => 0,
                      Length => Size,
                      Tag    => Tag,
                      Mask   => 0)));
   end Register_Property;

   ---------------------------------
   -- Register_Reference_Property --
   ---------------------------------

   procedure Register_Reference_Property
     (Target : Type_Identifier;
      Result : out Property_Identifier)
   is
      Size : constant System.Storage_Elements.Storage_Count :=
        Node_Reference'Size / System.Storage_Elements.Storage_Element'Size;

      Data  : Type_Registration renames Type_Register (Target);
      Props : Property_Registration_Vectors.Vector renames Data.Props;
   begin
      if Props.Is_Empty then
         Inherit_Properties (Target);
      end if;

      Result := Props.Last_Index + 1;

      Props.Append
        ((Prop_Id => Result,
          Prop    => (Offset => 0,
                      Length => Size,
                      Tag    => Ada.Tags.No_Tag,
                      Mask   => 0)));
   end Register_Reference_Property;

   -------------------
   -- Register_Type --
   -------------------

   procedure Register_Type
     (Result : out Type_Identifier;
      Parent : Type_Identifier := No_Type)
   is
   begin
      Result := new Type_Descriptor'(Parent => Parent,
                                     Length => 0,
                                     List   => null);

      Type_Register.Insert
        (Result, (Complete => False, Props => <>));
   end Register_Type;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self     : Node_Reference;
      Property : Property_Identifier;
      Value    : Boolean)
   is
      Prop : Property_Descriptor renames Self.Type_Id.List (Property);
   begin
      pragma Assert (Prop.Mask /= 0);

      if Value then
         Self.Data (Prop.Offset) := Self.Data (Prop.Offset) or Prop.Mask;
      else
         Self.Data (Prop.Offset) := Self.Data (Prop.Offset) and not Prop.Mask;
      end if;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self     : Node_Reference;
      Property : Property_Identifier;
      Value    : Integer)
   is
      Prop : Property_Descriptor renames Self.Type_Id.List (Property);
   begin
      pragma Assert (Prop.Mask = 0 and Prop.Tag = Ada.Tags.No_Tag);

      Integer_Convert (Self.Data (Prop.Offset)'Access).all := Value;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self     : Node_Reference;
      Property : Property_Identifier;
      Value    : Node_Reference)
   is
      Prop : Property_Descriptor renames Self.Type_Id.List (Property);
   begin
      pragma Assert (Prop.Mask = 0 and Prop.Tag = Ada.Tags.No_Tag);

      Reference_Convert (Self.Data (Prop.Offset)'Access).all := Value;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self     : Node_Reference;
      Property : Property_Identifier;
      Value    : Abstract_Property'Class)
   is
      Prop : Property_Descriptor renames Self.Type_Id.List (Property);
      Buffer : System.Storage_Elements.Storage_Array renames
        Self.Data (Prop.Offset .. Prop.Offset + Prop.Length - 1);
   begin
      pragma Assert (Prop.Tag /= Ada.Tags.No_Tag);

      Value.Encode (Buffer);
   end Set;

   -------------
   -- Type_Id --
   -------------

   function Type_Id (Self : Node_Reference) return Type_Identifier is
   begin
      return Self.Type_Id;
   end Type_Id;

end Gela.Dynamic_Types;
