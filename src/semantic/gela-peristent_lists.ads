with Ada.Containers.Vectors;

generic
   type Element_Type is private;

package Gela.Peristent_Lists is
   pragma Preelaborate;

   type Container is tagged private;
   --  Store actual values of Element_Type

   type List is private;
   --  Represent persistent list

   Empty : constant List;  --  Represent empty persistent list

   procedure Prepend
     (Self   : in out Container;
      Value  : Element_Type;
      Input  : List := Empty;
      Output : out List);

   function Head
     (Self   : Container;
      Index  : List) return Element_Type;

   function Tail
     (Self   : Container;
      Index  : List) return List;

   procedure Delete
     (Self   : in out Container;
      Input  : List;
      Value  : Element_Type;
      Output : out List);

   procedure For_Each
     (Self   : Container;
      Input  : List;
      Action : access procedure (Value : Element_Type));

private

   type List is new Natural;
   --  Represent persistent list (empty for 0 or not otherwise)

   Empty : constant List := 0;

   package Element_Vectors is new
     Ada.Containers.Vectors (Positive, Element_Type);

   type Link is record
      Value : Positive;
      Next  : List;
   end record;

   subtype Index_Type is List range 1 .. List'Last;
   --  Represent non-empty persistent list

   package Link_Vectors is new
     Ada.Containers.Vectors (Index_Type, Link);

   type Container is tagged record
      Elements : Element_Vectors.Vector;
      Links    : Link_Vectors.Vector;
   end record;

end Gela.Peristent_Lists;
