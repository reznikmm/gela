with Ada.Containers.Vectors;

generic
   type Element_Type is private;

package Gela.Peristent_Lists is
   pragma Preelaborate;

   type Container is tagged private;
   --  Store actual values of Element_Type

   type Count_Type is new Natural;
   --  Represent persistent list (empty for 0 or not otherwise)
   subtype Index_Type is Count_Type range 1 .. Count_Type'Last;
   --  Represent non-empty persistent list

   procedure Prepend
     (Self   : in out Container;
      Value  : Element_Type;
      Input  : Count_Type := 0;
      Output : out Index_Type);

   function Head
     (Self   : Container;
      Index  : Index_Type) return Element_Type;

   function Tail
     (Self   : Container;
      Index  : Index_Type) return Count_Type;

   procedure Delete
     (Self   : in out Container;
      Input  : Count_Type;
      Value  : Element_Type;
      Output : out Count_Type);

   procedure For_Each
     (Self   : Container;
      Input  : Count_Type;
      Action : access procedure (Value : Element_Type));

private

   package Element_Vectors is new
     Ada.Containers.Vectors (Positive, Element_Type);

   type Link is record
      Value : Positive;
      Next  : Count_Type;
   end record;

   package Link_Vectors is new
     Ada.Containers.Vectors (Index_Type, Link);

   type Container is tagged record
      Elements : Element_Vectors.Vector;
      Links    : Link_Vectors.Vector;
   end record;

end Gela.Peristent_Lists;
