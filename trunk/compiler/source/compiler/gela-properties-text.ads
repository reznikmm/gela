with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash;

package Gela.Properties.Text is

   type Text is private;

   type Text_Container is tagged limited private;
   type Text_Container_Access is access all Text_Container'Class;

   function Literal
     (Self  : access Text_Container;
      Value : String) return Text;

   function New_Line
     (Self  : access Text_Container) return Text;

   function Join
     (Self  : access Text_Container;
      Left  : Text;
      Right : String) return Text;

   function Join
     (Self  : access Text_Container;
      Left  : String;
      Right : Text) return Text;

   function Join
     (Self  : access Text_Container;
      Left  : Text;
      Right : Text) return Text;

   function Join
     (Self  : access Text_Container;
      Left  : Text;
      Right : Natural) return Text;

   function Value
     (Self : access Text_Container;
      Item : Text) return String;

private

   type Text is mod 2 ** 32;

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Ada.Strings.Unbounded.Unbounded_String,
      "="          => Ada.Strings.Unbounded."=");

   package String_Hash_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Text,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Join_Node is record
      Left, Right : Text;
   end record;

   function Hash (Item : Join_Node) return Ada.Containers.Hash_Type;

   package Join_Node_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Join_Node);

   package Join_Node_Hash_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Join_Node,
      Element_Type    => Text,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Text_Container is tagged limited record
      String_Map    : String_Hash_Maps.Map;
      Join_Map      : Join_Node_Hash_Maps.Map;
      String_Vector : String_Vectors.Vector;
      Join_Vector   : Join_Node_Vectors.Vector;
   end record;

end Gela.Properties.Text;
