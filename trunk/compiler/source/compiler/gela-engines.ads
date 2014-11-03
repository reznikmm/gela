with Ada.Containers.Hashed_Maps;

with Asis;
with Asis.Extensions.Flat_Kinds;

with Gela.Properties.Text;

package Gela.Engines is

   type Engine is tagged limited private;
   type Engine_Access is access all Engine;

   function Get
     (Self     : access Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
     return Gela.Properties.Text.Text;

   type Text_Rule_Callback is access function
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
     return Gela.Properties.Text.Text;

   procedure Register_Rule
     (Self     : in out Engine;
      Kind     : Asis.Extensions.Flat_Kinds.Element_Flat_Kind;
      Property : Gela.Properties.Property_Name;
      Action   : Text_Rule_Callback);

   function Text_Container
     (Self : access Engine) return Gela.Properties.Text.Text_Container_Access;

private

   type Rule_Key is record
      Kind     : Asis.Extensions.Flat_Kinds.Element_Flat_Kind;
      Property : Gela.Properties.Property_Name;
   end record;

   function Hash (Item : Rule_Key) return Ada.Containers.Hash_Type;

   package Text_Rule_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Rule_Key,
      Element_Type    => Text_Rule_Callback,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Engine is tagged limited record
      Text_Rules     : Text_Rule_Maps.Map;
      Text_Container : aliased Gela.Properties.Text.Text_Container;
   end record;

end Gela.Engines;
