with Asis;

with Actions;
with Filters;

package Traversing_Actions is

   type Traversal_State is tagged limited private;

   procedure Pre_Action
     (Element :        Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Traversal_State);

   procedure Post_Action
     (Element :        Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Traversal_State);
   
   procedure Add
     (Self   : in out Traversal_State;
      Action : Actions.Action_Access;
      Filter : Filters.Filter_Access);
   
private
   
   type Item is record
      Action : Actions.Action_Access;
      Filter : Filters.Filter_Access;
   end record;
   
   type Item_Array is array (1 .. 10) of Item;
   
   type Traversal_State is tagged limited record
      Items : Item_Array;
      Last  : Natural := 0;
   end record;

end Traversing_Actions;
