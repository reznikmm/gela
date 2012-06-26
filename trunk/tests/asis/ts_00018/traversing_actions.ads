with Asis;
private with Ada.Strings.Wide_Unbounded;
with Ada.Containers;

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
   
   function Get_Text (State : Traversal_State) return Wide_String;
   
   function Get_Text_Hash
     (State : Traversal_State)
      return Ada.Containers.Hash_Type;
   
private
   
   type Traversal_State is tagged limited record
      Level : Natural := 0;
      Text  : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   end record;

end Traversing_Actions;
