with Ada.Wide_Text_IO;

package Actions.Create is
   
   function Action
     (Input : Ada.Wide_Text_IO.File_Type)
     return Action_Access;
   
end Actions.Create;
