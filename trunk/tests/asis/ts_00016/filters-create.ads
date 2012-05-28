with Ada.Wide_Text_IO;

package Filters.Create is
   
   function Filter
     (Input : Ada.Wide_Text_IO.File_Type)
     return Filter_Access;
   
end Filters.Create;
