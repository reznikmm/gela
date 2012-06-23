with Filters.Any;
--  with Ada.Strings.Wide_Fixed;

package body Filters.Create is
   
   package Enum is
      type Filter_Kind is (Any);
      
      package Filter_Kind_IO is
         new Ada.Wide_Text_IO.Enumeration_IO (Filter_Kind);
   end Enum;
   
   function Filter
     (Input : Ada.Wide_Text_IO.File_Type)
     return Filter_Access
   is
      Kind : Enum.Filter_Kind;
   begin
      Enum.Filter_Kind_IO.Get (Input, Kind);
      
      case Kind is
         when Enum.Any =>
            return new Filters.Any.Any_Filter;
      end case;
   end Filter;
   
end Filters.Create;
