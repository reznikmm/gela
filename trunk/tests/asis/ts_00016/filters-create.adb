with Filters.Any;

package body Filters.Create is
   
   function Filter
     (Input : Ada.Wide_Text_IO.File_Type)
     return Filter_Access
   is
      pragma Unreferenced (Input);
   begin
      return new Filters.Any.Any_Filter;
   end Filter;
   
end Filters.Create;
