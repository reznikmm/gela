package Gela.Properties is

   type Property_Name is
     (Code,     --  Text representation of unit, statement, etc
      Global,   --  Global declaration required for an element
      Value,    --  Name (like %123, @id) or value (like 123) of given expr
      Non_Static_Value,   --  Like Value, but for non-static expr
      First,    --  Value of T'First
      Last,     --  Value of T'Last
      Length);  --  Value of T'Last - T'First + 1

   type Boolean_Property_Name is
     (Is_Local);  --  Check if declaration if local or global

end Gela.Properties;
