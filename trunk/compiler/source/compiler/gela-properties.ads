package Gela.Properties is

   type Property_Name is
     (Code,     --  Text representation of unit, statement, etc
      Global,   --  Global declaration required for an element
      Value,    --  Name (like %123, @id) or value (like 123) of given expr
      Non_Static_Value);   --  Like Value, but for non-static expr

end Gela.Properties;
