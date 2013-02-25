package body Gela.Properties is

   Map : constant array (Global_Kind, Property_Kind) of
     Gela.Relocatable_Arrays.Index :=
       (Line  => (First => 0, Last => 1, Comment => 2, others => 0),
        Token => (Value => 0, Line => 1, First => 2, Last => 3,
                  Separator => 4, others => 0));

   Last_Property : constant array (Global_Kind) of Property_Kind :=
     (Line => Comment, Token => Separator);

   --------------------
   -- Property_Index --
   --------------------

   function Property_Index
     (Element  : Global_Kind;
      Property : Property_Kind)
      return Gela.Relocatable_Arrays.Index is
   begin
      return Map (Element, Property);
   end Property_Index;

   ----------
   -- Size --
   ----------

   function Size
     (Element  : Global_Kind)
      return Gela.Relocatable_Arrays.Index
   is
      use type Gela.Relocatable_Arrays.Index;
   begin
      return Map (Element, Last_Property (Element)) + 1;
   end Size;

end Gela.Properties;
