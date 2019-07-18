with System.Storage_Elements;
with System.Address_To_Access_Conversions;

package body Program.Relative_Access_Types is

   package Conversions is new System.Address_To_Access_Conversions (Object);
   use System.Storage_Elements;

   ---------
   -- "+" --
   ---------

   function "+" (Value : Object_Access) return Relative_Access is
   begin
      return Result : Relative_Access do
         if Value = null then
            Result := Relative_Access'First;
         else
            declare
               Value_Address : constant Integer_Address :=
                 To_Integer (Value.all'Address);

               Result_Address : constant Integer_Address :=
                 To_Integer (Result'Address);
            begin
               if Value_Address > Result_Address then
                  Result := Relative_Access (Value_Address - Result_Address);
               else
                  Result := -Relative_Access (Result_Address - Value_Address);
               end if;
            end;
         end if;
      end return;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Value : Relative_Access) return Object_Access is
      Self : constant Integer_Address := To_Integer (Value'Address);
   begin
      if Value = Relative_Access'First then
         return null;
      elsif Value > 0 then
         return Object_Access
           (Conversions.To_Pointer
              (To_Address (Self + Integer_Address (Value))));
      else
         return Object_Access
           (Conversions.To_Pointer
              (To_Address (Self - Integer_Address (abs Value))));
      end if;
   end "-";

end Program.Relative_Access_Types;
