--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Strings;
with League.String_Vectors;

package Meta.Classes is

   type Capacity_Kind is
     (Just_One,
      Zero_Or_One,
      Zero_Or_More,
      One_Or_More);

   type Property is record
      Name      : League.Strings.Universal_String;
      Type_Name : League.Strings.Universal_String;
      Capacity  : Capacity_Kind := Just_One;
   end record;

   type Property_Array is array (Positive range <>) of Property;

   type Class is tagged private;

   not overriding function Name
     (Self : Class) return League.Strings.Universal_String;

   not overriding function Is_Abstract (Self : Class) return Boolean;

   not overriding function Parents
     (Self : Class) return League.String_Vectors.Universal_String_Vector;

   not overriding function Properties (Self : Class) return Property_Array;

   not overriding procedure Initialize
     (Self        : in out Class;
      Name        : League.Strings.Universal_String;
      Is_Abstract : Boolean := False);

   not overriding procedure Add_Parent
     (Self : in out Class;
      Name : League.Strings.Universal_String);

   not overriding procedure Add_Property
     (Self  : in out Class;
      Value : Property);

   generic
      with function Pass (Value : Property) return Boolean;
   function Generic_Filter (List : Property_Array) return Property_Array;

private

   type Class is tagged record
      Name        : League.Strings.Universal_String;
      Is_Abstract : Boolean;
      Parents     : League.String_Vectors.Universal_String_Vector;
      Properties  : Property_Array (1 .. 30);
      Last_Prop   : Natural := 0;
   end record;

end Meta.Classes;
