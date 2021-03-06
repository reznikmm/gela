--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Visibility;

package Program.Type_Matchers is
   pragma Preelaborate;

   type Type_Matcher is limited interface;

   type Type_Matcher_Access is access all Type_Matcher'Class;

   not overriding function Is_Matched
     (Self : Type_Matcher;
      Tipe : Program.Visibility.View) return Boolean is abstract
        with Pre'Class => Tipe.Kind in Program.Visibility.Type_View_Kind;

   type Record_Type_Matcher is new Type_Matcher with null record;

   use all type Program.Visibility.View_Kind;

   overriding function Is_Matched
     (Self : Record_Type_Matcher;
      Tipe : Program.Visibility.View) return Boolean is
        (Tipe.Kind = Record_Type_View);

   type Discrete_Type_Matcher is new Type_Matcher with null record;

   overriding function Is_Matched
     (Self : Discrete_Type_Matcher;
      Tipe : Program.Visibility.View) return Boolean is
        (Tipe.Kind in Enumeration_Type_View
                       | Signed_Integer_Type_View | Modular_Type_View);

end Program.Type_Matchers;
