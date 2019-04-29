--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Names;

package Program.Elements.Defining_Operator_Symbols is

   pragma Pure (Program.Elements.Defining_Operator_Symbols);

   type Defining_Operator_Symbol is
     limited interface and Program.Elements.Defining_Names.Defining_Name;

   type Defining_Operator_Symbol_Access is
     access all Defining_Operator_Symbol'Class with Storage_Size => 0;

end Program.Elements.Defining_Operator_Symbols;
