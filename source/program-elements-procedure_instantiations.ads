--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Procedure_Instantiations is

   pragma Pure (Program.Elements.Procedure_Instantiations);

   type Procedure_Instantiation is
     limited interface and Program.Elements.Declarations.Declaration;

   type Procedure_Instantiation_Access is
     access all Procedure_Instantiation'Class with Storage_Size => 0;

end Program.Elements.Procedure_Instantiations;
