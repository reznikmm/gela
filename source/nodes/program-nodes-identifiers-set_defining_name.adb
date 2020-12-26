--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

procedure Program.Nodes.Identifiers.Set_Defining_Name
  (Self  : in out Identifier'Class;
   Value : Program.Elements.Defining_Identifiers.Defining_Identifier_Access)
is
begin
   Self.Corresponding_Defining_Identifier := Value;
end Program.Nodes.Identifiers.Set_Defining_Name;
