--  SPDX-FileCopyrightText: 2020-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

procedure Program.Nodes.Identifiers.Set_Defining_Name
  (Self  : not null Program.Elements.Identifiers.Identifier_Access;
   Value : Program.Elements.Defining_Identifiers.Defining_Identifier_Access);
pragma Preelaborate (Program.Nodes.Identifiers.Set_Defining_Name);
