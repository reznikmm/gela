--  SPDX-FileCopyrightText: 2020-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

procedure Program.Nodes.Operator_Symbols.Set_Defining_Name
  (Self  : not null Program.Elements.Operator_Symbols.Operator_Symbol_Access;
   Value : Program.Elements.Defining_Operator_Symbols
   .Defining_Operator_Symbol_Access) is
begin
   Base_Operator_Symbol (Self.all).Corresponding_Defining_Operator_Symbol :=
     Value;
end Program.Nodes.Operator_Symbols.Set_Defining_Name;
