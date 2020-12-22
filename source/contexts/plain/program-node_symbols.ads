--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Compilation_Units;
with Program.Elements.Defining_Names;
with Program.Elements.Expressions;
with Program.Elements;
with Program.Symbol_Lists;
with Program.Symbols;

package Program.Node_Symbols is
   pragma Preelaborate;

   function Get_Symbol (Name : access Program.Elements.Element'Class)
     return Program.Symbols.Symbol;
   --  Return a symbol for given direct name or defining name. Return symbol of
   --  the selector for expanded [defining] name.

   procedure Unit_Full_Name
     (Self : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Unit : not null Program.Compilation_Units.Compilation_Unit_Access;
      Name : out Program.Symbol_Lists.Symbol_List);
   --  Return unit full name as a symbol list

   procedure Defining_Name_Symbol
     (Self    : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Element : not null Program.Elements.Defining_Names.Defining_Name_Access;
      Result  : out Program.Symbol_Lists.Symbol_List);

   procedure Name_Symbol
     (Self    : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Element : not null Program.Elements.Expressions.Expression_Access;
      Result  : out Program.Symbol_Lists.Symbol_List);

end Program.Node_Symbols;
