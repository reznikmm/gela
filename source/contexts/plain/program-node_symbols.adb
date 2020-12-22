--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Safe_Element_Visitors;
with Program.Elements.Defining_Character_Literals;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Character_Literals;
with Program.Elements.Identifiers;
with Program.Lexical_Elements;
with Program.Plain_Lexical_Elements;

package body Program.Node_Symbols is

   type Getter is new Program.Safe_Element_Visitors.Safe_Element_Visitor
   with record
      Result : Program.Symbols.Symbol := Program.Symbols.No_Symbol;
   end record;

   overriding procedure Character_Literal
     (Self    : in out Getter;
      Element : not null Program.Elements.Character_Literals
        .Character_Literal_Access);

   overriding procedure Defining_Character_Literal
     (Self    : in out Getter;
      Element : not null Program.Elements.Defining_Character_Literals
        .Defining_Character_Literal_Access);

   overriding procedure Defining_Identifier
     (Self    : in out Getter;
      Element : not null Program.Elements.Defining_Identifiers
        .Defining_Identifier_Access);

   overriding procedure Identifier
     (Self    : in out Getter;
      Element : not null Program.Elements.Identifiers.Identifier_Access);

   -----------------------
   -- Character_Literal --
   -----------------------

   overriding procedure Character_Literal
     (Self    : in out Getter;
      Element : not null Program.Elements.Character_Literals
      .Character_Literal_Access)
   is
      Token : constant Program.Lexical_Elements.Lexical_Element_Access :=
        Element.To_Character_Literal_Text.Character_Literal_Token;
   begin
      Self.Result := Program.Plain_Lexical_Elements.Lexical_Element
        (Token.all).Symbol;
   end Character_Literal;

   --------------------------------
   -- Defining_Character_Literal --
   --------------------------------

   overriding procedure Defining_Character_Literal
     (Self    : in out Getter;
      Element : not null Program.Elements.Defining_Character_Literals
        .Defining_Character_Literal_Access)
   is
      Token : constant Program.Lexical_Elements.Lexical_Element_Access :=
        Element.To_Defining_Character_Literal_Text.Character_Literal_Token;
   begin
      Self.Result := Program.Plain_Lexical_Elements.Lexical_Element
        (Token.all).Symbol;
   end Defining_Character_Literal;

   -------------------------
   -- Defining_Identifier --
   -------------------------

   overriding procedure Defining_Identifier
     (Self    : in out Getter;
      Element : not null Program.Elements.Defining_Identifiers
        .Defining_Identifier_Access)
   is
      Token : constant Program.Lexical_Elements.Lexical_Element_Access :=
        Element.To_Defining_Identifier_Text.Identifier_Token;
   begin
      Self.Result := Program.Plain_Lexical_Elements.Lexical_Element
        (Token.all).Symbol;
   end Defining_Identifier;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Name : access Program.Elements.Element'Class)
      return Program.Symbols.Symbol
   is
      G : Getter;
   begin
      G.Visit (Name);

      return G.Result;
   end Get_Symbol;

   ----------------
   -- Identifier --
   ----------------

   overriding procedure Identifier
     (Self    : in out Getter;
      Element : not null Program.Elements.Identifiers.Identifier_Access)
   is
      Token : constant Program.Lexical_Elements.Lexical_Element_Access :=
        Element.To_Identifier_Text.Identifier_Token;
   begin
      Self.Result := Program.Plain_Lexical_Elements.Lexical_Element
        (Token.all).Symbol;
   end Identifier;

end Program.Node_Symbols;
