--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Visitors;
with Program.Elements.Defining_Character_Literals;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Package_Declarations;
with Program.Elements;
with Program.Lexical_Elements;
with Program.Plain_Lexical_Elements;

package body Program.Resolvers is

   package Visitors is
      type Visitor
        (Env : not null access Program.Visibility.Context)
      is new Program.Element_Visitors.Element_Visitor with record
         null;
      end record;

      overriding procedure Package_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Package_Declarations
         .Package_Declaration_Access);

   end Visitors;

   -------------------
   -- Resolve_Names --
   -------------------

   procedure Resolve_Names
     (Env  : aliased in out Program.Visibility.Context;
      Unit : not null Program.Compilation_Units.Compilation_Unit_Access)
   is
      Element : constant Program.Elements.Element_Access :=
        Unit.Unit_Declaration;
      Visitor : Visitors.Visitor (Env'Access);
   begin
      Element.Visit (Visitor);
   end Resolve_Names;

   ---------------
   -- To_Symbol --
   ---------------

   function To_Symbol
     (Name : access Program.Elements.Defining_Names.Defining_Name'Class)
         return Program.Symbols.Symbol
   is
      type Getter is new Program.Element_Visitors.Element_Visitor with record
         Result : Program.Symbols.Symbol := Program.Symbols.No_Symbol;
      end record;

      overriding procedure Defining_Character_Literal
        (Self    : in out Getter;
         Element : not null Program.Elements.Defining_Character_Literals
           .Defining_Character_Literal_Access);

      overriding procedure Defining_Identifier
        (Self    : in out Getter;
         Element : not null Program.Elements.Defining_Identifiers
           .Defining_Identifier_Access);

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

      G : Getter;
   begin
      Name.Visit (G);
      pragma Assert (G.Result not in Program.Symbols.No_Symbol);

      return G.Result;
   end To_Symbol;

   --------------
   -- Visitors --
   --------------

   package body Visitors is

      -------------------------
      -- Package_Declaration --
      -------------------------

      overriding procedure Package_Declaration
        (Self    : in out Visitor;
         Element : not null
           Program.Elements.Package_Declarations.Package_Declaration_Access)
      is
         Name   : constant
           Program.Elements.Defining_Names.Defining_Name_Access :=
             Element.Name;
         Symbol : constant Program.Symbols.Symbol := To_Symbol (Name);
      begin
         Self.Env.Create_Package
           (Symbol => Symbol,
            Name   => Name);
      end Package_Declaration;

   end Visitors;

end Program.Resolvers;
