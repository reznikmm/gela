--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------
package body Program.Units.Declarations is

   ------------------
   -- Append_Child --
   ------------------

   procedure Append_Child
     (Self  : in out Unit_Declaration;
      Value : Program.Compilation_Units.Compilation_Unit_Access) is
   begin
      Self.Childern.Append (Value);
   end Append_Child;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   overriding function Corresponding_Body (Self : access Unit_Declaration)
     return Program.Library_Unit_Bodies.Library_Unit_Body_Access is
   begin
      return Self.Impl;
   end Corresponding_Body;

   ----------------------------
   -- Corresponding_Childern --
   ----------------------------

   overriding function Corresponding_Childern (Self : access Unit_Declaration)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access is
   begin
      if Self.Childern.Is_Empty then
         return null;
      else
         return Self.Childern'Access;
      end if;
   end Corresponding_Childern;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self             : in out Unit_Declaration;
      Compilation      : Program.Compilations.Compilation_Access;
      Full_Name        : Text;
      Context_Clause   : Program.Element_Vectors.Element_Vector_Access;
      Declaration      : not null Program.Elements.Element_Access;
      Parent           : Program.Library_Unit_Declarations
                           .Library_Unit_Declaration_Access) is
   begin
      Self.Initialize
        (Compilation      => Compilation,
         Full_Name        => Full_Name,
         Context_Clause   => Context_Clause,
         Unit_Declaration => Declaration);

      Self.Parent := Parent;

      if Parent not in null then
         Unit_Declaration (Parent.all).Append_Child (Self'Unchecked_Access);
      end if;

      Self.Childern.Clear;
   end Initialize;

   --------------
   -- Set_Body --
   --------------

   procedure Set_Body
     (Self  : in out Unit_Declaration;
      Value : Program.Library_Unit_Bodies.Library_Unit_Body_Access) is
   begin
      Self.Impl := Value;
   end Set_Body;

   ------------
   -- Parent --
   ------------

   overriding function Parent (Self : access Unit_Declaration)
     return Program.Library_Unit_Declarations.Library_Unit_Declaration_Access
   is
   begin
      return Self.Parent;
   end Parent;

end Program.Units.Declarations;
