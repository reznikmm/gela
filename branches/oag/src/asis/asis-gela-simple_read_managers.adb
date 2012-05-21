with Asis.Gela.Parser;
with Asis.Gela.Contexts;
--with Asis.Gela.Normalize;
with Asis.Gela.Compilations;  use Asis.Gela.Compilations;
with Asis.Gela.Current_State;

with Ada.Strings.Wide_Unbounded;

package body Asis.Gela.Simple_Read_Managers is
   use Asis.Gela.Contexts;

   procedure Read_File
     (Context : in out Context_Node;
      File    : in     Wide_String);

   ----------
   -- Read --
   ----------

   procedure Read
     (Manager : in out Read_Manager;
      Context : in out Asis.Context;
      Units   : in     Read_Managers.Source_List)
   is
      package W renames Ada.Strings.Wide_Unbounded;

      C : Context_Node renames Current_State.Contexts (Context.Index);
   begin
      for J in Units'Range loop
         Read_File (C, W.To_Wide_String (Units (J)));
      end loop;
   end Read;

   ---------------
   -- Read_File --
   ---------------

   procedure Read_File
     (Context : in out Context_Node;
      File    : in     Wide_String)
   is
      Comp   : Compilation;
      Result : ASIS_Natural;
   begin
      --  1) Check if file already in Context
      if Has_File (Context, File) then
         return;
      end if;

      --  2) Create compilation
      Create_Compilation (Comp, File, Encoding (Context));

      --  3) Parse source
      Parser.Run (Context, Comp, Result);

      --  4) Normalize tree
--      Normalize.Run (Comp, Result);
   end Read_File;
end Asis.Gela.Simple_Read_Managers;
