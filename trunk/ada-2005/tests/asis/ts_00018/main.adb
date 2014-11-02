with Ada.Command_Line;
with Ada.Wide_Text_IO;
with Ada.Characters.Conversions;
with Ada.Containers;

with Asis;
with Asis.Implementation;
with Asis.Ada_Environments;
with Asis.Compilation_Units;

with Process_Unit;
with Traversing_Actions;

procedure Main is

   Context : Asis.Context;
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Ada.Wide_Text_IO.Put_Line ("Usage: main <input_ada_file> <print_log?>");
      return;
   end if;
   
   declare
      Input   : constant String := Ada.Command_Line.Argument (1);
      Args    : constant Wide_String := "-I../../../source/asis/spec" &
        " -I../../../source/asis/spec/obsolescent" &
        " -I../../../../acats/include "
        & Ada.Characters.Conversions.To_Wide_String (Input);
   begin
      Asis.Implementation.Initialize ("");

      Asis.Ada_Environments.Associate
        (The_Context => Context,
         Name        => Asis.Ada_Environments.Default_Name,
         Parameters  => Args);
      
      Asis.Ada_Environments.Open (Context);
   
      declare
         use type Asis.Unit_Origins;
      
         function Check_Name (Name : Wide_String) return Boolean;
      
         ----------------
         -- Check_Name --
         ----------------

         function Check_Name (Name : Wide_String) return Boolean is
         begin
            if Name = "REPORT" then
               return False;
            elsif Name = "Ada" then
               return False;
            elsif Name = "System" then
               return False;
            elsif Name = "Interface" then
               return False;
            elsif Name = "Standard" then
               return False;
               --           elsif Name'Length > 4 and then
               --             Name (Name'First .. Name'First + 3) = "Ada."
               --           then
               --              return False;
               --           elsif Name'Length > 7 and then
               --             Name (Name'First .. Name'First + 6) = "System."
               --           then
               --              return False;
               --           elsif Name'Length > 10 and then
               --             Name (Name'First .. Name'First + 9) = "Interfaces."
               --           then
               --              return False;
            else
               return True;
            end if;
         end Check_Name;
      
         Need  : Boolean;
         State : Traversing_Actions.Traversal_State;
         Units : constant Asis.Compilation_Unit_List :=
           Asis.Compilation_Units.Compilation_Units (Context);
      begin
         for J in Units'Range loop
            Need := Asis.Compilation_Units.Unit_Kind (Units (J)) not in
              Asis.A_Nonexistent_Declaration .. Asis.An_Unknown_Unit;
         
            Need := Need and
              Asis.Compilation_Units.Unit_Origin (Units (J))
              = Asis.An_Application_Unit;

            if Need then
               Need := Check_Name
                 (Asis.Compilation_Units.Unit_Full_Name (Units (J)));
            end if;
         
            if Need then
               Process_Unit (Units (J), State);
            end if;
         end loop;
         
         if Ada.Command_Line.Argument_Count = 1 then
            declare
               Image : constant Wide_String :=
                 Ada.Containers.Hash_Type'Wide_Image
                   (Traversing_Actions.Get_Text_Hash (State));
            begin
               if Image (1) = ' ' then
                  Ada.Wide_Text_IO.Put_Line (Image (2 .. Image'Last));
               else
                  Ada.Wide_Text_IO.Put_Line (Image);
               end if;
            end;
         else
            Ada.Wide_Text_IO.Put_Line
              (Traversing_Actions.Get_Text (State));
         end if;
      end;
   end;

--   Asis.Ada_Environments.Close        (Context);
--   Asis.Ada_Environments.Dissociate   (Context);
--   Asis.Implementation.Finalize       ("");
end Main;
