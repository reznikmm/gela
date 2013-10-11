------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Host;

package body Gela.Test_Cases.Runner is

   ------------
   -- Create --
   ------------

   function Create
     (Command   : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector;
      Directory : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String;
      Info      : Base.Base_Information := Base.No_Info)
      return Test_Case
   is
   begin
      return Result : Test_Case :=
        (Base.Test_Case with
           Command   => Command,
           Arguments => Arguments,
           Directory => Directory,
           Status    => Error,
           Output    => <>,
           Traceback => <>)
      do
         Result.Set_Information (Info);
      end return;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Self : in out Test_Case) is
      Code : Integer;
   begin
      Host.Execute
        (Command     => Self.Command,
         Arguments   => Self.Arguments,
         Exit_Code   => Code,
         Output      => Self.Output,
         Output_File => Self.Name,
         Directory   => Self.Directory);

      Self.Status := (if Code = 0 then Success else Failure);
   end Execute;

   ------------
   -- Status --
   ------------

   overriding function Status (Self : Test_Case) return Status_Kind is
   begin
      return Self.Status;
   end Status;

   ------------
   -- Output --
   ------------

   overriding function Output
     (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.Output;
   end Output;

   ---------------
   -- Traceback --
   ---------------

   overriding function Traceback
     (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.Traceback;
   end Traceback;

end Gela.Test_Cases.Runner;
