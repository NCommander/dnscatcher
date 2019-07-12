with Packet_Processor_Test_Suite;
with AUnit.Run;
with AUnit.Reporter.Text;

procedure Test_Runner is
   procedure Run is new AUnit.Run.Test_Runner
     (Packet_Processor_Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Test_Runner;
