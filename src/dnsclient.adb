with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DNS_Client;
with DNS_Core_Constructs; use DNS_Core_Constructs;

procedure DNSClient is
   DClient: DNS_Client.Client;
begin
   DClient.Create_Header;
   DClient.Add_Query(To_Unbounded_String("store.apple.com"), A, INternet);
   DClient.Run_Query;
end DNSClient;
