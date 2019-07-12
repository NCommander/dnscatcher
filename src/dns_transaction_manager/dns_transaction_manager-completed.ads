with DNS_Transaction_Manager; use DNS_Transaction_Manager;
with DNS_Packet_Processor;    use DNS_Packet_Processor;

-- Holds concluded DNS Transactions

-- When a server response is received, it gets sent here so clients can parse
-- data and process the next steps necessary to processing the data, either
-- making additional DNS requests, or just to get get their answers

package DNS_Transaction_Manager.Completed is
   protected type Completed_Transactions_Type is
      entry Add_Transaction
        (Client_Packet : Parsed_DNS_Packet;
         Server_Packet : Parsed_DNS_Packet);
      entry Pop_Server_Response
        (Server_IP      :     Unbounded_String;
         Server_Port    :     Integer;
         Transaction_ID :     Unsigned_16;
         Parsed_Packet  : out Parsed_DNS_Packet);
      entry Empty;
   private
      Transaction_Hashmap : DNS_Transaction_Maps.Map;
   end Completed_Transactions_Type;

   -- Global for logging queues
   Completed_Transactions : DNS_Transaction_Maps.Map;
end DNS_Transaction_Manager.Completed;
