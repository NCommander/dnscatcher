package body DNS_Transaction_Manager.Completed is
   protected body Completed_Transactions_Type is
      entry Add_Transaction(Client_Packet: Parsed_DNS_Packet; Server_Packet: Parsed_DNS_Packet)
        when Standard.True
      is
         
      begin
         null;
      end Add_Transaction;
      
      entry Pop_Server_Response(Server_IP: Unbounded_String; Server_Port: Integer; Transaction_ID: Unsigned_16; Parsed_Packet: out Parsed_DNS_Packet)
      when Standard.True
      is
         
      begin
         null;
      end Pop_Server_Response;
      
      entry Empty
        when Standard.True
      is
         
      begin
         null;
      end Empty;
      

   end Completed_Transactions_Type;
   

end DNS_Transaction_Manager.Completed;
