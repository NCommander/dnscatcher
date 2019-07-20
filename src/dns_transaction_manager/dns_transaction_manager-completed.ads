-- Copyright 2019 Michael Casadevall <michael@casadevall.pro>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
-- sell copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
-- THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
-- DEALINGS IN THE SOFTWARE.

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
