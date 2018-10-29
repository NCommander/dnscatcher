with System;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

package Raw_DNS_Packets is
   type DNS_Packet_Header is record
      Identifier              : Unsigned_16;
      Query_Response_Flag     : Boolean;
      Opcode                  : Unsigned_4;
      Authoritative_Answer    : Boolean;
      Truncated               : Boolean; -- Authoritive Answer
      Recursion_Desired       : Boolean; --
      Recursion_Available     : Boolean;
      Zero                    : Boolean;
      Authenticated_Data      : Boolean;
      Checking_Disabled       : Boolean;
      Response_Code           : Unsigned_4;
      Question_Count          : Unsigned_2;
      Answer_Record_Count     : Unsigned_2;
      Authority_Record_Count  : Unsigned_2; -- NSCount
      Additional_Record_Count : Unsigned_2;
   end record;
   for DNS_Packet_Header'Bit_Order use System.High_Order_First;
   for DNS_Packet_Header'Scalar_Storage_Order use System.High_Order_First;
   pragma Pack (DNS_Packet_Header);

end Raw_DNS_Packets;
