with Ada.Unchecked_Conversion;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with System;

package body DNS_RData_Processor.Utils is
   function Decode_DNS_IPv4_Address
     (Parsed_RR : Parsed_DNS_Resource_Record) return Unbounded_String
   is
      type Raw_IPv4_Components is record
         A : Unsigned_8;
         B : Unsigned_8;
         C : Unsigned_8;
         D : Unsigned_8;
      end record;
      pragma Pack (Raw_IPv4_Components);
      for Raw_IPv4_Components'Bit_Order use System.High_Order_First;
      for Raw_IPv4_Components'Scalar_Storage_Order use System.High_Order_First;

      IPv4_Components : Raw_IPv4_Components;
      ASCII_IPv4      : Unbounded_String;

      function To_IPv4_Components is new Ada.Unchecked_Conversion (Source => String,
         Target                                                           => Raw_IPv4_Components);
   begin
      IPv4_Components := To_IPv4_Components (To_String (Parsed_RR.RData) (1..4));
      ASCII_IPv4 := ASCII_IPv4 & IPv4_Components.A'Image & IPv4_Components.B'Image & IPv4_Components.C'Image & IPv4_Components.D'Image;

      return ASCII_IPv4;
   end Decode_DNS_IPv4_Address;
end DNS_RData_Processor.Utils;
