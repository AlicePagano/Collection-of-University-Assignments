----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/20/2019 03:30:53 PM
-- Design Name: 
-- Module Name: mux21 - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity mux21 is
Port (a_in : in std_logic;
      b_in : in std_logic;
      sel_in: in std_logic;
      y_out: out std_logic);
end mux21;

architecture rtl of mux21 is
begin

process(a_in, b_in, sel_in) is
begin
    if sel_in ='0' then
    y_out <= a_in;
    else
    y_out <= b_in;
    end if;
    end process;
end rtl;
