----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/20/2019 04:17:01 PM
-- Design Name: 
-- Module Name: mux41 - Behavioral
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

entity mux41 is
Port (a_in : in std_logic;
      b_in : in std_logic;
      c_in : in std_logic;
      d_in : in std_logic;      
      sel_in: in std_logic_vector(1 downto 0);
      y_out: out std_logic);
end mux41;

architecture rtl of mux41 is

component mux21 is
    port (a_in : in std_logic;
          b_in : in std_logic;
          sel_in : in std_logic;
          y_out : out std_logic);
end component;

signal y_m1_m3 : std_logic;
signal y_m2_m3 : std_logic;
  
begin

m1 : mux21 port map (a_in => a_in, b_in => b_in, sel_in => sel_in(0), y_out => y_m1_m3);
m2 : mux21 port map (a_in => c_in, b_in => d_in, sel_in => sel_in(0), y_out => y_m2_m3);
m3 : mux21 port map (a_in => y_m1_m3, b_in => y_m2_m3, sel_in => sel_in(1), y_out => y_out);

end rtl;
