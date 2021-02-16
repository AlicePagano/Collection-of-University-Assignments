----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/20/2019 03:49:34 PM
-- Design Name: 
-- Module Name: tb_mux21 - Behavioral
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

entity tb_mux21 is
--  Port ( );
end tb_mux21;

architecture Behavioral of tb_mux21 is

component mux21 is 
Port (a_in : in std_logic;
      b_in : in std_logic;
      sel_in: in std_logic;
      y_out: out std_logic);
end component;

signal a, b, sel, y: std_logic;
begin
uut : mux21 Port map (a_in => a, b_in => b, sel_in => sel, y_out => y);

p_sel : process
begin
    sel <= '0' ; wait for 200 ns;
    sel <= '1' ; wait for 200 ns;  
end process;

p_ab : process
begin
    a<='0'; b<='0'; wait for 125 ns; 
    a<='0'; b<='1'; wait for 125 ns; 
    a<='1'; b<='0'; wait for 125 ns; 
    a<='1'; b<='1'; wait for 125 ns; 
end process;

end Behavioral;
