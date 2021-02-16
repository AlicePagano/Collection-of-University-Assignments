----------------------------------------------------------------------------------
-- Company:
-- Engineer:
--
-- Create Date: 01/08/2020 02:40:51 PM
-- Design Name:
-- Module Name: tb_cnt - Behavioral
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

entity tb_cnt is
--  Port ( );
end tb_cnt;

architecture Behavioral of tb_cnt is

component cnt is
    Port (clk : in std_logic;
            rst : in std_logic;
            frz : in std_logic;
            start : in std_logic;
            sel_in : in std_logic_vector(3 downto 0);
            y_out : out std_logic_vector(3 downto 0));
end component;

signal clk : std_logic;
signal sel :  std_logic_vector (2 downto 0);
signal rev : std_logic;
signal state : std_logic := '1';
signal led_out : std_logic_vector (3 downto 0);
signal btn_frz : std_logic;
signal btn_start : std_logic;
signal btn_rst : std_logic;


begin

uut : cnt port map (clk => clk,
                    sel_in(0) => rev,
                    sel_in(1) => sel(0),
                    sel_in(2) => sel(1),
                    sel_in(3) => sel(2),
                    y_out => led_out,
                    frz => btn_frz,
                    start => btn_start,
                    rst => btn_rst);


speed : process
    begin
        sel <= "111";
        rev <= '0';
        wait;
    end process;


p_clk: process
begin
    clk <= '0'; wait for 5 ns;
    clk <= '1'; wait for 5 ns;
end process;

p_start: process
begin
    btn_rst <= '1'; wait for 50 ns;
    btn_rst <= '0'; wait for 50 ns;
    btn_start <= '1'; wait for 50 ns;
    btn_start <= '0'; wait for 1200 ns;
    btn_frz <= '1'; wait for 50 ns;
    btn_frz <= '0'; wait for 50 ns;
    
end process;

end Behavioral;