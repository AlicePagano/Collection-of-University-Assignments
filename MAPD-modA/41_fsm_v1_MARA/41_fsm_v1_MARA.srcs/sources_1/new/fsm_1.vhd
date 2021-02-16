----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/25/2019 05:38:00 PM
-- Design Name: 
-- Module Name: blink - Behavioral
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity fsm_1 is
    Port (clk : in std_logic;
            rst : in std_logic;
            --frz : in std_logic;
            sel_in : in std_logic_vector(3 downto 0);            
            y_out : out std_logic_vector(3 downto 0));
end fsm_1;

architecture rtl of fsm_1 is
    signal slow_clk, slow_clk_p : std_logic; signal counter : unsigned (27 downto 0);
    signal slow_counter : unsigned (3 downto 0);
    signal sel :  std_logic_vector (2 downto 0);
    
    type state is (s_000, s_001, s_010, s_011, s_100, s_101, s_110, s_111);
    signal state_curr, state_next : state;

begin

--sel <= sel_in(3 downto 1);

p_cnt : process(clk,rst) is
    begin
        if rst = '1' then 
            counter <= (others => '0');
         
       elsif rising_edge(clk) then
        counter <= counter +1;
    end if;
 end process;
 
 slow_clk <= counter(26);
 
 p_slw_cnt : process(clk,rst,slow_clk) is
    begin
       if rst = '1' then 
           state_curr <= s_000;           
       elsif rising_edge(clk) then
           slow_clk_p <= slow_clk;
           if slow_clk = '1' and slow_clk_p = '0' then  
                state_curr <= state_next;
           end if;
       end if;
 end process;

y_out(3) <= '0';

p_cmb : process(state_curr, sel_in) is
    begin
   
    case state_curr is 
        when s_000 =>
            y_out(2 downto 0) <= "000";
            if      sel_in(0) = '0' then state_next <= s_111;
            elsif   sel_in(0) = '1' then state_next <= s_001;
            end if;
        when s_001 =>
            y_out(2 downto 0) <= "001";
            if      sel_in(0) = '0' then state_next <= s_000;
            elsif   sel_in(0) = '1' then state_next <= s_010;
            end if;
        when s_010 =>
            y_out(2 downto 0) <= "010";
            if      sel_in(0) = '0' then state_next <= s_001;
            elsif   sel_in(0) = '1' then state_next <= s_011;
            end if;
        when s_011 =>
            y_out(2 downto 0) <= "011";
            if      sel_in(0) = '0' then state_next <= s_010;
            elsif   sel_in(0) = '1' then state_next <= s_100;
            end if;
        when s_100 =>
            y_out(2 downto 0) <= "100";
            if      sel_in(0) = '0' then state_next <= s_011;
            elsif   sel_in(0) = '1' then state_next <= s_101;
            end if;
        when s_101 =>
            y_out(2 downto 0) <= "101";
            if      sel_in(0) = '0' then state_next <= s_100;
            elsif   sel_in(0) = '1' then state_next <= s_110;
            end if;  
        when s_110 =>
            y_out(2 downto 0) <= "110";
            if      sel_in(0) = '0' then state_next <= s_101;
            elsif   sel_in(0) = '1' then state_next <= s_111;
            end if;
        when s_111 =>
            y_out(2 downto 0) <= "111";
            if      sel_in(0) = '0' then state_next <= s_110;
            elsif   sel_in(0) = '1' then state_next <= s_000;
            end if;
    end case;      
       
end process;

--y_out <= std_logic_vector(slow_counter);

end rtl;