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
    Port (  clk : in std_logic;
            rst : in std_logic;
            --frz : in std_logic;
            sel_in : in std_logic_vector(3 downto 0);            
            y_out : out std_logic_vector(3 downto 0);
            y_out_r : out std_logic_vector(3 downto 0));
end fsm_1;

architecture rtl of fsm_1 is
    signal slow_clk, slow_clk_p : std_logic; signal counter : unsigned (27 downto 0);
    signal slow_counter : unsigned (3 downto 0);
    signal sel :  std_logic_vector (3 downto 0);
    
    type state is (s_idle, s_0xxx, s_01xx, s_010x, s_0101);
    signal state_curr, state_next : state;

begin

p_cnt : process(clk,rst) is
    begin
        if rst = '1' then 
            counter <= (others => '0');
         
       elsif rising_edge(clk) then
        counter <= counter +1;
    end if;
 end process;
 
 slow_clk <= counter(24);
 
 p_slw_cnt : process(clk,rst,slow_clk) is
    begin
       if rst = '1' then 
           state_curr <= s_idle;           
       elsif rising_edge(clk) then
           slow_clk_p <= slow_clk;
           if slow_clk = '1' and slow_clk_p = '0' then  
                state_curr <= state_next;
           end if;
       end if;
 end process;



p_cmb : process(state_curr, sel_in) is
    begin
   
    case state_curr is 
        when s_idle =>
            y_out_r(3 downto 0) <= "0000";
            y_out(3 downto 0) <= "1111";
            if sel_in(0) = '0' then 
                state_next <= s_0xxx;
            elsif sel_in(0) = '1' then 
                state_next <= s_idle;
            end if;
            
        when s_0xxx =>
            y_out_r(3 downto 0) <= "0000";
            y_out(3 downto 0) <= "1111";
            if sel_in(1 downto 0) = "10" then 
                state_next <= s_01xx;
            elsif sel_in(1 downto 0) = "00" then 
                state_next <= s_0xxx;
            end if; 
            
        when s_01xx =>
            y_out_r(3 downto 0) <= "0000";
            y_out(3 downto 0) <= "1111";
            if sel_in(2 downto 0) = "010" then 
                state_next <= s_010x;
            elsif sel_in(2 downto 0) = "110" then
                state_next <= s_idle;
            end if; 
            
                        
        when s_010x =>
            y_out_r(3 downto 0) <= "0000";
            y_out(3 downto 0) <= "1111";
            if sel_in(3 downto 0) = "1010" then 
                state_next <= s_0101;               
            elsif sel_in(3 downto 0) = "0010" then 
                state_next <= s_0xxx;
            end if; 
            
        when s_0101 =>
            state_next <= s_idle;
            y_out_r(3 downto 0) <= "1111";
            y_out(3 downto 0) <= "0000";
        when others => state_next <= s_idle;        
    end case;      
       
end process;

end rtl;