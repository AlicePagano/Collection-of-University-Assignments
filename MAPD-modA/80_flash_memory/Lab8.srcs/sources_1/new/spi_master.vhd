library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity spi_master is
   generic (
      WTIME    : integer   := 100;
      TXBITS   : integer   := 16;
      RXBITS   : integer   := 8
      );
   port ( 
      clock    : in  std_logic;
      reset    : in  std_logic;
      txd      : in  std_logic_vector(TXBITS-1 downto 0);
      rxd      : out std_logic_vector(RXBITS-1 downto 0);
      start    : in  std_logic;
      ready    : out std_logic;
      miso     : in  std_logic;
      mosi     : out std_logic;
      sclk     : out std_logic;
      cs       : out std_logic
      );
end spi_master;

architecture Behavioral of spi_master is

   type state_t is (s_idle, s_send, s_receive);
   signal state   : state_t;
   signal start_s : std_logic;
   signal bufout  : std_logic_vector(txd'range);
   signal bufin, s_rxd   : std_logic_vector(rxd'range);
   signal mosi_s  : std_logic;
   signal cs_s    : std_logic;
   signal sclk_s  : std_logic;
   
   
begin

   process(clock, reset, start)
      variable tcnt, wcnt, rcnt : integer;
	  variable start_p : std_logic;
   begin
      if rising_edge(clock) then
         if reset = '1' then
            tcnt    := 0;
            wcnt    := TXBITS - 1;
			rcnt    := RXBITS - 1;
            state   <= s_idle;
			bufout  <= (others => '0');
            ready   <= '0';
			start_p := '0';
            cs_s    <= '1';
			sclk_s  <= '0';
         else
            case state is
            
            when s_idle =>
               cs_s <= '1';
			   ready  <= '0';
			   sclk_s <= '0';
               if start = '1' and start_p = '0' then  
                  state  <= s_send;
                  bufout <= txd;                  
               end if;
     
             when s_send => -- write the command + address 
               cs_s <= '0';
			   ready <= '0';
               tcnt := tcnt + 1;
               if tcnt = 1 then
				   sclk_s <= '0';
                   mosi_s <= bufout(wcnt);
               elsif tcnt = WTIME / 2 then
                   sclk_s <= '1';
               elsif tcnt = WTIME then
                   tcnt := 0;
                   sclk_s <= '0';
                   if wcnt = 0 then
                     state <= s_receive;
                     wcnt := TXBITS - 1;
                     mosi_s <= '1';
                   else
                     wcnt := wcnt - 1;
                   end if;
               end if;
	
             when s_receive => -- read the register data  
               cs_s <= '0';
               tcnt := tcnt + 1;
               if tcnt = 1 then
				 sclk_s <= '0';	
				 ready  <= '0';
               elsif tcnt = WTIME / 2 then
                  sclk_s <= '1';
                  bufin(rcnt) <= miso;
				  ready  <= '0';
               elsif tcnt = WTIME then
                  tcnt := 0;
                  sclk_s <= '0';
                  if rcnt = 0 then
                     state <= s_idle;
                     ready <= '1';
                     rcnt := RXBITS - 1;
                     rxd <= bufin;
                  else
					 ready  <= '0';
                     rcnt := rcnt - 1;
                  end if;
               end if;
            end case;
            start_p := start;
         end if;
      end if;
   end process;

   sclk <= sclk_s;
   mosi <= mosi_s;
   cs   <= cs_s;
   
end Behavioral;

