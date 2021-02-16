library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library unisim;
use unisim.vcomponents.all;

entity top is
  port(
    clk_base_xc7a_i : in std_logic;   
    -- FLASH Signals
    flash_mosi_o      : out std_logic;
    flash_miso_i      : in  std_logic;
    flash_clk_o       : out std_logic;
    flash_cs_o        : out std_logic;
    flash_w_o         : out std_logic -- write protect. To set low.    
     
    );
end top;

architecture rtl of top is

signal vio_rst, s_start, s_ready_fsm: std_logic;
signal vio_addr: std_logic_vector(3 downto 0);

-- FLASH NOR signals 
signal flash_rxd: std_logic_vector(7 downto 0);
signal flash_txd: std_logic_vector(8*4  -1 downto 0);
signal mac_retrieved : std_logic;
	
signal flash_mosi_s : std_logic;
signal flash_miso_s : std_logic;
signal flash_clk_s : std_logic;
signal flash_cs_s : std_logic;

component spi_master is
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
end component;    

COMPONENT ila_0

  PORT (
    clk : IN STD_LOGIC;



    probe0 : IN STD_LOGIC_VECTOR(7 DOWNTO 0)
    );
END COMPONENT  ;

COMPONENT vio_0
  PORT (
    clk : IN STD_LOGIC;
    probe_out0 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    probe_out1 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    probe_out2 : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
  );
END COMPONENT;

begin

   master : spi_master 
   generic map (
      WTIME    => 6,
      TXBITS   => 8*4,
	  RXBITS   => 8
      )
   port map( 
      clock    => clk_base_xc7a_i,
      reset    => vio_rst,
      txd      => flash_txd,
      rxd      => flash_rxd,
      start    => s_start,
      ready    => s_ready_fsm,
      miso     => flash_miso_s,
      mosi     => flash_mosi_s,
      sclk     => flash_clk_s,
      cs       => flash_cs_s
      );
      
   flash_w_o   <= '0';
   flash_mosi_o <= flash_mosi_s;
   flash_miso_s <= flash_miso_i;
   flash_clk_o  <= flash_clk_s;
   flash_cs_o   <= flash_cs_s;   

   flash_txd <= x"03" & x"00000" & vio_addr;

vio : vio_0
  PORT MAP (
    clk => clk_base_xc7a_i,
    probe_out0(0) => vio_rst,
    probe_out1(0) => s_start,
    probe_out2 => vio_addr
  );
  
   ila : ila_0
     PORT MAP (
       clk => clk_base_xc7a_i,
       probe0 => flash_rxd
       );

end architecture;
