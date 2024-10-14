

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_arith.all;
use IEEE.STD_LOGIC_unsigned.all;



entity MEM is
  Port (
  MemWrite: in std_logic;
  ALUResIn: in std_logic_vector(31 downto 0);
  RD2: in std_logic_vector(31 downto 0);
  MemData: out std_logic_vector(31 downto 0);
  ALUResOut: out std_logic_vector(31 downto 0);
  Enable: in std_logic;
  Clk: in std_logic
   );
end MEM;

architecture Behavioral of MEM is

type RAM is array(0 to 63) of std_logic_vector(31 downto 0);
signal MEM_RAM: RAM :=(
            x"00000010",
            x"0000002C",
            x"00000008",
            x"00000200",
            x"0000003C",
            x"0000000C",
            x"00000014",
            x"0000001A",
            x"000000F4",
            x"0000005C",
            x"00000078",
            x"00000040",
            x"000000BC",
            x"00000064",
            x"00000020",
            x"0000007C",
            x"00000011",
            x"0000006E",
            x"000000A8",
            x"0000000D",
            x"0000002B",
            others => x"00000000"
);
      
begin

ALUResOut <= ALUResIn;

process(clk)
begin
    if rising_edge(clk) then
        if Enable = '1' then 
           if MemWrite = '1' then
                MEM_RAM(conv_integer(ALUResIn(5 downto 0))) <= RD2;
                MemData <= RD2;
            else
                MemData <= MEM_RAM(conv_integer(ALUResIn(5 downto 0)));
            end if;
        end if;
    end if;
end process;
    
end Behavioral;
