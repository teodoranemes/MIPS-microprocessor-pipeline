library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_arith.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;


entity MPG is
    Port(
    clk: in std_logic;
    input: in std_logic;
    en: out std_logic
    );
end MPG;

architecture Behavioral of MPG is

signal q1: std_logic;
signal q2: std_logic;
signal q3: std_logic;
signal cnt: std_logic_vector(15 downto 0):= x"0000";

begin

en <= q2 and (not q3);

process(clk)
begin
    if rising_edge(clk) then
        cnt <= cnt + 1;
    end if;
end process;

process(clk)
begin
    if rising_edge(clk) then
        if cnt = x"FFFF" then
            q1 <= input;
        end if;
    end if;
end process;

process(clk)
begin
    if rising_edge(clk) then
        q2 <= q1;
        q3 <= q2;
     end if;
end process;

end Behavioral;
