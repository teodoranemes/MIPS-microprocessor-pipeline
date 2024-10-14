library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;


entity EX is
     Port (
      RD1: in std_logic_vector(31 downto 0);
      ALUSrc: in std_logic;
      RD2: in std_logic_vector(31 downto 0);
      Ext_Imm: in std_logic_vector(31 downto 0);
      sa: in std_logic_vector(4 downto 0);
      func: in std_logic_vector(5 downto 0);
      ALUOp: in std_logic_vector(5 downto 0);
      PC_4: in std_logic_vector(31 downto 0);
      Zero: out std_logic;
      ALURes: out std_logic_vector(31 downto 0);
      Branch_Address: out std_logic_vector(31 downto 0);
      rt : in STD_LOGIC_VECTOR(4 downto 0);
      rd : in STD_LOGIC_VECTOR(4 downto 0);
      reg_dst : in STD_LOGIC;
      dest_registru : out STD_LOGIC_VECTOR(4 downto 0)
      );
end EX;

architecture Behavioral of EX is

signal ALUCtrl: std_logic_vector(3 downto 0);
signal ALURes_copy: std_logic_vector(31 downto 0);
signal B: std_logic_vector(31 downto 0);
signal shift_value: std_logic_vector(31 downto 0);

begin

ALURes <= ALURes_copy;
shift_value <= Ext_Imm(29 downto 0) & '0' & '0';
Branch_Address <= shift_value + PC_4;

process(ALUOp, func)
begin

case ALUOp is

when "000000" => 
    case func is  
        when "100000" => ALUCtrl <= "0000"; --add
        when "100010" => ALUCtrl <= "0100"; --sub
        when "000001" => ALUCtrl <= "0011"; --sll 
        when "000010" => ALUCtrl <= "0101"; --srl 
        when "100101" => ALUCtrl <= "0010"; --or
        when "100100" => ALUCtrl <= "0001"; --and
        when "100111" => ALUCtrl <= "0111";--xor
        when others => ALUCtrl <= "0011"; --sllv
    end case;

when "001000" => ALUCtrl <= "0000"; --addi
when "100011" => ALUCtrl <= "0000"; --lw
when "101011" => ALUCtrl <= "0000"; --sw
when "000100" => ALUCtrl <= "0100"; --beq
when "001011" => ALUCtrl <= "0010"; --ori
when "001001" => ALUCtrl <= "0111"; --xori
when others => ALUCtrl <= "0000";
end case;
end process;

process(ALUCtrl)
begin

case ALUCtrl is 

when "0000" => ALURes_copy <= RD1 + B;
when "0100" => ALURes_copy <= RD1 - B;
when "0011" => ALURes_copy <= to_stdlogicvector(to_bitvector(RD1) sll conv_integer(sa));
when "0101" => ALURes_copy <= to_stdlogicvector(to_bitvector(RD1) srl conv_integer(sa));
when "0010" => ALURes_copy <= RD1 or B;
when "0001" => ALURes_copy <= RD1 and B;
when "0111" => ALURes_copy <= RD1 xor B;
when others => ALURes_copy <= RD1 + B;
end case;

end process;

process(ALURes_copy)
begin

if ALURes_copy = x"00000000" then
    Zero <= '1';
else
    Zero <= '0';
end if;

end process;

process(ALUSrc, RD2, Ext_Imm)
begin

case ALUSrc is

when '0' => B <= RD2;
when others => B <= Ext_Imm;

end case;

end process;

process(reg_dst, rt, rd)
begin
    if reg_dst = '0' then
        dest_registru <= rt;
    else 
        dest_registru <= rd;
    end if;
end process;


end Behavioral;