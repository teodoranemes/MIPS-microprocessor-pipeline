library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

entity test_env is
    Port ( clk : in STD_LOGIC;
           btn : in STD_LOGIC_VECTOR (4 downto 0);
           sw : in STD_LOGIC_VECTOR (15 downto 0);
           led : out STD_LOGIC_VECTOR (15 downto 0);
           an : out STD_LOGIC_VECTOR (7 downto 0);
           cat : out STD_LOGIC_VECTOR (6 downto 0));
end test_env;


architecture Behavioral of test_env is

component MPG is
    Port(
    clk: in std_logic;
    input: in std_logic;
    en: out std_logic
    );
end component;

component SSD is
    Port ( cnt : in STD_LOGIC_VECTOR (31 downto 0);
           clk : in STD_LOGIC;
           an : out STD_LOGIC_VECTOR (7 downto 0);
           cat : out STD_LOGIC_VECTOR (6 downto 0));
end component;

component IFetch is
    Port (clk : in STD_LOGIC;
          rst : in STD_LOGIC;
          en : in STD_LOGIC;
          BranchAddress : in STD_LOGIC_VECTOR(31 downto 0);
          JumpAddress : in STD_LOGIC_VECTOR(31 downto 0);
          Jump : in STD_LOGIC;
          PCSrc : in STD_LOGIC;
          Instruction : out STD_LOGIC_VECTOR(31 downto 0);
          PCp4 : out STD_LOGIC_VECTOR(31 downto 0));
end component;

component ID is
     Port (
     clk: in std_logic;
     RegWrite: in std_logic;
     Instr: in std_logic_vector(31 downto 0);
     ExtOp: in std_logic;
     RegDst: in std_logic_vector(4 downto 0);
     En : in STD_LOGIC;
     WD: in std_logic_vector(31 downto 0);
     RD1: out std_logic_vector(31 downto 0);
     RD2: out std_logic_vector(31 downto 0);
     Ext_imm: out std_logic_vector(31 downto 0);
     funct: out std_logic_vector(5 downto 0);
     sa: out std_logic_vector(4 downto 0);
     rt : out STD_LOGIC_VECTOR(4 downto 0);
     rd : out STD_LOGIC_VECTOR(4 downto 0)
     ); 
end component;

component MEM is
  Port (
  MemWrite: in std_logic;
  ALUResIn: in std_logic_vector(31 downto 0);
  RD2: in std_logic_vector(31 downto 0);
  MemData: out std_logic_vector(31 downto 0);
  ALUResOut: out std_logic_vector(31 downto 0);
  Enable: in std_logic;
  Clk: in std_logic
   );
end component;

component EX is
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
end component;

signal cnt : std_logic_vector(5 downto 0) := (others => '0');
signal enable:  STD_LOGIC;
signal intrare_SSD : std_logic_vector(31 downto 0) := (others => '0');
signal A : std_logic_vector(31 downto 0);
signal B : std_logic_vector(31 downto 0);
signal C : std_logic_vector(31 downto 0);



signal instr_out: STD_LOGIC_VECTOR(31 downto 0);
signal pc_out: STD_LOGIC_VECTOR(31 downto 0);
signal br_addr: STD_LOGIC_VECTOR(31 downto 0):=x"00000000";
signal bne_addr: STD_LOGIC_VECTOR(31 downto 0):=x"00000000";
signal j_addr: STD_LOGIC_VECTOR(31 downto 0):=x"00000000";
signal jump: std_logic;
signal pc_src: std_logic;


signal reg_write: std_logic;
signal reg_dst: std_logic;
signal ext_op: std_logic;
signal wd_wb_id: std_logic_vector(31 downto 0) := x"00000002";
signal rd1: std_logic_vector(31 downto 0);
signal rd2: std_logic_vector(31 downto 0);
signal ext_imm: std_logic_vector(31 downto 0);
signal func: std_logic_vector(5 downto 0);
signal sa: std_logic_vector(4 downto 0);
signal mem_write : std_logic;
signal mem_to_reg : std_logic;
signal rt : std_logic_vector(4 downto 0);
signal rd : std_logic_vector(4 downto 0);


signal alu_src : std_logic;
signal alu_res : std_logic_vector(31 downto 0);
signal zero : std_logic;
signal mem_data : std_logic_vector(31 downto 0);
signal alu_res_out : std_logic_vector(31 downto 0);
signal dest_registru : std_logic_vector(4 downto 0);



-- IF/ID
--signal enable : std_logic;
signal pc_plus_if_id : std_logic_vector(31 downto 0);
signal instruction_if_id : std_logic_vector(31 downto 0);

-- ID/EX
signal pc_plus_id_ex : std_logic_vector(31 downto 0);
signal rd1_id_ex : std_logic_vector(31 downto 0);
signal rd2_id_ex : std_logic_vector(31 downto 0);
signal sa_id_ex : std_logic_vector(4 downto 0);
signal ext_imm_id_ex : std_logic_vector(31 downto 0);
signal func_id_ex : std_logic_vector(5 downto 0);
signal rt_id_ex : std_logic_vector(4 downto 0);
signal rd_id_ex : std_logic_vector(4 downto 0);
signal mem_to_reg_id_ex : std_logic;
signal reg_write_id_ex : std_logic;
signal mem_write_id_ex : std_logic;
signal pc_src_id_ex : std_logic;
signal alu_op_id_ex : std_logic_vector(5 downto 0);
signal alu_src_id_ex : std_logic;
signal reg_dst_id_ex : std_logic;

-- EX/MEM
signal reg_write_ex_mem : std_logic;
signal mem_to_reg_ex_mem : std_logic;
signal mem_write_ex_mem : std_logic;
signal pc_src_ex_mem : std_logic;
signal branch_addr_ex_mem : std_logic_vector(31 downto 0);
signal zero_ex_mem : std_logic;
signal alu_res_ex_mem: std_logic_vector(31 downto 0);
signal rt_ex_mem: std_logic_vector(31 downto 0);
signal dest_registru_ex_mem: std_logic_vector(4 downto 0);

-- MEM/WB
signal reg_write_mem_wb : std_logic;
signal mem_to_reg_mem_wb : std_logic;
signal mem_data_mem_wb : std_logic_vector(31 downto 0);
signal alu_res_mem_wb : std_logic_vector(31 downto 0);
signal dest_registru_mem_wb : std_logic_vector(4 downto 0);


signal pc_src_mem_if : std_logic;


begin

monopuls : MPG port map( clk => clk, input => btn(0),  en => enable);
display : SSD port map(cnt=>intrare_SSD, clk=>clk, an=>an, cat=>cat); 
instr_IF : IFETCH port map(clk=>clk, rst=>btn(1), en=>enable, BranchAddress=>branch_addr_ex_mem, JumpAddress=>j_addr, Jump=>jump, PCSrc=>pc_src_mem_if, Instruction=>instr_out, PCp4=>pc_out);
instr_ID : ID port map(clk=>clk, RegWrite=>reg_write_mem_wb, Instr=>instruction_if_id, ExtOp=>ext_op, RegDst=>dest_registru_mem_wb, En=>enable, WD=>wd_wb_id, RD1=>rd1, RD2=>rd2, Ext_imm=>ext_imm, funct=>func, sa=>sa, rt=>rt, rd=>rd);
instr_MEM : MEM port map( MemWrite=>mem_write_ex_mem, ALUResIn=>alu_res_ex_mem, RD2=>rt_ex_mem, MemData=>mem_data, ALUResOut=>alu_res_out, Enable=>'1', clk=>clk);
instr_EX : EX port map(RD1=>rd1_id_ex, AluSrc=>alu_src_id_ex, RD2=>rd2_id_ex, Ext_Imm=>ext_imm_id_ex, sa=>sa_id_ex,  func=>func_id_ex, ALUOp=>alu_op_id_ex, PC_4=>pc_plus_id_ex, zero=>zero, ALURes=>alu_res, Branch_Address=>br_addr, rt=>rt_id_ex, rd=>rd_id_ex, reg_dst=>reg_dst_id_ex, dest_registru=>dest_registru);

j_addr <= pc_plus_if_id(31 downto 28) & instruction_if_id(25 downto 0) & "00";
pc_src_mem_if <= pc_src_ex_mem AND zero_ex_mem;

process(sw(15 downto 10))
begin
    case sw(15 downto 10) is 
        when "000000" => intrare_SSD <= instr_out;
        when "000001" => intrare_SSD <= instruction_if_id;
        when "000010" => intrare_SSD <= pc_out;
        when "000011" => intrare_SSD <= pc_plus_if_id;
        when "000100" => intrare_SSD <= pc_plus_id_ex;
        when "000101" => intrare_SSD <= rd1;
        when "000110" => intrare_SSD <= rd1_id_ex;
        when "000111" => intrare_SSD <= rd2;
        when "001000" => intrare_SSD <= rd2_id_ex;
        when "001001" => intrare_SSD <= rt_ex_mem;
        when "001010" => intrare_SSD <= "000000000000000000000000000" & rt;
        when "001011" => intrare_SSD <= "000000000000000000000000000" & rd;
        when "001100" => intrare_SSD <= "0000000000000000000000000000000" & reg_dst;
        when "001101" => intrare_SSD <= "000000000000000000000000000" & dest_registru_ex_mem;
        when "001110" => intrare_SSD <= "000000000000000000000000000" & dest_registru_mem_wb;
        when "001111" => intrare_SSD <= ext_imm;
        when "010000" => intrare_SSD <= ext_imm_id_ex;
        when "010011" => intrare_SSD <= "0000000000000000000000000000000" & zero;
        when "010100" => intrare_SSD <= "0000000000000000000000000000000" & zero_ex_mem;
        when "010110" => intrare_SSD <= branch_addr_ex_mem;
        when "010111" => intrare_SSD <= mem_data;
        when "011000" => intrare_SSD <= mem_data_mem_wb;
        when "011001" => intrare_SSD <="0000000000000000000000000000000" &  mem_write_ex_mem;
        when "011010" => intrare_SSD <="0000000000000000000000000000000" &  reg_write_mem_wb;
        when "011011" => intrare_SSD <= alu_res;
        when "011100" => intrare_SSD <= alu_res_ex_mem; 
        when "011101" => intrare_SSD <= "0000000000000000000000000000000" & alu_src;
        when "011110" => intrare_SSD <= "0000000000000000000000000000000" & mem_to_reg_mem_wb;
        when "011111" => intrare_SSD <= wd_wb_id;
        when "100000" => intrare_SSD <= alu_res_out;
        when "100001" => intrare_SSD <= "000000000000000000000000000" & rd_id_ex;
        when "100010" => intrare_SSD <= "000000000000000000000000000" & rt_id_ex;
        when "100011" => intrare_SSD <= "0000000000000000000000000000000" & mem_to_reg;
        when "100100" => intrare_SSD <= "0000000000000000000000000000000" & mem_to_reg_id_ex;
        when "100101" => intrare_SSD <= "0000000000000000000000000000000" & mem_to_reg_ex_mem;
        when others => intrare_SSD <= x"AAAAAAAA";
    end case;
end process;

process(instruction_if_id)
begin
case instruction_if_id(31 downto 26) is
when "000000" => pc_src <= '0';     -- operatii de tip R
                         jump <= '0';
                         reg_dst <= '1';
                         reg_write <= '1';
                         ext_op <= '0';
                         mem_write <= '0';
                         mem_to_reg <= '0';
                         alu_src <= '0';
        when "001000" => pc_src <= '0';     -- addi
                         jump <= '0';
                         reg_dst <= '0';
                         reg_write <= '1';
                         ext_op <= '1';
                         mem_write <= '0';
                         mem_to_reg <= '0'; 
                         alu_src <= '1';
        when "100011" => pc_src <= '0';     -- lw 
                         jump <= '0';
                         reg_dst <= '0';
                         reg_write <= '1';
                         ext_op <= '1';
                         mem_write <= '0';
                         mem_to_reg <= '1';
                         alu_src <= '1'; 
        when "101011" => pc_src <= '0';      --sw
                         jump <= '0';
                         reg_dst <= '0';
                         reg_write <= '0';
                         ext_op <= '1';
                         mem_write <= '1';
                         mem_to_reg <= '0';
                         alu_src <= '1'; 
        when "000100" => pc_src <= '1';     -- beq 
                         jump <= '0';
                         reg_dst <= '0';
                         reg_write <= '0';
                         ext_op <= '1';
                         mem_write <= '0';
                         mem_to_reg <= '0';
                         alu_src <= '0';
    
        when "000111" => pc_src <= '0';     -- j 
                         jump <= '1';
                         reg_dst <= '0';
                         reg_write <= '0';
                         ext_op <= '0';
                         mem_write <= '0';
                         mem_to_reg <= '0';
                         alu_src <= '0';
         
         when "001011" => pc_src <= '0';     --ori
                         jump <= '0';
                         reg_dst <= '1';
                         reg_write <= '1';
                         ext_op <= '0';
                         mem_write <= '0';
                         mem_to_reg <= '0';
                         alu_src <= '1';
                         
         when "001001" => pc_src <= '0';     --xori
                         jump <= '0';
                         reg_dst <= '1';
                         reg_write <= '1';
                         ext_op <= '0';
                         mem_write <= '0';
                         mem_to_reg <= '0';
                         alu_src <= '1';
                         
         when others => pc_src <= '0';
                         jump <= '0';
                         reg_dst <= '0';
                         reg_write <= '0';
                         ext_op <= '0';
                         mem_write <= '0';
                         mem_to_reg <= '0';
                         alu_src <= '0';
    end case;                 
end process;

process(mem_to_reg_mem_wb, mem_data_mem_wb, alu_res_mem_wb)
begin
    if mem_to_reg_mem_wb = '0' then
        wd_wb_id <= alu_res_mem_wb;
    else 
        wd_wb_id <= mem_data_mem_wb;
    end if; 
end process;

-- IF/ID
process(clk)
begin
    if rising_edge(clk) then
        if enable = '1' then
            pc_plus_if_id <= pc_out;
            instruction_if_id <= instr_out;
        end if;
    end if;
end process;

-- ID/EX
process(clk)
begin
    if rising_edge(clk) then
        if enable = '1' then
            pc_plus_id_ex <= pc_plus_if_id;
            rd1_id_ex <= rd1;
            rd2_id_ex <= rd2;
            sa_id_ex <= sa;
            ext_imm_id_ex <= ext_imm;
            func_id_ex <= func;
            rt_id_ex <= rt;
            rd_id_ex <= rd;
            mem_to_reg_id_ex <= mem_to_reg;
            reg_write_id_ex <= reg_write;
            mem_write_id_ex <= mem_write;
            pc_src_id_ex <= pc_src;
            alu_op_id_ex <= instruction_if_id(31 downto 26);
            alu_src_id_ex <= alu_src;
            reg_dst_id_ex <= reg_dst;  
        end if;
    end if;
end process;

-- EX/MEM
process(clk)
begin
    if rising_edge(clk) then
        if enable = '1' then
            reg_write_ex_mem <= reg_write_id_ex;
            mem_to_reg_ex_mem <= mem_to_reg_id_ex;
            mem_write_ex_mem <= mem_write_id_ex;
            pc_src_ex_mem <= pc_src_id_ex;
            branch_addr_ex_mem <= br_addr;
            zero_ex_mem <= zero;
            alu_res_ex_mem <= alu_res;
            rt_ex_mem <= rd2_id_ex;  
            dest_registru_ex_mem <= dest_registru;
        end if;
    end if;
end process;

-- MEM/WB
process(clk)
begin
    if rising_edge(clk) then
        if enable = '1' then
            reg_write_mem_wb <= reg_write_ex_mem;
            mem_to_reg_mem_wb <= mem_to_reg_ex_mem;
            mem_data_mem_wb <= mem_data;
            alu_res_mem_wb <= alu_res_out;
            dest_registru_mem_wb <= dest_registru_ex_mem;
        end if;
    end if;
end process;


end Behavioral;