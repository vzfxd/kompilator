import utils
from sys import exit
from sly import Lexer as sly_Lexer
from sly import Parser as sly_Parser
from sys import argv

class Identifier():
    def __init__(self, name):
        self.name = name
    
    def declare_variables(var_list, program_memory):
        if(var_list == None): return []
        l = []
        memory_taken = 0
        for var in var_list:
            type, name, idx = Identifier.get_var(var)

            if(type == "ARR_NUM"):
                size = int(idx)
                var_obj = Array(name,size,program_memory+memory_taken)
                memory_taken += size
            else:
                var_obj = Variable(name)
            
            l.append(var_obj)

        return l,memory_taken
    
    def get_var(raw):
        var_type, var_name, var_idx = raw[0], raw[1], None
        if(len(raw) == 3):
            var_idx = raw[2]
        return var_type, var_name, var_idx

class Array(Identifier):
    def __init__(self, name, size, start_addr):
        super().__init__(name)
        self.start_addr = start_addr
        self.size = size

    def __repr__(self):
        return f"\n name:{self.name}, start_addr:{self.start_addr}, size:{self.size} \n"

class Variable(Identifier):
    def __init__(self, name=None):
        super().__init__(name)
        self.addr = None

    def set_addr(self, addr):
        self.addr = addr

    def __repr__(self):
        return f"\n name:{self.name}, addr:{self.addr}\n"

class Program():

    def __init__(self):
        self.procedures = []
        self.main_decl = []
        self.main_commands = []
        self.mem_cells_taken = 0

    def set_declarations(self, var_list):
        self.main_decl, cells_taken = Identifier.declare_variables(var_list,self.mem_cells_taken)
        self.mem_cells_taken += cells_taken

    def set_commands(self, commands):
        self.main_commands = commands

    def find_var(self, name, type="VAR", scope="main"):
        if(type == "VAR"):
            type = Variable
        else:
            type = Array

        var_list = None
        if(scope == "main"):
            var_list = self.main_decl
        else:
            pass
            #TODO: scope jakiejs procedury

        found = []
        for var in var_list:
            if(var.name == name):
                found.append(var)
            
        if(len(found) > 1):
            raise RuntimeError(f"identifier {name} declared more than once")
        
        if(len(found) == 0):
            raise RuntimeError(f"{type.__name__} {name} not declared")
        
        var = found[0]
        if(not isinstance(var,type)):
            raise RuntimeError(f"wrong usage of {var.__class__.__name__} {name}")
        
        return var
    
    def init_var(self, var: Variable):
        var.set_addr(self.mem_cells_taken)
        self.mem_cells_taken += 1

    def __repr__(self):
        x = f"PROCEDURES:\n\n{self.procedures} \n\n DECL IN MAIN:\n{self.main_decl}\n\n"
        for c in self.main_commands:
            x += f"{c}\n"
        return f"{x}\n\n MEM CELLS TAKEN:{self.mem_cells_taken}"

class CodeGen():
    def __init__(self, program: Program):
        self.program = program
        self.instructions = []

    def save_to_reg(self, val,reg):
        self.reset_regiser(reg)
        self.instructions += utils.reach_number(val,reg)

    def reset_regiser(self, reg):
        self.instructions += [f"RST {reg}"]

    def save_to_memory(self, reg):
        self.instructions += [f"STORE {reg}"]

    def load_from_memory(self, reg):
        self.instructions += [f"LOAD {reg}"]

    def add_reg_to_acc(self, reg):
        self.instructions += [f"ADD {reg}"]

    def save_var_addr_to_reg(self, var: Identifier, reg, idx, idx_type):
        if(reg == 'a'): raise RuntimeError("adrr saved to accumulator")
        if(isinstance(var,Variable)):
            self.save_to_reg(var.addr, reg)
        else:
            if(idx_type == "ARR_NUM"):
                self.save_to_reg(int(idx),'a')
            else:
                idx_var = self.program.find_var(idx)
                self.save_to_reg(idx_var.addr,reg)
                self.load_from_memory(reg)

            self.save_to_reg(var.start_addr,reg)
            self.add_reg_to_acc(reg)
            self.instructions += [f"PUT {reg}"]

    def gen(self):
        main_commands = self.program.main_commands

        for command in main_commands:
            type = command[0]

            if(type == "ASSIGN"):
                self.gen_assign(command[1:])

            if(type == "WRITE"):
                self.gen_write(command[1:])

        self.instructions.append("HALT")
    
    def gen_assign(self,command):
        exp_raw = command[1]
        var_raw = command[0]
        type, name, idx = Identifier.get_var(var_raw)
        
        var = self.program.find_var(name, type)
        if(isinstance(var,Variable) and var.addr == None):
            self.program.init_var(var)

        if(exp_raw[0] not in ["ARR_NUM","ARR_PID","NUM","VAR"]):
            pass
        else:
            val_type, val_name, val_idx = Identifier.get_var(exp_raw)

            self.save_var_addr_to_reg(var,'h',idx,type)
                
            if(val_type == "NUM"):
                self.save_to_reg(int(val_name),'a')
            else:
                val = self.program.find_var(val_name, val_type)
                if(isinstance(val,Variable) and val.addr == None):
                    raise RuntimeError(f"Variable {val.name} not initialized")
                self.save_var_addr_to_reg(val,'g',val_idx,val_type)
                self.load_from_memory('g')

            self.save_to_memory('h')

    def gen_write(self,command):
        var_raw = command[0]
        type, name, idx = Identifier.get_var(var_raw)
        var = self.program.find_var(name, type)

        if(isinstance(var,Variable) and var.addr == None):
            raise RuntimeError(f"Variable {var.name} not initialized")
        
        self.save_var_addr_to_reg(var,'h',idx,type)
        self.load_from_memory('h')
        self.instructions += [f"WRITE"]

class Lexer(sly_Lexer):
    tokens = {
        ASSIGN,
        IF,THEN,ELSE,ENDIF,
        WHILE,DO,ENDWHILE,
        REPEAT,UNTIL,
        READ,WRITE,
        PROGRAM,PROCEDURE,IS,IN,END,
        EQ, NEQ, GT, LT, GEQ, LEQ,
        NUM,PIDENTIFIER
    }

    literals = {
        '(',')',
        '[',']',
        ';','T',',',
        '+','-','*','/','%',
    }

    ignore_comment = r'\#.*'
    ignore_newline = r'\n+'
    ignore_spaces = r'[ \t]'

    ASSIGN = r':='

    IF = r'IF'
    THEN = r'THEN'
    ELSE = r'ELSE'
    ENDIF = r'ENDIF'

    WHILE = r'WHILE'
    DO = r'DO'
    ENDWHILE = r'ENDWHILE'

    REPEAT = r'REPEAT'
    UNTIL = r'UNTIL'

    READ = r'READ'
    WRITE = r'WRITE'

    PROGRAM = r'PROGRAM'
    PROCEDURE = r'PROCEDURE'
    IS = r'IS'
    IN = r'IN'
    END = r'END'

    NEQ = r'!='
    GEQ = r'>='
    LEQ = r'<='
    EQ = r'='
    GT = r'>'
    LT = r'<'

    PIDENTIFIER = r'[_a-z]+'
    NUM = r'\d+'

class Parser(sly_Parser):
    tokens = Lexer.tokens

    def __init__(self, program: Program):
        self.ctx = program

    @_('procedures main')
    def program_all(self, p):
       #self.ctx.set_procedures(p.procedures)
        self.ctx.set_declarations(p.main[0])
        self.ctx.set_commands(p.main[1])            
    
    @_('procedures PROCEDURE proc_head IS declarations IN commands END')
    def procedures(self, p):
        return p.procedures + [(p.proc_head, p.declarations, p.commands)]

    @_('procedures PROCEDURE proc_head IS IN commands END')
    def procedures(self, p):
        return p.procedures + [(p.proc_head, p.commands)]

    @_('')
    def procedures(self, p):
        return []
    
    @_('PROGRAM IS declarations IN commands END')
    def main(self, p):
        return (p.declarations, p.commands)

    @_('PROGRAM IS IN commands END')
    def main(self, p):
        return p.commands

    @_('commands command')
    def commands(self, p):
        return p.commands + [p.command]

    @_('command')
    def commands(self, p):
        return [p.command]

    @_('identifier ASSIGN expression ";"')
    def command(self, p):
        return ('ASSIGN', p.identifier, p.expression)

    @_('IF condition THEN commands ELSE commands ENDIF')
    def command(self, p):
        return ("IF ELSE", p.condition, p.commands0, p.commands1)

    @_('IF condition THEN commands ENDIF')
    def command(self, p):
        return ("IF", p.condition, p.commands)
    
    @_('WHILE condition DO commands ENDWHILE')
    def command(self, p):
        return ("WHILE", p.condition, p.commands)

    @_('REPEAT commands UNTIL condition ";"')
    def command(self, p):
        return ("REPEAT", p.condition, p.commands)

    @_('proc_call ";"')
    def command(self, p):
        return ("CALL",p.proc_call)

    @_('READ identifier ";"')
    def command(self, p):
        return ('READ', p.identifier)

    @_('WRITE value ";"')
    def command(self, p):
        return ("WRITE", p.value)

    @_('PIDENTIFIER "(" args_decl ")"')
    def proc_head(self, p):
        return (p.PIDENTIFIER, p.args_decl)

    @_('PIDENTIFIER "(" args ")"')
    def proc_call(self, p):
        return (p.PIDENTIFIER, p.args)

    @_('declarations "," PIDENTIFIER')
    def declarations(self, p):
        return p.declarations + [("VAR",p.PIDENTIFIER)]

    @_('declarations "," PIDENTIFIER "[" NUM "]"')
    def declarations(self, p):
        return p.declarations + [("ARR_NUM", p.PIDENTIFIER, p.NUM)]

    @_('PIDENTIFIER')
    def declarations(self, p):
        return [("VAR", p.PIDENTIFIER)]

    @_('PIDENTIFIER "[" NUM "]"')
    def declarations(self, p):
        return [("ARR_NUM", p.PIDENTIFIER, p.NUM)]

    @_('args_decl "," PIDENTIFIER')
    def args_decl(self, p):
        return p.args_decl + [("VAR",p.PIDENTIFIER)] 

    @_('args_decl "," "T" PIDENTIFIER')
    def args_decl(self, p):
        return p.args_decl + [("ARR",p.PIDENTIFIER)]

    @_('PIDENTIFIER')
    def args_decl(self, p):
        return [("VAR",p.PIDENTIFIER)] 

    @_('"T" PIDENTIFIER')
    def args_decl(self, p):
        return [("ARR", p.PIDENTIFIER)]
    
    @_('args "," PIDENTIFIER')
    def args(self, p):
        return p.args + [p.PIDENTIFIER] 

    @_('PIDENTIFIER')
    def args(self, p):
        return [p.PIDENTIFIER] 

    @_('value')
    def expression(self, p):
        return p.value;
    
    @_('value "+" value')
    def expression(self, p):
        return ('PLUS', p.value0, p.value1)

    @_('value "-" value')
    def expression(self, p):
        return ('MINUS', p.value0, p.value1)

    @_('value "*" value')
    def expression(self, p):
        return ('MUL', p.value0, p.value1)

    @_('value "/" value')
    def expression(self, p):
        return ('DIV', p.value0, p.value1)

    @_('value "%" value')
    def expression(self, p):
        return ('MOD', p.value0, p.value1)

    @_('value EQ value')
    def condition(self, p):
        return ("EQ", p.value0, p.value1)

    @_('value NEQ value')
    def condition(self, p):
        return ("NEQ", p.value0, p.value1)

    @_('value GT value')
    def condition(self, p):
        return ("GT", p.value0, p.value1)

    @_('value LT value')
    def condition(self, p):
        return ("LT", p.value0, p.value1)

    @_('value GEQ value')
    def condition(self, p):
        return ("GEQ", p.value0, p.value1)

    @_('value LEQ value')
    def condition(self, p):
        return ("LEQ", p.value0, p.value1)

    @_('NUM')
    def value(self, p):
        return ("NUM", int(p.NUM))

    @_('identifier')
    def value(self, p):
        return p.identifier

    @_('PIDENTIFIER')
    def identifier(self, p):
        return ("VAR", p.PIDENTIFIER)

    @_('PIDENTIFIER "[" NUM "]"')
    def identifier(self, p):
        return ("ARR_NUM", p.PIDENTIFIER, p.NUM)

    @_('PIDENTIFIER "[" PIDENTIFIER "]"')
    def identifier(self, p):
        return ("ARR_PID", p.PIDENTIFIER0, p.PIDENTIFIER1)

program = Program()
lexer = Lexer()
parser = Parser(program)
code_gen = CodeGen(program)

source_code = argv[1]
target_code = argv[2]
debug = False
if(len(argv)>3 and argv[3]=="-debug"):
    debug = True

with open(source_code, 'r') as f:
    code = f.read()
    parser.parse(lexer.tokenize(code))

try:
    code_gen.gen()
except RuntimeError as e:
    print(e)
    exit(1)
    

with open(target_code, 'w') as f:
    instructions = code_gen.instructions
    for i in instructions:
        f.write(f"{i}\n")

if(debug):
    print(program)