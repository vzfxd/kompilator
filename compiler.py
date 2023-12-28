import utils
from sys import exit
from sly import Lexer as sly_Lexer
from sly import Parser as sly_Parser
from sys import argv

class Identifier():
    def __init__(self, name):
        self.name = name

    def check_type(var1, var2):
        return var1.__class__.__name__ == var2.__class__.__name__
    
    def declare_variables(var_list, program_memory):
        if(var_list == None): return [],0
        l = []
        memory_taken = 0
        for var in var_list:
            type, name, idx = Identifier.get_var(var)

            if(type == "ARR_NUM"):
                size = int(idx)
                var_obj = Array(name,size,program_memory+memory_taken)
                memory_taken += size
            if(type == "VAR"):
                var_obj = Variable(name)
            
            l.append(var_obj)

        return l,memory_taken
    
    def get_var(raw):
        var_type, var_name, var_idx = raw[0], raw[1], None
        if(len(raw) == 3):
            var_idx = raw[2]
        return var_type, var_name, var_idx

class Array(Identifier):
    def __init__(self, name, size=None, start_addr=None):
        super().__init__(name)
        self.start_addr = start_addr
        self.size = size

    def __repr__(self):
        return f"\n name:{self.name}, start_addr:{self.start_addr}, size:{self.size} \n"
    
    def set_addr(self, addr):
        self.start_addr = addr
    
    def get_addr(self):
        return self.start_addr

class Variable(Identifier):
    def __init__(self, name=None):
        super().__init__(name)
        self.addr = None

    def set_addr(self, addr):
        self.addr = addr

    def get_addr(self):
        return self.addr

    def __repr__(self):
        return f"\n name:{self.name}, addr:{self.addr}\n"
    
class Procedure():
    def __init__(self, name, args, commands, decl):
        self.name = name
        self.commands = commands
        self.decl_raw = decl
        self.decl = []
        self.args = self.declare_args(args)

    def declare_args(self,args):
        l = []
        for arg in args:
            type,name = arg[0], arg[1]
            if(type == "ARR"):
                l.append(Array(name))
            if(type == "VAR"):
                l.append(Variable(name))
        return l

    def declare_variables(self, mem):
        self.decl,mem = Identifier.declare_variables(self.decl_raw,mem)
        
        for arg in self.args:
            for var in self.decl:
                if(arg.name == var.name):
                    raise RuntimeError(f"Declared {var.__class__.__name__} {var.name} has same name as {arg.__class__.__name__} {arg.name}")
        
        return mem

    def inject_args(self, args):
        self.decl += self.args
        for idx,arg in enumerate(self.args):
            arg.set_addr(args[idx].get_addr())

    def update_addr(self, args):
        for idx,arg in enumerate(self.args):
            args[idx].set_addr(arg.get_addr())


class Program():

    def __init__(self):
        self.procedures = []
        self.main_commands = []
        self.mem_cells_taken = 0
        self.scope = []
        self.curr_procedure = None
        self.curr_command = None

    def set_declarations(self, var_list):
        self.scope, cells_taken = Identifier.declare_variables(var_list,self.mem_cells_taken)
        self.mem_cells_taken += cells_taken

    def set_commands(self, commands):
        self.main_commands = commands

    def set_procedures(self, procedures):
        for p in procedures:
            head = p[0]
            name = head[0]
            args = head[1]
            commands = p[1]
            decl = None if len(p)<3 else p[2] 
            self.procedures.append(Procedure(name,args,commands,decl))

    def find_procedure(self,name,args):
        found = []
        
        for p in self.procedures:
            if(p.name == name):
                found.append(p)

        if(len(found) == 0):
            raise RuntimeError(f"Procedure {name} not declared")
        
        if(len(found)>1):
            raise RuntimeError(f"Procedure {name} declared more than once")
        
        p:Procedure = found[0]
        for idx,arg in enumerate(args):
            if(Identifier.check_type(arg,p.args[idx]) == False):
                raise RuntimeError(f"type mismatch between {arg.__class__.__name__} {arg.name} and {p.args[idx].__class__.__name__} {p.args[idx].name}")
            
        return p

    def find_var(self, name, type="VAR", type_check=True):
        if(type == "VAR"):
            type = Variable
        else:
            type = Array

        found = []
        for var in self.scope:
            if(var.name == name):
                found.append(var)
            
        if(len(found) > 1):
            raise RuntimeError(f"identifier {name} declared more than once")
        
        if(len(found) == 0):
            raise RuntimeError(f"{type.__name__} {name} not declared\n{self.curr_command}")
        
        var = found[0]
        if(type_check):
            if(not isinstance(var,type)):
                raise RuntimeError(f"wrong usage of {var.__class__.__name__} {name}")
        
        return var
    
    def init_var(self, var: Variable):
        var.set_addr(self.mem_cells_taken)
        self.mem_cells_taken += 1

    def __repr__(self):
        x = f"PROCEDURES:\n\n{self.procedures} \n\n DECL IN MAIN:\n{self.scope}\n\n"
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

    def put_to_reg(self, reg):
        self.instructions += [f"PUT {reg}"]

    def put_reg_to_accumulator(self, reg):
        self.instructions += [f"GET {reg}"]

    def add_reg_to_acc(self, reg):
        self.instructions += [f"ADD {reg}"]

    def reg_plus_reg(self, reg1, reg2):
        self.instructions += [f"GET {reg1}",f"ADD {reg2}"]

    def reg_minus_reg(self, reg1, reg2):
        self.instructions += [f"GET {reg1}", f"SUB {reg2}"]

    def set_jump(self, idx, ir):
        new_ir = self.instructions[idx][:-1] + str(ir)
        self.instructions[idx] = new_ir

    def save_var_addr_to_reg(self, var: Identifier, reg, idx, idx_type):
        if(reg == 'a'): raise RuntimeError("adrr saved to accumulator")
        
        if(isinstance(var,Variable)):
            if(var.addr == None):
                raise RuntimeError(f"Variable {var.name} not initialized\n")
            self.save_to_reg(var.addr, reg)
        else:
            if(idx_type == "ARR_NUM"):
                self.save_to_reg(int(idx),'a')
            else:
                idx_var = self.program.find_var(idx)
                if(idx_var.addr == None):
                    raise RuntimeError(f"Variable {idx_var.name} not initialized")
                self.save_to_reg(idx_var.addr,reg)
                self.load_from_memory(reg)

            self.save_to_reg(var.start_addr,reg)
            self.add_reg_to_acc(reg)
            self.put_to_reg(reg)

    def assign_var(self, val_raw, reg):
        val_type, val_name, val_idx = Identifier.get_var(val_raw)    
            
        if(val_type == "NUM"):
            self.save_to_reg(int(val_name),reg)
        else:
            val = self.program.find_var(val_name, val_type)
            self.save_var_addr_to_reg(val,reg,val_idx,val_type)
            self.load_from_memory(reg)
            self.put_to_reg(reg)

    def gen(self, commands):
        for command in commands:
            self.program.curr_command = command
            type = command[0]

            if(type == "ASSIGN"):
                self.gen_assign(command[1:])

            if(type == "WRITE"):
                self.gen_write(command[1:])

            if(type == "READ"):
                self.gen_read(command[1:])

            if(type == "IF" or type == "IF ELSE"):
                self.gen_if(command[1:], type)

            if(type == "WHILE"):
                self.gen_while(command[1:])

            if(type == "REPEAT"):
                self.gen_repeat(command[1:])

            if(type == "CALL"):
                self.gen_call(command[1:])

    def gen_call(self, command):
        p = command[0]
        name,args = p[0],p[1]
        arg_list = []

        if(self.program.curr_procedure == name):
            raise RuntimeError("Recursion prohibited")
        
        for arg in args:
            arg_list.append(self.program.find_var(arg,"dummy",False))
        
        p:Procedure = self.program.find_procedure(name,arg_list)
        mem = p.declare_variables(self.program.mem_cells_taken)
        self.program.mem_cells_taken += mem
        p.inject_args(arg_list)
        
        prev_scope = self.program.scope
        prev_proc = self.program.curr_procedure

        self.program.curr_procedure = name
        self.program.scope = p.decl
        self.gen(p.commands)
        p.update_addr(arg_list)
        self.program.scope = prev_scope
        self.program.curr_procedure = prev_proc

    def gen_condition(self, condition):
        type = condition[0]
        val1 = condition[1]
        val2 = condition[2]

        if(type == "GT"):
            type = "LT"
            return self.gen_condition((type,val2,val1))
        
        if(type == "GEQ"):
            type = "LEQ"
            return self.gen_condition((type,val2,val1))

        if(type == "LT"):
            self.gen_operation(val2,val1,"MINUS",'a')
            self.instructions += ["JZERO j"]

        if(type == "LEQ"):
            self.gen_operation(val1,val2,"MINUS",'a')
            self.instructions += ["JPOS j"]

        if(type in ["EQ","NEQ"]):
            self.gen_operation(val1,val2,"MINUS",'a')
            self.put_to_reg('h')
            self.gen_operation(val2,val1,"MINUS",'a')
            self.reg_plus_reg('a','h')
            if(type == "EQ"):
                self.instructions += ["JPOS j"]
            else:
                self.instructions += ["JZERO j"]

    def gen_while(self, command):
        while_head = command[0]
        while_body = command[1]

        cond_start = len(self.instructions)
        self.gen_condition(while_head)
        jump_idx = len(self.instructions)-1
        self.gen(while_body)
        self.instructions += [f"JUMP {cond_start}"]
        end_while = len(self.instructions)
        self.set_jump(jump_idx,end_while)

    def gen_repeat(self, command):
        repeat_head = command[0]
        repeat_body = command[1]

        start_repeat = len(self.instructions)
        self.gen(repeat_body)
        self.gen_condition(repeat_head)
        jump = len(self.instructions)-1
        self.set_jump(jump,start_repeat)
    
    def gen_if(self, command, if_type):
        if_head = command[0]
        if_body = command[1]
    
        self.gen_condition(if_head)
        if_jump = len(self.instructions)-1
        self.gen(if_body)
        end_if = len(self.instructions)

        if(if_type == "IF ELSE"):
            else_body = command[2]
            self.instructions.append("JUMP j")
            else_jump = len(self.instructions)-1
            self.gen(else_body)
            else_end = len(self.instructions)
            self.set_jump(else_jump,else_end)
            end_if += 1

        self.set_jump(if_jump,end_if)

    
    def gen_read(self, command):
        var_raw = command[0]
        type, name, idx = Identifier.get_var(var_raw)
        var = self.program.find_var(name, type)

        if(isinstance(var,Variable) and var.addr == None):
            self.program.init_var(var)

        self.save_var_addr_to_reg(var,'h',idx,type)
        self.instructions += [f"READ"]
        self.save_to_memory('h')
    
    def gen_operation(self, val1, val2, operation, reg):
        self.assign_var(val1,'g')
        self.assign_var(val2,'f')
        
        if(operation == "PLUS"):
            self.reg_plus_reg('g','f')

        if(operation == "MINUS"):
            self.reg_minus_reg('g','f')

        if(operation == "MUL"):
            self.instructions += [f"RST e",f"GET g"]
            while_jump = len(self.instructions)
            self.instructions += [ f"JZERO j",f"SHR a",f"SHL a",f"INC a",f"SUB g"]
            if_jump = len(self.instructions)
            self.instructions += [f"JPOS j",f"GET e",f"ADD f",f"PUT e",]
            if_end = len(self.instructions)
            self.instructions += [f"SHR g",f"SHL f",f"JUMP {while_jump-1}"]
            while_end = len(self.instructions)

            self.set_jump(while_jump,while_end)
            self.set_jump(if_jump,if_end)
            self.put_reg_to_accumulator('e')

        '''
        def divide_logarithmically(a, b):
            r = 0

            while a >= b:
                power = 1
                divisor = b

                while a >= divisor * 2:
                    divisor *= 2
                    power *= 2

            a -= divisor
            r += power(DIV) || r = a(MOD)

        return quotient
        '''
        if(operation == "DIV" or operation == "MOD"):
            self.instructions += ["RST e"]
            start_while = len(self.instructions)
            self.reg_minus_reg('f','g')
            while_jump = len(self.instructions)
            self.instructions += ["JPOS j"]
            self.save_to_reg(1,'b')
            self.instructions += ["GET f", "PUT c"]
            start_inner_while = len(self.instructions)
            self.instructions += ["SHL c"]
            self.reg_minus_reg('c','g')
            inner_while_jump = len(self.instructions)
            self.instructions += ["JPOS j"]
            self.instructions += ["SHL b", f"JUMP {start_inner_while}"]
            inner_while_end = len(self.instructions)
            self.instructions += ["SHR c"]
            self.reg_minus_reg('g','c')
            self.put_to_reg('g')

            if(operation == "DIV"):
                self.reg_plus_reg('e','b')
                self.put_to_reg('e')
            if(operation == "MOD"):
                self.instructions += ["GET g","PUT e"]

            self.instructions += [f"JUMP {start_while}"]
            end_while = len(self.instructions)

            self.set_jump(while_jump,end_while)
            self.set_jump(inner_while_jump, inner_while_end)
            self.put_reg_to_accumulator('e')
        
        if(reg != 'a'):
            self.put_to_reg(reg)
    
    def gen_assign(self,command):
        exp_raw = command[1]
        var_raw = command[0]
        type, name, idx = Identifier.get_var(var_raw)
        
        var = self.program.find_var(name, type)
        
        if(isinstance(var,Variable) and var.addr == None):
            self.program.init_var(var)

        self.save_var_addr_to_reg(var,'h',idx,type)

        if(exp_raw[0] not in ["ARR_NUM","ARR_PID","NUM","VAR"]):
            operation = exp_raw[0]
            val1 = exp_raw[1]
            val2 = exp_raw[2]
            self.gen_operation(val1,val2,operation,'a')
        else:
            self.assign_var(exp_raw,'g')
            self.put_reg_to_accumulator('g')

        self.save_to_memory('h')

    def gen_write(self,command):
        var_raw = command[0]
        type, name, idx = Identifier.get_var(var_raw)
        if(type == "NUM"):
            self.save_to_reg(int(name),'a')
        else:
            var = self.program.find_var(name, type)
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
        self.ctx.set_procedures(p.procedures)
        self.ctx.set_declarations(p.main[0])
        self.ctx.set_commands(p.main[1])          
    
    @_('procedures PROCEDURE proc_head IS declarations IN commands END')
    def procedures(self, p):
        return p.procedures + [(p.proc_head, p.commands, p.declarations)]

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
        return p.value
    
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
    code_gen.gen(program.main_commands)
    code_gen.instructions.append("HALT")
except RuntimeError as e:
    print(e)
    exit(1)
    

with open(target_code, 'w') as f:
    instructions = code_gen.instructions
    for i in instructions:
        f.write(f"{i}\n")

if(debug):
    print(program)