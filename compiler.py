from sly import Lexer as sly_Lexer
from sly import Parser as sly_Parser
from sys import argv

def get_var_from_decl(decl_var, program_cells_taken):
    if(decl_var == None): return []
    l = []
    cells_taken = 0
    for var in decl_var:
        type = var[0]
        var_name = var[1]
        var_obj = None

        if(type == "ARR_NUM"):
            size = int(var[2])
            var_obj = Identifier(type,var_name,size,program_cells_taken+cells_taken)
            cells_taken += size
        else:
            var_obj = Identifier(type,var_name)
        
        l.append(var_obj)

    return l,cells_taken

class Identifier():
    def __init__(self, type, name, size=None, start_cell=None):
        self.type = type
        self.name = name
        self.size = size
        self.mem_cell = None
        if(size != None):
            self.start_cell = start_cell
            self.mem_cell = [None]*size

    def __repr__(self):
        str = f"{self.type},{self.name}"
        if(self.size != None):
            str += f',size:{self.size},start:{self.start_cell},table:\n{self.mem_cell}'
        else:
            str += f',mem_cell:{self.mem_cell}'
        return f"({str})"

class Procedure():
    def __init__(self, name, args, commands, decl_var=None):
        self.name = name
        self.decl_var = get_var_from_decl(decl_var)
        self.commands = commands
        self.args = []
        for arg in args:
            type = arg[0]
            arg_name = arg[1]
            self.args.append(Identifier(type,arg_name))

    def set_args(self, args):
        self.args = args

    def __repr__(self):
        return f"name:{self.name} \n args:{self.args} \n decl_var:{self.decl_var} \n commands:{self.commands}\n\n"

class Program():

    def __init__(self):
        self.procedures = []
        self.main_decl = []
        self.main_commands = []
        self.mem_cells_taken = 0

    def set_declarations(self, decl_var):
        self.main_decl, cells_taken = get_var_from_decl(decl_var,self.mem_cells_taken)
        self.mem_cells_taken += cells_taken

    def set_commands(self, commands):
        self.main_commands = commands

    def set_procedures(self, procedures):
        for pr in procedures:
            if(len(pr)==3):
                self.procedures.append( Procedure(pr[0][0],pr[0][1],pr[2],pr[1]) )
            else:
                self.procedures.append( Procedure(pr[0][0],pr[0][1],pr[1]) )

    def error_check(self, scope):
        pass

    # a := 5     sprwadzic czy "a" zainicjowane
    # b[3] := 5  sprawdzic czy "a[3]" zainicjowane
    # b[a] := 5  narazie zakladam ze "b" zainicjowane, sprwadzić "a[b]"
    def check_var_init(self, name, type, idx, scope="main"):
        var = None
        if(scope == "main"):
            var = self.find_var(name,type)
        else:
            pass
            # pr = self.find_procedure(scope)
            # var = self.find_var(name,type,pr)

        mem_cell = None
        if(type == "VAR"):
            mem_cell = var.mem_cell
        
        if(type == "ARR_NUM"):
            idx = int(idx)
            mem_cell = var.mem_cell[idx]

        if(type == "ARR_PID"):
            idx_cell = self.check_var_init(idx)
            mem_cell = var.mem_cell[idx_cell]
            # if(idx_cell == None):
            #     pass
            #     #TODO err | b[a], gdzie "a" nie jest zainicjowane

        return mem_cell, var
            
    def init_var(self, var: Identifier, idx=None):
        if(idx == None):
            var.mem_cell = self.mem_cells_taken
            self.mem_cells_taken += 1
        else:
            idx = int(idx)
            start = var.start_cell
            var.mem_cell[idx] = start+idx

    def find_var(self, name, type, scope="main"):
        var_list = None
        if(scope == "main"):
            var_list = self.main_decl
        else:
            pass
            #TODO: scope jakiejs procedury

        for var in var_list:
            if(var.name == name):
                if(var.type != type):
                    pass
                    #TODO: type mismatch
                return var
        return None

    def find_procedure(self, name):
        for pr in self.procedures:
            if(pr.name == name):
                return pr
        return None
        

    def __repr__(self):
        x = f"PROCEDURES:\n\n{self.procedures} \n\n DECL IN MAIN:{self.main_decl}\n\n"
        for c in self.main_commands:
            x += f"{c}\n"
        return f"{x}\n\n MEM CELLS TAKEN:{self.mem_cells_taken}"
    


class CodeGen():
    def __init__(self, program: Program):
        self.program = program

    def gen(self):
        main_commands = self.program.main_commands

        for command in main_commands:
            type = command[0]

            if(type == "ASSIGN"):
                self.gen_assign(command[1:])

            if(type == "WRITE"):
                self.gen_write_command(command[1:])
    
    def init_var(self):
        pass

    def gen_assign(self,command):
        exp = command[1]
        var = command[0]
        var_type = var[0]
        var_name = var[1]
        var_idx = None
        if(var_type != "VAR"):
            var_idx = var[2]

        mem_cell, var = self.program.check_var_init(var_name,var_type,var_idx)
        if(mem_cell == None):
            self.program.init_var(var,var_idx)

        if(len(exp) == 2):
            var2_type = exp[0]
            var2_name = exp[1]

            if(var2_type == "NUM"):

        else:
            pass
        


    def gen_write_command(self,command):
        pass

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
debug = False
if(len(argv)>2 and argv[2]=="-debug"):
    debug = True

with open(source_code, 'r') as f:
    code = f.read()
    parser.parse(lexer.tokenize(code))

code_gen.gen()

if(debug):
    print(program)