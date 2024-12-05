""" Pascal interpreter """
from collections import OrderedDict

########################################################
#                                                      #
#  LEXER                                               #
#                                                      #
########################################################

# OPERATORS DEFINITIONS
PLUS, MINUS, MUL, FLOAT_DIV, LPAREN, RPAREN, ASSIGN = (
    'PLUS', 'MINUS', 'MUL', 'FLOAT_DIV', 'LPAREN', 'RPAREN', 'ASSIGN'
)

# CONSTANTS DEFINITIONS
INTEGER_CONST, REAL_CONST, ID = (
    'INTEGER_CONST', 'REAL_CONST', 'ID'
)

# RESERVED KEYWORDS DEFINITIONS
PROGRAM, VAR, BEGIN, END, DIV, PROCEDURE = (
    'program', 'var', 'begin', 'end', 'div', 'procedure'
)

# TYPES RESERVED KEYWORDS DEFINITIONS
INTEGER, REAL = (
    'integer', 'real'
)

# META SYMBOLS DEFINITIONS
DOT, SEMI, COLON, COMMA, EOF = (
   'DOT', 'SEMI', 'COLON', 'COMMA', 'EOF'
)


class Token(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        return f'Token({self.type}: {self.value})'

    def __repr__(self):
        return self.__str__()


class Lexer(object):
    def __init__(self, text):
        self.pos = 0
        self.text = text
        self.current_character = text[self.pos]

    def error(self):
        raise Exception('Invalid character')

    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos <= len(self.text) - 1:
            return self.text[peek_pos]
        else:
            return None

    def advance(self):
        if self.pos < len(self.text) - 1:
            self.pos += 1
            self.current_character = self.text[self.pos]
        else:
            self.current_character = None

    def skip_whitespace(self):
        while self.current_character is not None and self.current_character.isspace():
            self.advance()

    def skip_comment(self):
        while self.current_character is not None and self.current_character != '}':
            self.advance()
        self.advance()

    RESERVED_KEYWORDS = {
        PROGRAM: Token(PROGRAM, 'PROGRAM'),
        VAR: Token(VAR, 'VAR'),
        PROCEDURE: Token(PROCEDURE, 'PROCEDURE'),
        BEGIN: Token(BEGIN, 'BEGIN'),
        END: Token(END, 'END'),
        DIV: Token(DIV, 'DIV'),
        INTEGER: Token(INTEGER, 'INTEGER'),
        REAL: Token(REAL, 'REAL'),
    }

    def id(self):
        result = ''

        while self.current_character is not None and self.current_character.isalnum():
            result += self.current_character
            self.advance()

        # remove case sensitivity
        result = result.lower()

        return self.RESERVED_KEYWORDS.get(result, Token(ID, result))

    def number(self):
        result = ''
        while self.current_character is not None and self.current_character.isdigit():
            result += self.current_character
            self.advance()

        if self.current_character == '.':
            result += self.current_character
            self.advance()

            while self.current_character is not None and self.current_character.isdigit():
                result += self.current_character
                self.advance()

            token = Token(REAL_CONST, float(result))
        else:
            token = Token(INTEGER_CONST, int(result))

        return token

    def get_next_token(self):
        while self.current_character is not None:
            if self.current_character.isspace():
                self.skip_whitespace()
                continue

            if self.current_character == '{':
                self.advance()
                self.skip_comment()
                continue

            if self.current_character.isalpha() or self.current_character == '_':
                return self.id()

            if self.current_character == ':' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(ASSIGN, ':=')

            if self.current_character == ':':
                self.advance()
                return Token(COLON, ':')

            if self.current_character == ';':
                self.advance()
                return Token(SEMI, ';')

            if self.current_character == '.':
                self.advance()
                return Token(DOT, '.')

            if self.current_character == ',':
                self.advance()
                return Token(COMMA, ',')

            if self.current_character.isdigit():
                return self.number()

            if self.current_character == '+':
                self.advance()
                return Token(PLUS, '+')

            if self.current_character == '-':
                self.advance()
                return Token(MINUS, '-')

            if self.current_character == '*':
                self.advance()
                return Token(MUL, '*')

            if self.current_character == '/':
                self.advance()
                return Token(FLOAT_DIV, '/')

            if self.current_character == '(':
                self.advance()
                return Token(LPAREN, '(')

            if self.current_character == ')':
                self.advance()
                return Token(RPAREN, ')')

            self.error()

        return Token(EOF, None)


########################################################
#                                                      #
#  PARSER                                              #
#                                                      #
########################################################


class AST(object):
    pass


class Program(AST):
    def __init__(self, name, block_node):
        self.name = name
        self.block = block_node


class Block(AST):
    def __init__(self, declarations, compound_statement):
        self.declarations = declarations
        self.compound_statement = compound_statement


class VarDecl(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


class ProcedureDecl(AST):
    def __init__(self, name, parameters, block_node):
        self.name = name
        self.parameters = parameters
        self.block_node = block_node


class ParameterDecl(AST):
    def __init__(self, var_node, type_node):
        self.parameter = var_node
        self.type_node = type_node


class Type(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class Compound(AST):
    def __init__(self):
        self.children = []


class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(AST):
    def __init__(self, token):
        self.token = token
        self.name = token.value


class NoOp(AST):
    pass


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

    def __str__(self):
        return f'BinOp(left: {self.left}, op: {self.op}, right: {self.right})'


class UnaryOp(AST):
    def __init__(self, op, factor):
        self.token = self.op = op
        self.factor = factor


class Number(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

    def __str__(self):
        return f'Number(value: {self.value})'


class Parser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def error(self):
        raise Exception('Invalid syntax')

    def eat(self, type):
        if self.current_token.type == type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def factor(self):
        token = self.current_token

        if token.type == PLUS:
            self.eat(PLUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == MINUS:
            self.eat(MINUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == INTEGER_CONST:
            self.eat(INTEGER_CONST)
            return Number(token)
        elif token.type == REAL_CONST:
            self.eat(REAL_CONST)
            return Number(token)
        elif token.type == LPAREN:
            self.eat(LPAREN)
            node = self.expr()
            self.eat(RPAREN)
            return node
        else:
            node = self.variable()
            return node

    def term(self):
        node = self.factor()

        while self.current_token.type in (MUL, DIV, FLOAT_DIV):
            token = self.current_token
            if self.current_token.type == MUL:
                self.eat(MUL)
            elif self.current_token.type == DIV:
                self.eat(DIV)
            elif self.current_token.type == FLOAT_DIV:
                self.eat(FLOAT_DIV)

            node = BinOp(left=node, op=token, right=self.factor())

        return node

    def expr(self):
        node = self.term()

        while self.current_token.type in (PLUS, MINUS):
            token = self.current_token
            if self.current_token.type == PLUS:
                self.eat(PLUS)
            elif self.current_token.type == MINUS:
                self.eat(MINUS)

            node = BinOp(left=node, op=token, right=self.term())

        return node

    def program(self):
        self.eat(PROGRAM)
        var_node = self.variable()
        program_name = var_node.name
        self.eat(SEMI)
        block_node = self.block()
        program_node = Program(program_name, block_node)
        self.eat(DOT)
        return program_node

    def block(self):
        declarations_node = self.declarations()
        comp_stat_node = self.compound_statement()
        block_node = Block(declarations_node, comp_stat_node)
        return block_node

    def declarations(self):
        declarations = []
        procedures = []

        if self.current_token.type == VAR:
            self.eat(VAR)

            while self.current_token.type == ID:
                var_decl_node = self.variable_declaration()
                declarations.extend(var_decl_node)
                self.eat(SEMI)

        while self.current_token.type == PROCEDURE:
            self.eat(PROCEDURE)
            procedure_name = self.current_token.value
            self.eat(ID)

            parameters = None
            if self.current_token.type == LPAREN:
                parameters = self.formal_parameters_list()

            self.eat(SEMI)
            block_node = self.block()
            procedures.append(ProcedureDecl(procedure_name, parameters, block_node))
            self.eat(SEMI)

        declarations.extend(procedures)
        return declarations

    def formal_parameters_list(self):
        self.eat(LPAREN)
        parameters = self.formal_parameters()
        self.eat(RPAREN)
        return parameters

    def formal_parameters(self):
        parameters = []

        while self.current_token.type != RPAREN:
            parameters.extend(self.parameter_declaration())
            if self.current_token.type != RPAREN:
                self.eat(SEMI)

        return parameters

    def parameter_declaration(self):
        var_nodes = [Var(self.current_token)]
        self.eat(ID)

        while self.current_token.type == COMMA:
            self.eat(COMMA)
            var_nodes.append(Var(self.current_token))
            self.eat(ID)

        self.eat(COLON)
        type_node = self.type_spec()
        parameters = [ParameterDecl(var_node, type_node) for var_node in var_nodes]
        return parameters

    def variable_declaration(self):
        var_nodes = [Var(self.current_token)]
        self.eat(ID)

        while self.current_token.type == COMMA:
            self.eat(COMMA)
            var_nodes.append(Var(self.current_token))
            self.eat(ID)

        self.eat(COLON)
        type_node = self.type_spec()
        var_declarations = [VarDecl(var_node, type_node) for var_node in var_nodes]
        return var_declarations

    def type_spec(self):
        token = self.current_token

        if self.current_token.type == INTEGER:
            self.eat(INTEGER)
        else:
            self.eat(REAL)

        node = Type(token)
        return node

    def compound_statement(self):
        self.eat(BEGIN)
        nodes = self.statement_list()
        self.eat(END)

        root = Compound()
        for node in nodes:
            root.children.append(node)

        return root

    def statement_list(self):
        node = self.statement()

        results = [node]

        while self.current_token.type is SEMI:
            self.eat(SEMI)
            results.append(self.statement())

        return results

    def statement(self):
        if self.current_token.type is BEGIN:
            return self.compound_statement()
        elif self.current_token.type is ID:
            return self.assignment_statement()
        else:
            return self.empty()

    def assignment_statement(self):
        left = self.variable()
        token = self.current_token
        self.eat(ASSIGN)
        right = self.expr()
        return Assign(left, token, right)

    def empty(self):
        return NoOp()

    def variable(self):
        token = self.current_token
        self.eat(ID)
        return Var(token)

    def parse(self):
        """
                                GRAMMAR

            program               : PROGRAM variable SEMI block DOT

            block                 : declarations compound_statement

            declarations          : (VAR (variable_declaration SEMI)+)* |
                                    (PROCEDURE variable formal_parameter_list SEMI block DOT)* |
                                    PROCEDURE variable SEMI block DOT |
                                    empty

            formal_parameter_list : LPAREN formal_parameters RPAREN

            formal_parameters     :  variable_declaration SEMI formal_parameters |
                                     variable_declaration

            parameter_declaration : variable (COMMA variable)* COLON type_spec

            variable_declaration  : variable (COMMA variable)* COLON type_spec

            type_spec             : INTEGER |
                                    REAL

            compound_statement    : BEGIN statement_list END

            statement_list        : statement |
                                    statement SEMI statement_list

            statement             : compound_statement |
                                    assignment_statement |
                                    empty

            assign_statement      : variable ASSIGN expr

            empty                 : λ

            expr                  : term ((PLUS | MINUS) term)*

            term                  : factor ((MUL | FLOAT_DIV | DIV) factor)*

            factor                : PLUS factor |
                                    MINUS factor |
                                    INTEGER_CONST |
                                    REAL_CONST |
                                    LPAREN expr RPAREN |
                                    variable

            variable              : ID
        """
        node = self.program()
        if self.current_token.type is not EOF:
            self.error()

        return node


########################################################
#                                                      #
#  SYMBOL TABLE                                        #
#                                                      #
########################################################


class Symbol(object):
    def __init__(self, name, type=None):
        self.name = name
        self.type = type


class BuiltinTypeSymbol(Symbol):
    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return f'<BuiltInTypeSymbol(name={self.name})>'


class VarSymbol(Symbol):
    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return f'<VarSymbol(name={self.name}, type={self.type})>'

    __repr__ = __str__


class ScopedSymbolTable(object):
    def __init__(self, scope_name, scope_level):
        self._symbols = OrderedDict()
        self.scope_name = scope_name
        self.scope_level = scope_level
        self._init_builtins()

    def _init_builtins(self):
        self.define(BuiltinTypeSymbol('INTEGER'))
        self.define(BuiltinTypeSymbol('REAL'))

    def __str__(self):
        lines = []
        scope_header = '\nSCOPE (ScopedSymbolTable):'
        lines.append(scope_header)
        lines.append('=' * len(scope_header))

        scope_name_info = f'Name: {self.scope_name}'
        scope_level_info = f'Level: {self.scope_level}'
        lines.append(scope_name_info)
        lines.append(scope_level_info)

        scope_symbols_header = 'Scope contents:'
        lines.append(scope_symbols_header)
        lines.append('-' * len(scope_symbols_header))

        for key, value in self._symbols.items():
            lines.append('%-14s: %s' % (key, value))

        return '\n'.join(lines)

    __repr__ = __str__

    def define(self, symbol):
        self._symbols[symbol.name] = symbol

    def lookup(self, name):
        return self._symbols.get(name)


class NodeVisitor(object):
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception(f'No visit_{type(node).__name__} method')


class SemanticAnalyzer(NodeVisitor):
    def __init__(self):
        self.symtab = ScopedSymbolTable('global', 1)

    def visit_Program(self, node):
        self.visit(node.block)

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.symtab.lookup(type_name)
        var_name = node.var_node.name

        if self.symtab.lookup(var_name) is not None:
            raise Exception(f'Duplicate identifier {var_name} found')

        self.symtab.define(VarSymbol(var_name, type_symbol))

    def visit_ProcDecl(self, node):
        pass

    def visit_ProcedureDecl(self, node):
        pass

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node):
        var = self.symtab.lookup(node.left.name)
        if var is None:
            raise NameError(f'Variable {node.left.name} not found')

        self.visit(node.right)

    def visit_Var(self, node):
        var = self.symtab.lookup(node.name)

        if var is None:
            raise NameError(f'Variable {node.name} not found')

    def visit_NoOp(self, node):
        pass

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_Number(self, node):
        pass

    def visit_UnaryOp(self, node):
        self.visit(node.factor)


########################################################
#                                                      #
#  INTERPRETER                                         #
#                                                      #
########################################################


class Interpreter(NodeVisitor):
    def __init__(self, parser):
        self.parser = parser
        self.GLOBAL_SCOPE = {}

    def visit_Program(self, node):
        self.visit(node.block_node)

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        pass

    def visit_ProcDecl(self, node):
        pass

    def visit_Type(self, node):
        pass

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node):
        var_name = node.left.name
        self.GLOBAL_SCOPE[var_name] = self.visit(node.right)

    def visit_Var(self, node):
        var_name = node.name
        value = self.GLOBAL_SCOPE.get(var_name)
        if value is None:
            raise NameError(str(var_name))
        else:
            return value

    def visit_NoOp(self, node):
        pass

    def visit_BinOp(self, node):
        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == DIV:
            return self.visit(node.left) // self.visit(node.right)
        elif node.op.type == FLOAT_DIV:
            return float(self.visit(node.left) / self.visit(node.right))

    def visit_Number(self, node):
        return node.value

    def visit_UnaryOp(self, node):
        if node.op.type == PLUS:
            return +self.visit(node.factor)
        elif node.op.type == MINUS:
            return -self.visit(node.factor)

    def interpret(self):
        tree = self.parser.parse()
        return self.visit(tree)


def main():
    text = """
    PROGRAM Test;
    VAR
       number     : INTEGER;
       a, b, c, x : INTEGER;
       y          : REAL;
    PROCEDURE proc1(z : INTEGER);
    VAR
       x, y : REAL;
    BEGIN
       x := y;
    END;
    BEGIN
       BEGIN
          number := 2;
          a := number;
          b := 10 * a + 10 * number DIV 4;
          c := a - - b
       END;
       x := 11;
       y := 20 / 7 + 3.14;
    END.
    """

    lexer = Lexer(text)
    parser = Parser(lexer)
    prog = parser.parse()
    symtab = SemanticAnalyzer()
    symtab.visit(prog)
    print(symtab.symtab)


if __name__ == '__main__':
    main()

