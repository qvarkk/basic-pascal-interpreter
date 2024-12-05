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
