from enum import StrEnum
from typing import TypeAlias, Optional

TokenValue: TypeAlias = str | int | float | None


class TokenType(StrEnum):
    # single-character types
    PLUS            = '+'
    MINUS           = '-'
    MUL             = '*'
    FLOAT_DIV       = '/'
    LPAREN          = '('
    RPAREN          = ')'
    DOT             = '.'
    SEMI            = ';'
    COLON           = ':'
    COMMA           = ','
    # reserved keywords
    PROGRAM         = 'PROGRAM'     # has to be first in order for Lexer._init_reserved_keywords to work
    VAR             = 'VAR'
    INTEGER         = 'INTEGER'
    REAL            = 'REAL'
    DIV             = 'DIV'
    PROCEDURE       = 'PROCEDURE'
    BEGIN           = 'BEGIN'
    END             = 'END'         # has to be last for the same reason
    # miscellaneous
    ID              = 'ID'
    INTEGER_CONST   = 'INTEGER_CONST'
    REAL_CONST      = 'REAL_CONST'
    ASSIGN          = ':='
    EOF             = 'EOF'


class Token(object):
    """ The smallest lexeme that lexer can recognize.

        :parameter type: Type of the token
        :parameter value: Value of the token
    """
    def __init__(self, type: TokenType, value: Optional[TokenValue] = None, line_number: int = 0) -> None:
        self.type: TokenType = type

        if value is None:
            self.value: TokenValue = type
        else:
            self.value: TokenValue = value

        self.line_number: int = line_number

    def __str__(self) -> str:
        return f'<Token(type={self.type.name}, value=\'{self.value}\'>'

    def __repr__(self) -> str:
        return self.__str__()
