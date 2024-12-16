from enum import StrEnum
from typing import TypeAlias


TokenValue: TypeAlias = str | int | float | None


class TokenType(StrEnum):
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
    """ The smallest lexeme that lexer can recognize.

        :parameter type: Type of the token
        :parameter value: Value of the token
    """
    def __init__(self, type: TokenType, value: TokenValue) -> None:
        self.type: TokenType = type
        self.value: TokenValue = value

    def __str__(self) -> str:
        return f'Token({self.type}: {self.value})'

    def __repr__(self) -> str:
        return self.__str__()
