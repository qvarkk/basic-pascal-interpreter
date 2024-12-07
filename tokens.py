from enum import StrEnum


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
    def __init__(self, type: TokenType, value: str | int | float | None) -> None:
        self.type: TokenType = type
        self.value: str | int | float | None = value

    def __str__(self) -> str:
        return f'Token({self.type}: {self.value})'

    def __repr__(self) -> str:
        return self.__str__()
