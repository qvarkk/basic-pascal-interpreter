from enum import StrEnum
from typing import Optional
from tokens import Token


class ErrorCode(StrEnum):
    UNEXPECTED_CHARACTER = 'Unexpected character'
    UNEXPECTED_TOKEN = 'Unexpected token'
    UNEXPECTED_ERROR = 'Unexpected error'
    ID_NOT_FOUND = 'Invalid identifier'
    DUPLICATE_ID = 'Duplicate identifier'


class Error(Exception):
    def __init__(self, message: Optional[str] = '') -> None:
        super().__init__(message)


class LexerError(Error):
    def __init__(self, error_code: ErrorCode, message: Optional[str] = '') -> None:
        self.message = f'{error_code.value} {message}'
        super().__init__(self.message)


class ParserError(Error):
    def __init__(self, error_code: ErrorCode, token: Token, message: Optional[str] = '') -> None:
        self.message = f'{error_code.value} {token} at line {token.line_number} {message}'
        super().__init__(self.message)


class SemanticError(Error):
    def __init__(self, error_code: ErrorCode, variable_token: Optional[Token] = None, message: Optional[str] = '') -> None:
        if variable_token is None:
            self.message = f'{error_code.value} {message}'
        else:
            self.message = f'{error_code.value} {variable_token.value} at line {variable_token.line_number} {message}'

        super().__init__(self.message)
