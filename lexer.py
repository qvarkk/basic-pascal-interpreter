from errors import LexerError, ErrorCode
from tokens import *


class Lexer(object):
    """ Breaks given pascal program into a series of tokens.

        :parameter text: pascal program in string representation.
    """
    def __init__(self, text: str) -> None:
        self.pos: int = 0
        self.text: str = text
        self.line_number: int = 1
        self.current_character: str | None = text[self.pos]
        self._init_reserved_keywords()

    def _init_reserved_keywords(self) -> None:
        token_types_list = list(TokenType)
        start_index: int = token_types_list.index(TokenType.PROGRAM)
        end_index: int = token_types_list.index(TokenType.END)
        self.RESERVED_KEYWORDS = {
            token_type: Token(token_type)
            for token_type in token_types_list[start_index:end_index + 1]
        }

    def error(self) -> None:
        """ Raises SyntaxError when called
            :raises SyntaxError:
        """
        message: str = f'\'{self.current_character}\' at line {self.line_number}'
        raise LexerError(error_code=ErrorCode.UNEXPECTED_CHARACTER, message=message)

    def peek(self) -> str | None:
        """ Peeks at the next character if possible.
            :returns: Peeked character if there is one, None otherwise.
        """
        peek_pos: int = self.pos + 1
        if peek_pos <= len(self.text) - 1:
            return self.text[peek_pos]
        else:
            return None

    def step(self) -> None:
        """ Updates the current character to the next character or to None if reached end of the program string.
        """
        if self.pos < len(self.text) - 1:
            self.pos += 1
            self.current_character = self.text[self.pos]
            if self.current_character == '\n':
                self.line_number += 1
        else:
            self.current_character = None

    def skip_whitespace(self) -> None:
        """ Steps through the program string until non-whitespace is encountered.
        """
        while self.current_character is not None and self.current_character.isspace():
            self.step()

    def skip_comment(self) -> None:
        """ Steps through the program string until the end of a comment is encountered.
        """
        while self.current_character is not None and self.current_character != '}':
            self.step()
        self.step()

    def id(self) -> Token:
        """ Reads identifiers.

            :returns: ID Token if it's not a reserved keyword, otherwise the reserved keyword token.
        """
        result: str = ''

        while self.current_character is not None and self.current_character.isalnum():
            result += self.current_character
            self.step()

        # remove case sensitivity
        upper_result: str = result.upper()
        result_token: Token = self.RESERVED_KEYWORDS.get(upper_result, Token(TokenType.ID, result, line_number=self.line_number))

        return result_token

    def number(self) -> Token:
        """ Reads numbers.

            :returns: INTEGER_CONST Token if number is integer, otherwise REAL_CONST Token if number is float.
        """
        result: str = ''
        while self.current_character is not None and self.current_character.isdigit():
            result += self.current_character
            self.step()

        if self.current_character == '.':
            result += self.current_character
            self.step()

            while self.current_character is not None and self.current_character.isdigit():
                result += self.current_character
                self.step()

            token: Token = Token(TokenType.REAL_CONST, float(result), line_number=self.line_number)
        else:
            token: Token = Token(TokenType.INTEGER_CONST, int(result), line_number=self.line_number)

        return token

    def get_next_token(self) -> Token:
        """ Steps through the program string until the next token is encountered.

            :returns: Next token encountered in the program string.
            :raises SyntaxError: If encountered not recognizable character
        """
        while self.current_character is not None:
            if self.current_character.isspace():
                self.skip_whitespace()
                continue

            if self.current_character == '{':
                self.step()
                self.skip_comment()
                continue

            if self.current_character.isdigit():
                return self.number()

            if self.current_character.isalpha() or self.current_character == '_':
                return self.id()

            if self.current_character == ':' and self.peek() == '=':
                self.step()
                self.step()
                return Token(TokenType.ASSIGN, line_number=self.line_number)

            one_character_tokens = (':', ';', '.', ',', '+', '-', '*', '/', '(', ')')

            if self.current_character in one_character_tokens:
                current_character: str = self.current_character
                self.step()
                return Token(TokenType(current_character), line_number=self.line_number)

            self.error()

        return Token(TokenType.EOF, None)
