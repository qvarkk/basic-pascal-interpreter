from tokens import *


class Lexer(object):
    """ Breaks given pascal program into a series of tokens.

        :parameter text: pascal program in string representation.
    """
    def __init__(self, text: str) -> None:
        self.pos: int = 0
        self.text: str = text
        self.current_character: str | None = text[self.pos]

    def error(self) -> None:
        """ Raises SyntaxError when called
            :raises SyntaxError:
        """
        raise SyntaxError('Invalid character')

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

    RESERVED_KEYWORDS: dict[str, Token] = {
        TokenType.PROGRAM: Token(TokenType.PROGRAM, 'PROGRAM'),
        TokenType.VAR: Token(TokenType.VAR, 'VAR'),
        TokenType.PROCEDURE: Token(TokenType.PROCEDURE, 'PROCEDURE'),
        TokenType.BEGIN: Token(TokenType.BEGIN, 'BEGIN'),
        TokenType.END: Token(TokenType.END, 'END'),
        TokenType.DIV: Token(TokenType.DIV, 'DIV'),
        TokenType.INTEGER: Token(TokenType.INTEGER, 'INTEGER'),
        TokenType.REAL: Token(TokenType.REAL, 'REAL'),
    }

    def id(self) -> Token:
        """ Reads identifiers.

            :returns: ID Token if it's not a reserved keyword, otherwise the reserved keyword token.
        """
        result: str = ''

        while self.current_character is not None and self.current_character.isalnum():
            result += self.current_character
            self.step()

        # remove case sensitivity
        result = result.lower()

        return self.RESERVED_KEYWORDS.get(result, Token(TokenType.ID, result))

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

            token: Token = Token(TokenType.REAL_CONST, float(result))
        else:
            token: Token = Token(TokenType.INTEGER_CONST, int(result))

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

            if self.current_character.isalpha() or self.current_character == '_':
                return self.id()

            if self.current_character == ':' and self.peek() == '=':
                self.step()
                self.step()
                return Token(TokenType.ASSIGN, ':=')

            if self.current_character == ':':
                self.step()
                return Token(TokenType.COLON, ':')

            if self.current_character == ';':
                self.step()
                return Token(TokenType.SEMI, ';')

            if self.current_character == '.':
                self.step()
                return Token(TokenType.DOT, '.')

            if self.current_character == ',':
                self.step()
                return Token(TokenType.COMMA, ',')

            if self.current_character.isdigit():
                return self.number()

            if self.current_character == '+':
                self.step()
                return Token(TokenType.PLUS, '+')

            if self.current_character == '-':
                self.step()
                return Token(TokenType.MINUS, '-')

            if self.current_character == '*':
                self.step()
                return Token(TokenType.MUL, '*')

            if self.current_character == '/':
                self.step()
                return Token(TokenType.FLOAT_DIV, '/')

            if self.current_character == '(':
                self.step()
                return Token(TokenType.LPAREN, '(')

            if self.current_character == ')':
                self.step()
                return Token(TokenType.RPAREN, ')')

            self.error()

        return Token(TokenType.EOF, None)
