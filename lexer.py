from tokens import *


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
