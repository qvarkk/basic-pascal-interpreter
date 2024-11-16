""" Pascal interpreter """

########################################################
#                                                      #
#  LEXER                                               #
#                                                      #
########################################################

INTEGER, PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, BEGIN, END, DOT, ID, ASSIGN, SEMI, EOF = (
    'INTEGER', 'PLUS', 'MINUS', 'MUL', 'div', 'LPAREN', 'RPAREN', 'begin', 'end', 'DOT', 'ID', 'ASSIGN', 'SEMI', 'EOF'
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

    RESERVED_KEYWORDS = {
        BEGIN: Token(BEGIN, 'BEGIN'),
        END: Token(END, 'END'),
        DIV: Token(DIV, 'DIV'),
    }

    def id(self):
        result = ''

        while self.current_character is not None and self.current_character.isalnum():
            result += self.current_character
            self.advance()

        # remove case sensitivity
        result = result.lower()

        return self.RESERVED_KEYWORDS.get(result, Token(ID, result))

    def integer(self):
        result = ''
        while self.current_character is not None and self.current_character.isdigit():
            result += self.current_character
            self.advance()
        return int(result)

    def get_next_token(self):
        while self.current_character is not None:
            if self.current_character.isspace():
                self.skip_whitespace()
                continue

            if self.current_character.isalpha() or self.current_character == '_':
                return self.id()

            if self.current_character == ':' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(ASSIGN, ':=')

            if self.current_character == ';':
                self.advance()
                return Token(SEMI, ';')

            if self.current_character == '.':
                self.advance()
                return Token(DOT, '.')

            if self.current_character.isdigit():
                return Token(INTEGER, self.integer())

            if self.current_character == '+':
                self.advance()
                return Token(PLUS, '+')

            if self.current_character == '-':
                self.advance()
                return Token(MINUS, '-')

            if self.current_character == '*':
                self.advance()
                return Token(MUL, '*')

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
        elif token.type == INTEGER:
            self.eat(INTEGER)
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

        while self.current_token.type in (MUL, DIV):
            token = self.current_token
            if self.current_token.type == MUL:
                self.eat(MUL)
            elif self.current_token.type == DIV:
                self.eat(DIV)

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
        node = self.compound_statement()
        self.eat(DOT)
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

            program            : compound_statement DOT

            compound_statement : BEGIN statement_list END

            statement_list     : statement |
                                 statement SEMI statement_list

            statement          : compound_statement |
                                 assign_statement |
                                 empty

            assign_statement   : variable ASSIGN expr

            empty              :

            expr               : term ((PLUS | MINUS) term)*

            term               : factor ((MUL | DIV) factor)*

            factor             : PLUS factor |
                                 MINUS factor |
                                 INTEGER |
                                 LPAREN expr RPAREN |
                                 variable

            variable           : ID
        """
        node = self.program()
        if self.current_token.type is not EOF:
            self.error()

        return node


########################################################
#                                                      #
#  INTERPRETER                                         #
#                                                      #
########################################################


class NodeVisitor(object):
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception(f'No visit_{type(node).__name__} method')


class Interpreter(NodeVisitor):
    def __init__(self, parser):
        self.parser = parser
        self.GLOBAL_SCOPE = {}

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
            return self.visit(node.left) / self.visit(node.right)

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
    BEGIN
        BEGIN
            number := 2;
            a := NumBer;
            b := 10 * a + 10 * NUMBER diV 4;
            c := a - - b;
        end;
        x := 11;
    END.
    """

    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    interpreter.interpret()
    print(interpreter.GLOBAL_SCOPE)


if __name__ == '__main__':
    main()

