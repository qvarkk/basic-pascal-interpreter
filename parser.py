from ast_nodes import *
from tokens import *


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

        while self.current_token.type == VAR:
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
