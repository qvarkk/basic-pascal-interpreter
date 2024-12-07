from ast_nodes import *
from lexer import Lexer
from tokens import *


class Parser(object):
    def __init__(self, lexer: Lexer) -> None:
        self.lexer: Lexer = lexer
        self.current_token: Token = self.lexer.get_next_token()

    def error(self) -> None:
        raise Exception('Invalid syntax')

    def eat(self, type: TokenType) -> None:
        if self.current_token.type == type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def factor(self) -> UnaryOperationNode | BinaryOperationNode | VariableNode | NumberNode:
        token: Token = self.current_token

        if token.type == TokenType.PLUS:
            self.eat(TokenType.PLUS)
            node: UnaryOperationNode = UnaryOperationNode(token, self.factor())
        elif token.type == TokenType.MINUS:
            self.eat(TokenType.MINUS)
            node: UnaryOperationNode = UnaryOperationNode(token, self.factor())
        elif token.type == TokenType.INTEGER_CONST:
            self.eat(TokenType.INTEGER_CONST)
            node: NumberNode = NumberNode(token)
        elif token.type == TokenType.REAL_CONST:
            self.eat(TokenType.REAL_CONST)
            node: NumberNode = NumberNode(token)
        elif token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            node: UnaryOperationNode | BinaryOperationNode | VariableNode | NumberNode = self.expr()
            self.eat(TokenType.RPAREN)
        else:
            node: VariableNode = self.variable()

        return node

    def term(self) -> UnaryOperationNode | BinaryOperationNode | VariableNode | NumberNode:
        node: UnaryOperationNode | BinaryOperationNode | VariableNode | NumberNode = self.factor()

        while self.current_token.type in (TokenType.MUL, TokenType.DIV, TokenType.FLOAT_DIV):
            token: Token = self.current_token
            if self.current_token.type == TokenType.MUL:
                self.eat(TokenType.MUL)
            elif self.current_token.type == TokenType.DIV:
                self.eat(TokenType.DIV)
            elif self.current_token.type == TokenType.FLOAT_DIV:
                self.eat(TokenType.FLOAT_DIV)

            node: BinaryOperationNode = BinaryOperationNode(left_operand=node, operator_token=token, right_operand=self.factor())

        return node

    def expr(self) -> UnaryOperationNode | BinaryOperationNode | VariableNode | NumberNode:
        node: UnaryOperationNode | BinaryOperationNode | VariableNode | NumberNode = self.term()

        while self.current_token.type in (TokenType.PLUS, TokenType.MINUS):
            token: Token = self.current_token
            if self.current_token.type == TokenType.PLUS:
                self.eat(TokenType.PLUS)
            elif self.current_token.type == TokenType.MINUS:
                self.eat(TokenType.MINUS)

            node: BinaryOperationNode = BinaryOperationNode(left_operand=node, operator_token=token, right_operand=self.term())

        return node

    def program(self) -> ProgramNode:
        self.eat(TokenType.PROGRAM)
        variable_node: VariableNode = self.variable()
        program_name: str = variable_node.name
        self.eat(TokenType.SEMI)
        block_node: BlockNode = self.block()
        program_node: ProgramNode = ProgramNode(program_name, block_node)
        self.eat(TokenType.DOT)
        return program_node

    def block(self) -> BlockNode:
        declarations: list[VariableDeclarationNode | ProcedureDeclarationNode] = self.declarations()
        compound_statement_node: CompoundNode = self.compound_statement()
        block_node: BlockNode = BlockNode(declarations, compound_statement_node)
        return block_node

    def declarations(self) -> list[VariableDeclarationNode | ProcedureDeclarationNode]:
        declarations: list[VariableDeclarationNode | ProcedureDeclarationNode] = []

        while self.current_token.type == TokenType.VAR:
            self.eat(TokenType.VAR)

            while self.current_token.type == TokenType.ID:
                variable_declarations: list[VariableDeclarationNode] = self.variable_declarations()
                declarations.extend(variable_declarations)
                self.eat(TokenType.SEMI)

        while self.current_token.type == TokenType.PROCEDURE:
            self.eat(TokenType.PROCEDURE)
            procedure_name: str = self.current_token.value
            self.eat(TokenType.ID)

            parameters: list[ParameterDeclarationNode] | None = None
            if self.current_token.type == TokenType.LPAREN:
                parameters = self.formal_parameters_list()

            self.eat(TokenType.SEMI)
            block_node: BlockNode = self.block()
            declarations.append(ProcedureDeclarationNode(procedure_name, parameters, block_node))
            self.eat(TokenType.SEMI)

        return declarations

    def formal_parameters_list(self) -> list[ParameterDeclarationNode]:
        self.eat(TokenType.LPAREN)
        parameters: list[ParameterDeclarationNode] = self.formal_parameters()
        self.eat(TokenType.RPAREN)
        return parameters

    def formal_parameters(self) -> list[ParameterDeclarationNode]:
        parameters: list[ParameterDeclarationNode] = []

        while self.current_token.type != TokenType.RPAREN:
            parameters.extend(self.parameter_declaration())
            if self.current_token.type != TokenType.RPAREN:
                self.eat(TokenType.SEMI)

        return parameters

    def parameter_declaration(self) -> list[ParameterDeclarationNode]:
        variable_nodes: list[VariableNode] = [VariableNode(self.current_token)]
        self.eat(TokenType.ID)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            variable_nodes.append(VariableNode(self.current_token))
            self.eat(TokenType.ID)

        self.eat(TokenType.COLON)
        type_node: TypeNode = self.type_spec()
        parameters: list[ParameterDeclarationNode] = [ParameterDeclarationNode(var_node, type_node) for var_node in variable_nodes]
        return parameters

    def variable_declarations(self) -> list[VariableDeclarationNode]:
        variable_nodes: list[VariableNode] = [VariableNode(self.current_token)]
        self.eat(TokenType.ID)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            variable_nodes.append(VariableNode(self.current_token))
            self.eat(TokenType.ID)

        self.eat(TokenType.COLON)
        type_node: TypeNode = self.type_spec()
        variable_declarations: list[VariableDeclarationNode] = [VariableDeclarationNode(variable_node, type_node) for variable_node in variable_nodes]
        return variable_declarations

    def type_spec(self) -> TypeNode:
        token: Token = self.current_token

        if self.current_token.type == TokenType.INTEGER:
            self.eat(TokenType.INTEGER)
        else:
            self.eat(TokenType.REAL)

        node: TypeNode = TypeNode(token)
        return node

    def compound_statement(self) -> CompoundNode:
        self.eat(TokenType.BEGIN)
        nodes: list[ASTNode] = self.statement_list()
        self.eat(TokenType.END)

        root: CompoundNode = CompoundNode()
        for node in nodes:
            root.children.append(node)

        return root

    def statement_list(self) -> list[ASTNode]:
        node: CompoundNode | AssignNode | NoOpNode = self.statement()

        results: list[CompoundNode | AssignNode | NoOpNode] = [node]

        while self.current_token.type is TokenType.SEMI:
            self.eat(TokenType.SEMI)
            results.append(self.statement())

        return results

    def statement(self) -> CompoundNode | AssignNode | NoOpNode:
        if self.current_token.type is TokenType.BEGIN:
            return self.compound_statement()
        elif self.current_token.type is TokenType.ID:
            return self.assignment_statement()
        else:
            return self.empty()

    def assignment_statement(self) -> AssignNode:
        left_operand: VariableNode = self.variable()
        operator_token: Token = self.current_token
        self.eat(TokenType.ASSIGN)
        right_operand: ASTNode = self.expr()
        return AssignNode(left_operand, operator_token, right_operand)

    def empty(self) -> NoOpNode:
        return NoOpNode()

    def variable(self) -> VariableNode:
        token: Token = self.current_token
        self.eat(TokenType.ID)
        return VariableNode(token)

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
        if self.current_token.type is not TokenType.EOF:
            self.error()

        return node
