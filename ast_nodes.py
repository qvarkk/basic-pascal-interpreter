from tokens import Token


class ASTNode(object):
    def __str__(self) -> str:
        return f'<AST()>'


class VariableNode(ASTNode):
    def __init__(self, token: Token) -> None:
        self.token: Token = token
        self.name: str = token.value

    def __str__(self) -> str:
        return f'<Variable(name={self.name})>'


class TypeNode(ASTNode):
    def __init__(self, token: Token) -> None:
        self.token: Token = token
        self.type: str = token.value

    def __str__(self) -> str:
        return f'<Type(name={self.type})>'


class VariableDeclarationNode(ASTNode):
    def __init__(self, variable_node: VariableNode, type_node: TypeNode) -> None:
        self.variable_node: VariableNode = variable_node
        self.type_node: TypeNode = type_node

    def __str__(self) -> str:
        return f'<VariableDeclaration(variable_node={self.variable_node}, type={self.type_node})>'


class ParameterDeclarationNode(ASTNode):
    def __init__(self, variable_node: VariableNode, type_node: TypeNode) -> None:
        self.variable_node: VariableNode = variable_node
        self.type_node: TypeNode = type_node

    def __str__(self) -> str:
        return f'<ParameterDeclaration(variable_node={self.variable_node}, type={self.type_node})>'


class ProcedureDeclarationNode(ASTNode):
    def __init__(self, name: str, parameters: list[ParameterDeclarationNode] | None, block_node: 'BlockNode') -> None:
        self.name: str = name
        self.parameters: list[ParameterDeclarationNode] = parameters
        self.block_node: BlockNode = block_node

    def __str__(self) -> str:
        return f'<ProcedureDeclaration(name={self.name})>'


class CompoundNode(ASTNode):
    def __init__(self) -> None:
        self.children: list[ASTNode] = []

    def __str__(self) -> str:
        return f'<Compound()>'


class BlockNode(ASTNode):
    def __init__(self, declarations: list[VariableDeclarationNode | ProcedureDeclarationNode], compound_node: CompoundNode) -> None:
        self.declarations: list[VariableDeclarationNode | ProcedureDeclarationNode] = declarations
        self.compound_node: CompoundNode = compound_node

    def __str__(self) -> str:
        return f'<Block()>'


class ProgramNode(ASTNode):
    def __init__(self, name: str, block_node: BlockNode) -> None:
        self.name: str = name
        self.block_node: BlockNode = block_node

    def __str__(self) -> str:
        return f'<Program(name={self.name})>'


class AssignNode(ASTNode):
    def __init__(self, left_operand: VariableNode, operator_token: Token, right_operand: ASTNode) -> None:
        self.left_operand: VariableNode = left_operand
        self.operator_token: Token = operator_token
        self.right_operand: ASTNode = right_operand

    def __str__(self) -> str:
        return f'<Assign({self.operator_token}, {self.left_operand}, {self.right_operand})>'


class NoOpNode(ASTNode):
    def __str__(self) -> str:
        return f'<NoOp()>'


class BinaryOperationNode(ASTNode):
    def __init__(self, left_operand: ASTNode, operator_token: Token, right_operand: ASTNode) -> None:
        self.left_operand: ASTNode = left_operand
        self.operator_token: Token = operator_token
        self.right_operand: ASTNode = right_operand

    def __str__(self) -> str:
        return f'BinaryOperation(left: {self.left_operand}, operator: {self.operator_token}, right: {self.right_operand})'


class UnaryOperationNode(ASTNode):
    def __init__(self, operator_token: Token, operand: ASTNode) -> None:
        self.operator_token: Token = operator_token
        self.operand: ASTNode = operand

    def __str__(self) -> str:
        return f'UnaryOperation(operator: {self.operator_token}, right: {self.operand})'


class NumberNode(ASTNode):
    def __init__(self, token: Token) -> None:
        self.token: Token = token
        self.value: int | float = token.value

    def __str__(self) -> str:
        return f'Number(value={self.value})'
