from typing import TypeAlias
from ast_nodes import UnaryOperationNode, BinaryOperationNode, VariableNode, NumberNode, VariableDeclarationNode, \
    ProcedureDeclarationNode, CompoundNode, AssignNode, NoOpNode

ExpressionNode: TypeAlias = UnaryOperationNode | BinaryOperationNode | VariableNode | NumberNode

DeclarationNode: TypeAlias = VariableDeclarationNode | ProcedureDeclarationNode

StatementNode: TypeAlias = CompoundNode | AssignNode | NoOpNode
