from typing import Optional

from ast_nodes import *
from errors import ErrorCode, SemanticError
from node_visitor import NodeVisitor
from symbols import VariableSymbol, ProcedureSymbol, Symbol
from symbol_table import ScopedSymbolTable


class SemanticAnalyzer(NodeVisitor):
    def __init__(self, debug: Optional[bool] = False) -> None:
        self.debug = debug
        self.current_scope: ScopedSymbolTable | None = None

    def log(self, message: str) -> None:
        if self.debug:
            print(message)

    def error(self, error_code: ErrorCode, variable_node: Optional[VariableNode] = None) -> None:
        raise SemanticError(error_code, variable_token=variable_node.token)

    def visit_ProgramNode(self, node: ProgramNode) -> None:
        self.current_scope = ScopedSymbolTable('builtins', 0)
        self.current_scope.init_builtins()

        global_scope: ScopedSymbolTable = ScopedSymbolTable(
            scope_name='global',
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope
        )

        self.current_scope = global_scope
        self.visit(node.block_node)
        self.log(str(self.current_scope))

        self.current_scope = self.current_scope.enclosing_scope

    def visit_BlockNode(self, node: BlockNode) -> None:
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_node)

    def visit_VariableDeclarationNode(self, node: VariableDeclarationNode) -> None:
        if self.current_scope is None:
            self.error(ErrorCode.UNEXPECTED_ERROR)

        type_name: str = str(node.type_node.type)
        type_symbol: Symbol | None = self.current_scope.lookup(type_name)
        var_name: str = str(node.variable_node.name)

        if self.current_scope.lookup(var_name, current_scope_only=True) is not None:
            self.error(ErrorCode.DUPLICATE_ID, node.variable_node)

        self.current_scope.define(VariableSymbol(var_name, type_symbol))

    def visit_ProcedureDeclarationNode(self, node: ProcedureDeclarationNode) -> None:
        if self.current_scope is None:
            self.error(ErrorCode.UNEXPECTED_ERROR)

        procedure_name: str = node.name
        procedure_symbol: ProcedureSymbol = ProcedureSymbol(procedure_name)
        self.current_scope.define(procedure_symbol)

        procedure_scope: ScopedSymbolTable = ScopedSymbolTable(
            scope_name=procedure_name,
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope)

        self.current_scope = procedure_scope

        if node.parameters is not None:
            for parameter in node.parameters:
                type_symbol: Symbol | None = self.current_scope.lookup(str(parameter.type_node.type))
                param_name: str = str(parameter.variable_node.name)
                param_symbol: VariableSymbol = VariableSymbol(param_name, type_symbol)
                self.current_scope.define(param_symbol)
                procedure_symbol.parameters.append(param_symbol)

        self.visit(node.block_node)
        self.log(str(self.current_scope))

        self.current_scope = self.current_scope.enclosing_scope

    def visit_CompoundNode(self, node: CompoundNode) -> None:
        for child in node.children:
            self.visit(child)

    def visit_AssignNode(self, node: AssignNode) -> None:
        if self.current_scope is None:
            self.error(ErrorCode.UNEXPECTED_ERROR)

        variable: Symbol | None = self.current_scope.lookup(str(node.left_operand.name))

        if variable is None:
            self.error(ErrorCode.ID_NOT_FOUND, node.left_operand)

        self.visit(node.right_operand)

    def visit_VariableNode(self, node: VariableNode):
        if self.current_scope is None:
            self.error(ErrorCode.UNEXPECTED_ERROR)

        variable: Symbol | None = self.current_scope.lookup(str(node.name))

        if variable is None:
            self.error(ErrorCode.ID_NOT_FOUND, node)

    def visit_NoOpNode(self, node: NoOpNode):
        pass

    def visit_BinaryOperationNode(self, node: BinaryOperationNode):
        self.visit(node.left_operand)
        self.visit(node.right_operand)

    def visit_NumberNode(self, node: NumberNode):
        pass

    def visit_UnaryOperationNode(self, node: UnaryOperationNode):
        self.visit(node.operand)
