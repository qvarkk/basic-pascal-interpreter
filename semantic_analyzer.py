from ast_nodes import *
from node_visitor import NodeVisitor
from symbols import VariableSymbol, ProcedureSymbol
from symbol_table import ScopedSymbolTable


class SemanticAnalyzer(NodeVisitor):
    def __init__(self):
        self.current_scope = None

    def visit_ProgramNode(self, node: ProgramNode):
        self.current_scope = ScopedSymbolTable('builtins', 0)
        self.current_scope.init_builtins()

        global_scope = ScopedSymbolTable(
            scope_name='global',
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope
        )

        self.current_scope = global_scope
        self.visit(node.block_node)
        print(self.current_scope)

        self.current_scope = self.current_scope.enclosing_scope

    def visit_BlockNode(self, node: BlockNode):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_node)

    def visit_VariableDeclarationNode(self, node: VariableDeclarationNode):
        type_name = node.type_node.type
        type_symbol = self.current_scope.lookup(type_name)
        var_name = node.variable_node.name

        if self.current_scope.lookup(var_name, current_scope_only=True) is not None:
            raise Exception(f'Duplicate identifier {var_name} found')

        self.current_scope.define(VariableSymbol(var_name, type_symbol))

    def visit_ProcedureDeclarationNode(self, node: ProcedureDeclarationNode):
        procedure_name = node.name
        procedure_symbol = ProcedureSymbol(procedure_name)
        self.current_scope.define(procedure_symbol)

        procedure_scope = ScopedSymbolTable(
            scope_name=procedure_name,
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope)

        self.current_scope = procedure_scope

        for parameter in node.parameters:
            type_symbol = self.current_scope.lookup(parameter.type_node.type)
            param_name = parameter.variable_node.name
            param_symbol = VariableSymbol(param_name, type_symbol)
            self.current_scope.define(param_symbol)
            procedure_symbol.parameters.append(param_symbol)

        self.visit(node.block_node)
        self.current_scope = self.current_scope.enclosing_scope

    def visit_CompoundNode(self, node: CompoundNode):
        for child in node.children:
            self.visit(child)

    def visit_AssignNode(self, node: AssignNode):
        var = self.current_scope.lookup(node.left_operand.name)

        if var is None:
            raise NameError(f'Variable {node.left_operand.name} not found')

        self.visit(node.right_operand)

    def visit_VariableNode(self, node: VariableNode):
        var = self.current_scope.lookup(node.name)

        if var is None:
            raise NameError(f'Variable {node.name} not found')

    def visit_NoOpNode(self, node: NoOpNode):
        pass

    def visit_BinaryOperationNode(self, node: BinaryOperationNode):
        self.visit(node.left_operand)
        self.visit(node.right_operand)

    def visit_NumberNode(self, node: NumberNode):
        pass

    def visit_UnaryOperationNode(self, node: UnaryOperationNode):
        self.visit(node.operand)
