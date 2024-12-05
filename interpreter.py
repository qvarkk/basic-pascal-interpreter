from tokens import PLUS, MINUS, MUL, DIV, FLOAT_DIV
from node_visitor import NodeVisitor


class Interpreter(NodeVisitor):
    def __init__(self, parser):
        self.parser = parser
        self.GLOBAL_SCOPE = {}

    def visit_Program(self, node):
        self.visit(node.block_node)

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        pass

    def visit_Type(self, node):
        pass

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
            return self.visit(node.left) // self.visit(node.right)
        elif node.op.type == FLOAT_DIV:
            return float(self.visit(node.left) / self.visit(node.right))

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

