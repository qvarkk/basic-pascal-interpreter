from typing import Callable
from ast_nodes import ASTNode


class NodeVisitor(object):
    def visit(self, node: ASTNode) -> any:
        method_name: str = 'visit_' + type(node).__name__
        visitor: Callable[[ASTNode], any] = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node) -> None:
        raise Exception(f'No visit_{type(node).__name__} method')
