from collections import OrderedDict
from symbols import BuiltinTypeSymbol

class ScopedSymbolTable(object):
    def __init__(self, scope_name, scope_level, enclosing_scope=None):
        self._symbols = OrderedDict()
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope

    def init_builtins(self):
        self.define(BuiltinTypeSymbol('INTEGER'))
        self.define(BuiltinTypeSymbol('REAL'))

    def __str__(self):
        lines = []
        scope_header = '\nSCOPE (ScopedSymbolTable):'
        lines.append(scope_header)
        lines.append('=' * len(scope_header))

        scope_name_info = f'Name: {self.scope_name}'
        scope_level_info = f'Level: {self.scope_level}'
        lines.append(scope_name_info)
        lines.append(scope_level_info)

        enclosing_scope_info = f'Enclosing Scope: {self.enclosing_scope.scope_name if self.enclosing_scope else None}'
        lines.append(enclosing_scope_info)

        scope_symbols_header = 'Scope contents:'
        lines.append(scope_symbols_header)
        lines.append('-' * len(scope_symbols_header))

        for key, value in self._symbols.items():
            lines.append('%-14s: %s' % (key, value))

        return '\n'.join(lines)

    __repr__ = __str__

    def define(self, symbol):
        self._symbols[symbol.name] = symbol

    def lookup(self, name, current_scope_only=False):
        symbol = self._symbols.get(name)

        if symbol is not None:
            return symbol

        if current_scope_only:
            return None

        if self.enclosing_scope is not None:
            return self.enclosing_scope.lookup(name)
