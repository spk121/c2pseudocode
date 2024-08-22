#------------------------------------------------------------------------------
# pycparser: pseudocode_generator.py
#
# pseudocode code generator from pycparser AST nodes.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# Mike Gran
# License: BSD
#------------------------------------------------------------------------------
import pycparser

class PseudocodeGenerator(object):
    """ Uses the same visitor pattern as pycparser.c_ast.NodeVisitor, but modified to
        return a value from each visit method, using string accumulation in
        generic_visit.
    """
    def __init__(self, reduce_parentheses=False):
        """ Constructs C-code generator

            reduce_parentheses:
                if True, eliminates needless parentheses on binary operators
        """
        # Statements start with indentation of self.indent_level spaces, using
        # the _make_indent method.
        self.indent_level = 0
        self.reduce_parentheses = reduce_parentheses

    def _make_indent(self):
        return ' ' * self.indent_level

    def visit(self, node):
        method = 'visit_' + node.__class__.__name__
        return getattr(self, method, self.generic_visit)(node)

    def generic_visit(self, node):
        if node is None:
            return ''
        else:
            return ''.join(self.visit(c) for c_name, c in node.children())

    def visit_Constant(self, n):
        return n.value

    def visit_ID(self, n):
        return n.name

    def visit_Pragma(self, n):
        ret = '#pragma'
        if n.string:
            ret += ' ' + n.string
        return ret

    def visit_ArrayRef(self, n):
        arrref = self._parenthesize_unless_simple(n.name)
        return arrref + '[' + self.visit(n.subscript) + ']'

    def visit_StructRef(self, n):
        sref = self._parenthesize_unless_simple(n.name)
        # in pseudocode, always just use '.' for structs
        # instead of n.type
        return sref + '.' + self.visit(n.field)

    def visit_FuncCall(self, n):
        fref = self._parenthesize_unless_simple(n.name)
        return fref + '(' + self.visit(n.args) + ')'

    def visit_UnaryOp(self, n):
        if n.op == 'sizeof':
            # Always parenthesize the argument of sizeof since it can be
            # a name.
            return 'sizeof(%s)' % self.visit(n.expr)
        else:
            operand = self._parenthesize_unless_simple(n.expr)
            if n.op == 'p++':
                # return '%s++' % operand
                return operand + ' = ' + operand + ' + 1'
            elif n.op == '++':
                # return '%s%s' % (n.op, operand)
                return operand + ' = ' + operand + ' + 1'
            elif n.op == 'p--':
                # return '%s--' % operand
                return operand + ' = ' + operand + ' - 1'
            elif n.op == '--':
                # return '%s%s' % (n.op, operand)
                return operand + ' = ' + operand + ' - 1'
            elif n.op == '!':
                return 'NOT ' + operand
            else:
                return '%s%s' % (n.op, operand)

    # Precedence map of binary operators:
    precedence_map = {
        # Should be in sync with c_parser.CParser.precedence
        # Higher numbers are stronger binding
        '||': 0,  # weakest binding
        '&&': 1,
        '|': 2,
        '^': 3,
        '&': 4,
        '==': 5, '!=': 5,
        '>': 6, '>=': 6, '<': 6, '<=': 6,
        '>>': 7, '<<': 7,
        '+': 8, '-': 8,
        '*': 9, '/': 9, '%': 9  # strongest binding
    }

    def visit_BinaryOp(self, n):
        # Note: all binary operators are left-to-right associative
        #
        # If `n.left.op` has a stronger or equally binding precedence in
        # comparison to `n.op`, no parenthesis are needed for the left:
        # e.g., `(a*b) + c` is equivalent to `a*b + c`, as well as
        #       `(a+b) - c` is equivalent to `a+b - c` (same precedence).
        # If the left operator is weaker binding than the current, then
        # parentheses are necessary:
        # e.g., `(a+b) * c` is NOT equivalent to `a+b * c`.
        lval_str = self._parenthesize_if(
            n.left,
            lambda d: not (self._is_simple_node(d) or
                      self.reduce_parentheses and isinstance(d, pycparser.c_ast.BinaryOp) and
                      self.precedence_map[d.op] >= self.precedence_map[n.op]))
        # If `n.right.op` has a stronger -but not equal- binding precedence,
        # parenthesis can be omitted on the right:
        # e.g., `a + (b*c)` is equivalent to `a + b*c`.
        # If the right operator is weaker or equally binding, then parentheses
        # are necessary:
        # e.g., `a * (b+c)` is NOT equivalent to `a * b+c` and
        #       `a - (b+c)` is NOT equivalent to `a - b+c` (same precedence).
        rval_str = self._parenthesize_if(
            n.right,
            lambda d: not (self._is_simple_node(d) or
                      self.reduce_parentheses and isinstance(d, pycparser.c_ast.BinaryOp) and
                      self.precedence_map[d.op] > self.precedence_map[n.op]))
        op = n.op
        if n.op == '&&':
            op = 'AND'
        elif n.op == '||':
            op = 'OR'
        elif n.op == '==':
            op = 'EQUAL'
        elif n.op == '!=':
            op = 'NOTEQUAL'
        return '%s %s %s' % (lval_str, op, rval_str)

    def visit_Assignment(self, n):
        rval_str = self._parenthesize_if(
                            n.rvalue,
                            lambda n: isinstance(n, pycparser.c_ast.Assignment))
        if n.op == '+=':
            return '%s = %s + %s' % (self.visit(n.lvalue), self.visit(n.lvalue), rval_str)
        elif n.op == '-=':
            return '%s = %s * %s' % (self.visit(n.lvalue), self.visit(n.lvalue), rval_str)
        elif n.op == '*=':
            return '%s = %s * %s' % (self.visit(n.lvalue), self.visit(n.lvalue), rval_str)
        elif n.op == '/=':
            return '%s = %s / %s' % (self.visit(n.lvalue), self.visit(n.lvalue), rval_str)
            
        return '%s %s %s' % (self.visit(n.lvalue), n.op, rval_str)

    def visit_IdentifierType(self, n):
        return ' '.join(n.names)

    def _visit_expr(self, n):
        if isinstance(n, pycparser.c_ast.InitList):
            # Maybe pseudocode doesn't need curly braces for init lists
            # return '{' + self.visit(n) + '}'
            return '(' + self.visit(n) + ')'
        elif isinstance(n, pycparser.c_ast.ExprList):
            return '(' + self.visit(n) + ')'
        else:
            return self.visit(n)

    def visit_Decl(self, n, no_type=False):
        # no_type is used when a Decl is part of a DeclList, where the type is
        # explicitly only for the first declaration in a list.
        #
        s = n.name if no_type else self._generate_decl(n)
        if n.bitsize: s += ' : ' + self.visit(n.bitsize)
        if n.init:
            s += ' := ' + self._visit_expr(n.init)
        return s

    def visit_DeclList(self, n):
        s = self.visit(n.decls[0])
        if len(n.decls) > 1:
            s += ', ' + ', '.join(self.visit_Decl(decl, no_type=True)
                                    for decl in n.decls[1:])
        return s

    def visit_Typedef(self, n):
        s = ''
        if n.storage: s += ' '.join(n.storage) + ' '
        s += self._generate_type(n.type)
        return s

    def visit_Cast(self, n):
        "No need to render typecasts in pseudocode"
        s = '(' + self._generate_type(n.to_type, emit_declname=False) + ')'
        return self._parenthesize_unless_simple(n.expr)

    def visit_ExprList(self, n):
        visited_subexprs = []
        for expr in n.exprs:
            visited_subexprs.append(self._visit_expr(expr))
        return ', '.join(visited_subexprs)

    def visit_InitList(self, n):
        visited_subexprs = []
        for expr in n.exprs:
            visited_subexprs.append(self._visit_expr(expr))
        return ', '.join(visited_subexprs)

    def visit_Enum(self, n):
        return self._generate_struct_union_enum(n, name='enum')

    def visit_Alignas(self, n):
        return '_Alignas({})'.format(self.visit(n.alignment))

    def visit_Enumerator(self, n):
        if not n.value:
            return '{indent}{name},\n'.format(
                indent=self._make_indent(),
                name=n.name,
            )
        else:
            return '{indent}{name} = {value},\n'.format(
                indent=self._make_indent(),
                name=n.name,
                value=self.visit(n.value),
            )

    def visit_FuncDef(self, n):
        decl = 'PROCEDURE ' + self.visit(n.decl) + ' IS'
        self.indent_level = 0
        body = self.visit(n.body)
        if n.param_decls:
            knrdecls = ';\n'.join(self.visit(p) for p in n.param_decls)
            return decl + '\n' + knrdecls + ';\n' + body + 'END PROCEDURE\n'
        else:
            return decl + '\nBEGIN\n' + body + 'END PROCEDURE\n\n'

    def visit_FileAST(self, n):
        s = ''
        for ext in n.ext:
            if isinstance(ext, pycparser.c_ast.FuncDef):
                s += self.visit(ext)
            elif isinstance(ext, pycparser.c_ast.Pragma):
                s += self.visit(ext) + '\n'
            else:
                s += self.visit(ext) + '\n'
        return s

    def visit_Compound(self, n):
        s = ''
        self.indent_level += 2
        if n.block_items:
            s += ''.join(self._generate_stmt(stmt) for stmt in n.block_items)
        self.indent_level -= 2
        return s

    def visit_CompoundLiteral(self, n):
        return '(' + self.visit(n.type) + '){' + self.visit(n.init) + '}'


    def visit_EmptyStatement(self, n):
        return 'DO_NOTHING'

    def visit_ParamList(self, n):
        return ', '.join(self.visit(param) for param in n.params)

    def visit_Return(self, n):
        s = 'RETURN'
        if n.expr: s += ' ' + self.visit(n.expr)
        return s

    def visit_Break(self, n):
        return 'BREAK'

    def visit_Continue(self, n):
        return 'CONTINUE'

    def visit_TernaryOp(self, n):
        s  = '(IF (' + self._visit_expr(n.cond) + ') THEN '
        s += '(' + self._visit_expr(n.iftrue) + ') ELSE '
        s += '(' + self._visit_expr(n.iffalse) + '))'
        return s

    def visit_If(self, n):
        s = 'IF '
        if n.cond: s += self.visit(n.cond)
        s += ' THEN\n'
        s += self._generate_stmt(n.iftrue, add_indent=True)
        if n.iffalse:
            s += self._make_indent() + 'ELSE\n'
            s += self._generate_stmt(n.iffalse, add_indent=True)
        s += self._make_indent() +'END IF\n'
        return s

    def visit_For(self, n):
        s = 'FOR '
        if n.init: s += self.visit(n.init)
        s += ';'
        if n.cond: s += ' ' + self.visit(n.cond)
        s += ';'
        if n.next: s += ' ' + self.visit(n.next)
        s += ' LOOP\n'
        s += self._generate_stmt(n.stmt, add_indent=True)
        s += self._make_indent() +'END LOOP'
        return s

    def visit_While(self, n):
        s = 'WHILE ('
        if n.cond: s += self.visit(n.cond)
        s += ')\n'
        s += self._generate_stmt(n.stmt, add_indent=True)
        return s

    def visit_DoWhile(self, n):
        s = 'DO\n'
        s += self._generate_stmt(n.stmt, add_indent=True)
        s += self._make_indent() + 'WHILE ('
        if n.cond: s += self.visit(n.cond)
        s += ')'
        return s

    def visit_StaticAssert(self, n):
        s = '_Static_assert('
        s += self.visit(n.cond)
        if n.message:
            s += ','
            s += self.visit(n.message)
        s += ')'
        return s

    def visit_Switch(self, n):
        s = 'SWITCH ' + self.visit(n.cond) + '\n'
        s += self._generate_stmt(n.stmt, add_indent=True)
        return s

    def visit_Case(self, n):
        s = 'CASE ' + self.visit(n.expr) + ':\n'
        for stmt in n.stmts:
            s += self._generate_stmt(stmt, add_indent=True)
        return s

    def visit_Default(self, n):
        s = 'DEFAULT:\n'
        for stmt in n.stmts:
            s += self._generate_stmt(stmt, add_indent=True)
        return s

    def visit_Label(self, n):
        return n.name + ':\n' + self._generate_stmt(n.stmt)

    def visit_Goto(self, n):
        return 'GOTO ' + n.name

    def visit_EllipsisParam(self, n):
        return '...'

    def visit_Struct(self, n):
        return self._generate_struct_union_enum(n, 'struct')

    def visit_Typename(self, n):
        return self._generate_type(n.type)

    def visit_Union(self, n):
        return self._generate_struct_union_enum(n, 'union')

    def visit_NamedInitializer(self, n):
        s = ''
        for name in n.name:
            if isinstance(name, pycparser.c_ast.ID):
                s += '.' + name.name
            else:
                s += '[' + self.visit(name) + ']'
        s += ' = ' + self._visit_expr(n.expr)
        return s

    def visit_FuncDecl(self, n):
        return self._generate_type(n)

    def visit_ArrayDecl(self, n):
        return self._generate_type(n, emit_declname=False)

    def visit_TypeDecl(self, n):
        return self._generate_type(n, emit_declname=False)

    def visit_PtrDecl(self, n):
        return self._generate_type(n, emit_declname=False)

    def _generate_struct_union_enum(self, n, name):
        """ Generates code for structs, unions, and enums. name should be
            'struct', 'union', or 'enum'.
        """
        if name in ('struct', 'union'):
            members = n.decls
            body_function = self._generate_struct_union_body
        else:
            assert name == 'enum'
            members = None if n.values is None else n.values.enumerators
            body_function = self._generate_enum_body
        s = name + ' ' + (n.name or '')
        if members is not None:
            # None means no members
            # Empty sequence means an empty list of members
            s += '\n'
            s += self._make_indent()
            self.indent_level += 2
            s += '{\n'
            s += body_function(members)
            self.indent_level -= 2
            s += self._make_indent() + '}'
        return s

    def _generate_struct_union_body(self, members):
        return ''.join(self._generate_stmt(decl) for decl in members)

    def _generate_enum_body(self, members):
        # `[:-2] + '\n'` removes the final `,` from the enumerator list
        return ''.join(self.visit(value) for value in members)[:-2] + '\n'

    def _generate_stmt(self, n, add_indent=False):
        """ Generation from a statement node. This method exists as a wrapper
            for individual visit_* methods to handle different treatment of
            some statements in this context.
        """
        typ = type(n)
        if add_indent: self.indent_level += 2
        indent = self._make_indent()
        if add_indent: self.indent_level -= 2

        if typ in (
                pycparser.c_ast.Decl, pycparser.c_ast.Assignment, pycparser.c_ast.Cast, pycparser.c_ast.UnaryOp,
                pycparser.c_ast.BinaryOp, pycparser.c_ast.TernaryOp, pycparser.c_ast.FuncCall, pycparser.c_ast.ArrayRef,
                pycparser.c_ast.StructRef, pycparser.c_ast.Constant, pycparser.c_ast.ID, pycparser.c_ast.Typedef,
                pycparser.c_ast.ExprList):
            # These can also appear in an expression context so no semicolon
            # is added to them automatically
            #
            return indent + self.visit(n) + '\n'
        elif typ in (pycparser.c_ast.Compound,):
            # No extra indentation required before the opening brace of a
            # compound - because it consists of multiple lines it has to
            # compute its own indentation.
            #
            return self.visit(n)
        elif typ in (pycparser.c_ast.If,):
            return indent + self.visit(n)
        else:
            return indent + self.visit(n) + '\n'
 
    def _generate_decl(self, n):
        """ Generation from a Decl node.
        """
        s = ''
        if n.funcspec: s = ' '.join(n.funcspec) + ' '
        # Pseudocode doesn't need storage or alignment
        # if n.storage: s += ' '.join(n.storage) + ' '
        # if n.align: s += self.visit(n.align[0]) + ' '
        s += self._generate_type(n.type)
        return s

    def _generate_type(self, n, modifiers=[], emit_declname = True):
        """ Recursive generation from a type node. n is the type node.
            modifiers collects the PtrDecl, ArrayDecl and FuncDecl modifiers
            encountered on the way down to a TypeDecl, to allow proper
            generation from it.
        """
        long_type_names = {'int': 'Integer', 'char': 'String', 'double' : 'Float64'}
        typ = type(n)
        #~ print(n, modifiers)

        if typ == pycparser.c_ast.TypeDecl:
            s = ''
            # Pseudocode doesn't care about qualifiers
            # if n.quals: s += ' '.join(n.quals) + ' '
            s += self.visit(n.type)

            if s in long_type_names:
                s = long_type_names[s]

            nstr = n.declname if n.declname and emit_declname else ''
            # Resolve modifiers.
            # Wrap in parens to distinguish pointer to array and pointer to
            # function syntax.
            #
            for i, modifier in enumerate(modifiers):
                if isinstance(modifier, pycparser.c_ast.ArrayDecl):
                    if (i != 0 and
                        isinstance(modifiers[i - 1], pycparser.c_ast.PtrDecl)):
                            nstr = '(' + nstr + ')'
                    nstr += '['
                    if modifier.dim_quals:
                        nstr += ' '.join(modifier.dim_quals) + ' '
                    nstr += self.visit(modifier.dim) + ']'
                elif isinstance(modifier, pycparser.c_ast.FuncDecl):
                    if (i != 0 and
                        isinstance(modifiers[i - 1], pycparser.c_ast.PtrDecl)):
                            nstr = '(' + nstr + ')'
                    nstr += '(' + self.visit(modifier.args) + ')'
                elif isinstance(modifier, pycparser.c_ast.PtrDecl):
                    # Pseudocode doesn't care about pointer references
                    #if modifier.quals:
                    #    nstr = '* %s%s' % (' '.join(modifier.quals),
                    #                       ' ' + nstr if nstr else '')
                    #else:
                    #    nstr = '*' + nstr
                    nstr = nstr
            #if nstr: s += ' ' + nstr
            if nstr: s = nstr + ' : ' + s        
            return s
        elif typ == pycparser.c_ast.Decl:
            return self._generate_decl(n.type)
        elif typ == pycparser.c_ast.Typename:
            return self._generate_type(n.type, emit_declname = emit_declname)
        elif typ == pycparser.c_ast.IdentifierType:
            return ' '.join(n.names) + ' '
        elif typ == pycparser.c_ast.FuncDecl:
            s = ''
            s += self._generate_type(n.type, modifiers + [n],
                                       emit_declname = emit_declname)
            return s
        elif typ in (pycparser.c_ast.ArrayDecl, pycparser.c_ast.PtrDecl):
            return self._generate_type(n.type, modifiers + [n],
                                       emit_declname = emit_declname)
        else:
            return self.visit(n)

    def _parenthesize_if(self, n, condition):
        """ Visits 'n' and returns its string representation, parenthesized
            if the condition function applied to the node returns True.
        """
        s = self._visit_expr(n)
        if condition(n):
            return '(' + s + ')'
        else:
            return s

    def _parenthesize_unless_simple(self, n):
        """ Common use case for _parenthesize_if
        """
        return self._parenthesize_if(n, lambda d: not self._is_simple_node(d))

    def _is_simple_node(self, n):
        """ Returns True for nodes that are "simple" - i.e. nodes that always
            have higher precedence than operators.
        """
        return isinstance(n, (pycparser.c_ast.Constant, pycparser.c_ast.ID, pycparser.c_ast.ArrayRef,
                              pycparser.c_ast.StructRef, pycparser.c_ast.FuncCall))