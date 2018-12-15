#ifndef MAJI_AST_H
#define MAJI_AST_H

#include <assert.h>
#include <inttypes.h>

struct ast_expr;
struct ast_stmt;
struct ast_decl;
struct ast_typespec;
struct symbol;

struct ast_stmt_block
{
    struct ast_stmt **statements;
    size_t statements_count;
};

static inline void ast_print_typespec(struct ast_typespec *type);
static inline void ast_print_expr(struct ast_expr *expr);
static inline void ast_print_decl(struct ast_decl *decl);
static inline void ast_print_stmt_block(struct ast_stmt_block block);
static inline void ast_print_stmt(struct ast_stmt *stmt);

enum ast_typespec_kind
{
    AST_TYPESPEC_NONE,
    AST_TYPESPEC_IDENTIFIER,
    AST_TYPESPEC_FUNC,
    AST_TYPESPEC_ARRAY,
    AST_TYPESPEC_POINTER,
};

struct ast_typespec_func
{
    size_t args_count;
    struct ast_typespec **args;
    struct ast_typespec *ret;
};

struct ast_typespec_ptr
{
    struct ast_typespec *elem;
};

struct ast_typespec_array
{
    struct ast_typespec *elem;
    struct ast_expr *size;
};

struct ast_typespec
{
    enum ast_typespec_kind kind;
    union {
        const uint8_t *name;
        struct ast_typespec_func func;
        struct ast_typespec_array array;
        struct ast_typespec_ptr ptr;
    };
};

static inline struct ast_typespec *
ast_typespec_alloc(enum ast_typespec_kind kind)
{
    struct ast_typespec *type = malloc(sizeof(struct ast_typespec));
    memset(type, 0, sizeof(struct ast_typespec));
    type->kind = kind;
    return type;
}

static inline struct ast_typespec *
ast_typespec_identifier(const uint8_t *name)
{
    struct ast_typespec *type = ast_typespec_alloc(AST_TYPESPEC_IDENTIFIER);
    type->name = name;
    return type;
}

static inline struct ast_typespec *
ast_typespec_func(struct ast_typespec **args, size_t args_count, struct ast_typespec *ret)
{
    struct ast_typespec *type = ast_typespec_alloc(AST_TYPESPEC_FUNC);
    type->func.args = args;
    type->func.args_count = args_count;
    type->func.ret = ret;
    return type;
}

static inline struct ast_typespec *
ast_typespec_array(struct ast_typespec *elem, struct ast_expr *size)
{
    struct ast_typespec *type = ast_typespec_alloc(AST_TYPESPEC_ARRAY);
    type->array.elem = elem;
    type->array.size = size;
    return type;
}

static inline struct ast_typespec *
ast_typespec_pointer(struct ast_typespec *elem)
{
    struct ast_typespec *type = ast_typespec_alloc(AST_TYPESPEC_POINTER);
    type->ptr.elem = elem;
    return type;
}

enum ast_decl_kind
{
    AST_DECL_NONE,
    AST_DECL_ENUM,
    AST_DECL_STRUCT,
    AST_DECL_UNION,
    AST_DECL_VAR,
    AST_DECL_CONST,
    AST_DECL_FUNC,
    AST_DECL_FUNC_FOREIGN
};

struct ast_func_param
{
    const uint8_t *name;
    struct ast_typespec *type;
};

struct ast_enum_item
{
    const uint8_t *name;
    struct ast_expr *expr;
};

struct ast_struct_item
{
    const uint8_t *name;
    struct ast_typespec *type;
};

struct ast_decl_func
{
    struct ast_func_param *params;
    size_t params_count;
    struct ast_typespec *ret_type;
    struct ast_stmt_block block;
};

struct ast_decl_func_foreign
{
    struct ast_func_param *params;
    size_t params_count;
    struct ast_typespec *ret_type;
    const uint8_t *lib;
};

struct ast_decl_enum
{
    struct ast_enum_item *items;
    size_t items_count;
};

struct ast_decl_struct
{
    struct ast_struct_item *items;
    size_t items_count;
};

struct ast_decl_var
{
    struct ast_typespec *type;
    struct ast_expr *expr;
};

struct ast_decl_const
{
    struct ast_expr *expr;
};

struct ast_decl
{
    enum ast_decl_kind kind;
    const uint8_t *name;
    union {
        struct ast_decl_enum enum_decl;
        struct ast_decl_struct struct_decl;
        struct ast_decl_func func_decl;
        struct ast_decl_func_foreign foreign_func_decl;
        struct ast_decl_var var_decl;
        struct ast_decl_const const_decl;
    };
};

static inline struct ast_decl *
ast_decl_alloc(enum ast_decl_kind kind, const uint8_t *name)
{
    struct ast_decl *decl = malloc(sizeof(struct ast_decl));
    memset(decl, 0, sizeof(struct ast_decl));
    decl->kind = kind;
    decl->name = name;
    return decl;
}

static inline struct ast_decl *
ast_decl_enum(const uint8_t *name, struct ast_enum_item *items, size_t items_count)
{
    struct ast_decl *decl = ast_decl_alloc(AST_DECL_ENUM, name);
    decl->enum_decl.items = items;
    decl->enum_decl.items_count = items_count;
    return decl;
}

static inline struct ast_decl *
ast_decl_struct(const uint8_t *name, struct ast_struct_item *items, size_t items_count)
{
    struct ast_decl *decl = ast_decl_alloc(AST_DECL_STRUCT, name);
    decl->struct_decl.items = items;
    decl->struct_decl.items_count = items_count;
    return decl;
}

static inline struct ast_decl *
ast_decl_var(const uint8_t *name, struct ast_typespec *type, struct ast_expr *expr)
{
    struct ast_decl *decl = ast_decl_alloc(AST_DECL_VAR, name);
    decl->var_decl.type = type;
    decl->var_decl.expr = expr;
    return decl;
}

static inline struct ast_decl *
ast_decl_func(const uint8_t *name, struct ast_func_param *params, size_t params_count, struct ast_typespec *ret_type, struct ast_stmt_block block)
{
    struct ast_decl *decl = ast_decl_alloc(AST_DECL_FUNC, name);
    decl->func_decl.params = params;
    decl->func_decl.params_count = params_count;
    decl->func_decl.ret_type = ret_type;
    decl->func_decl.block = block;
    return decl;
}

static inline struct ast_decl *
ast_decl_func_foreign(const uint8_t *name, struct ast_func_param *params, size_t params_count, struct ast_typespec *ret_type, const uint8_t *lib)
{
    struct ast_decl *decl = ast_decl_alloc(AST_DECL_FUNC_FOREIGN, name);
    decl->foreign_func_decl.params = params;
    decl->foreign_func_decl.params_count = params_count;
    decl->foreign_func_decl.ret_type = ret_type;
    decl->foreign_func_decl.lib = lib;
    return decl;
}

static inline struct ast_decl *
ast_decl_const(const uint8_t *name, struct ast_expr *expr)
{
    struct ast_decl *decl = ast_decl_alloc(AST_DECL_CONST, name);
    decl->const_decl.expr = expr;
    return decl;
}

enum ast_expr_kind
{
    AST_EXPR_NONE,

    AST_EXPR_IDENTIFIER,
    AST_EXPR_INT_LITERAL,
    AST_EXPR_FLOAT_LITERAL,
    AST_EXPR_STRING_LITERAL,

    AST_EXPR_CALL,
    AST_EXPR_CAST,
    AST_EXPR_INDEX,
    AST_EXPR_FIELD,

    AST_EXPR_UNARY,
    AST_EXPR_BINARY,
    AST_EXPR_TERNARY,
};

struct ast_expr_call
{
    struct ast_expr *expr;
    struct ast_expr **args;
    size_t args_count;
};

struct ast_expr_cast
{
    struct ast_typespec *type;
    struct ast_expr *expr;
};

struct ast_expr_index
{
    struct ast_expr *expr;
    struct ast_expr *index;
};

struct ast_expr_field
{
    struct ast_expr *expr;
    const uint8_t *name;
};

struct ast_expr_unary
{
    enum token_kind op;
    struct ast_expr *expr;
};

struct ast_expr_binary
{
    enum token_kind op;
    struct ast_expr *left_expr;
    struct ast_expr *right_expr;
};

struct ast_expr_ternary
{
    struct ast_expr *condition;
    struct ast_expr *then_expr;
    struct ast_expr *else_expr;
};

struct ast_expr
{
    enum ast_expr_kind kind;
    struct symbol *symbol;

    union {
        uint64_t int_val;
        double float_val;
        const uint8_t *string_val;
        const uint8_t *name;
        struct ast_expr_call call;
        struct ast_expr_cast cast;
        struct ast_expr_index index;
        struct ast_expr_field field;
        struct ast_expr_unary unary;
        struct ast_expr_binary binary;
        struct ast_expr_ternary ternary;
    };
};

static inline struct ast_expr *
ast_expr_alloc(enum ast_expr_kind kind)
{
    struct ast_expr *result = malloc(sizeof(struct ast_expr));
    memset(result, 0, sizeof(struct ast_expr));
    result->kind = kind;
    return result;
}

static inline struct ast_expr *
ast_expr_int(uint64_t value)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_INT_LITERAL);
    result->int_val = value;
    return result;
}

static inline struct ast_expr *
ast_expr_float(double value)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_FLOAT_LITERAL);
    result->float_val = value;
    return result;
}

static inline struct ast_expr *
ast_expr_string(const uint8_t *string)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_STRING_LITERAL);
    result->string_val = string;
    return result;
}

static inline struct ast_expr *
ast_expr_identifier(const uint8_t *name)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_IDENTIFIER);
    result->name = name;
    return result;
}

static inline struct ast_expr *
ast_expr_call(struct ast_expr *expr, struct ast_expr **args, size_t args_count)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_CALL);
    result->call.expr = expr;
    result->call.args = args;
    result->call.args_count = args_count;
    return result;
}

static inline struct ast_expr *
ast_expr_cast(struct ast_typespec *type, struct ast_expr *expr)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_CAST);
    result->cast.type = type;
    result->cast.expr = expr;
    return result;
}

static inline struct ast_expr *
ast_expr_index(struct ast_expr *operand, struct ast_expr *index)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_INDEX);
    result->index.expr = operand;
    result->index.index = index;
    return result;
}


static inline struct ast_expr *
ast_expr_field(struct ast_expr *operand, const uint8_t *field)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_FIELD);
    result->field.expr = operand;
    result->field.name = field;
    return result;
}

static inline struct ast_expr *
ast_expr_unary(enum token_kind op, struct ast_expr *expr)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_UNARY);
    result->unary.op = op;
    result->unary.expr = expr;
    return result;
}

static inline struct ast_expr *
ast_expr_binary(enum token_kind op, struct ast_expr *left_expr, struct ast_expr *right_expr)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_BINARY);
    result->binary.op = op;
    result->binary.left_expr = left_expr;
    result->binary.right_expr = right_expr;
    return result;
}

static inline struct ast_expr *
ast_expr_ternary(struct ast_expr *condition, struct ast_expr *then_expr, struct ast_expr *else_expr)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_TERNARY);
    result->ternary.condition = condition;
    result->ternary.then_expr = then_expr;
    result->ternary.else_expr = else_expr;
    return result;
}

enum ast_stmt_kind
{
    AST_STMT_NONE,
    AST_STMT_RETURN,
    AST_STMT_BREAK,
    AST_STMT_CONTINUE,
    AST_STMT_BLOCK,
    AST_STMT_IF,
    AST_STMT_WHILE,
    AST_STMT_FOR,
    AST_STMT_EXPR,
    AST_STMT_ASSIGN,
    AST_STMT_INIT,
    AST_STMT_CONST,
    AST_STMT_DECL,
};

struct ast_else_if
{
    struct ast_expr *condition;
    struct ast_stmt_block block;
};

struct ast_stmt_return
{
    struct ast_expr *expr;
};

struct ast_stmt_if
{
    struct ast_expr *condition;
    struct ast_stmt_block then_block;
    struct ast_else_if *else_ifs;
    size_t else_ifs_count;
    struct ast_stmt_block else_block;
};

struct ast_stmt_while
{
    struct ast_expr *condition;
    struct ast_stmt_block block;
};

struct ast_stmt_for
{
    struct ast_stmt *init;
    struct ast_expr *condition;
    struct ast_stmt *next;
    struct ast_stmt_block block;
};

struct ast_stmt_assign
{
    enum token_kind op;
    struct ast_expr *left_expr;
    struct ast_expr *right_expr;
};

struct ast_stmt_init
{
    const uint8_t *name;
    struct ast_expr *expr;
    uint64_t address;
};

struct ast_stmt_decl
{
    const uint8_t *name;
    struct ast_typespec *type;
    struct ast_expr *expr;
    uint64_t address;
};

struct ast_stmt
{
    enum ast_stmt_kind kind;
    union {
        struct ast_stmt_return return_stmt;
        struct ast_stmt_if if_stmt;
        struct ast_stmt_while while_stmt;
        struct ast_stmt_for for_stmt;
        struct ast_stmt_block block;
        struct ast_stmt_assign assign;
        struct ast_stmt_init init;
        struct ast_stmt_decl decl;
        struct ast_expr *expr;
    };
};

static inline struct ast_stmt *
ast_stmt_alloc(enum ast_stmt_kind kind)
{
    struct ast_stmt *stmt = malloc(sizeof(struct ast_stmt));
    memset(stmt, 0, sizeof(struct ast_stmt));
    stmt->kind = kind;
    return stmt;
}

static inline struct ast_stmt *
ast_stmt_return(struct ast_expr *expr)
{
    struct ast_stmt *stmt = ast_stmt_alloc(AST_STMT_RETURN);
    stmt->return_stmt.expr = expr;
    return stmt;
}

static inline struct ast_stmt *
ast_stmt_break(void)
{
    struct ast_stmt *stmt = ast_stmt_alloc(AST_STMT_BREAK);
    return stmt;
}

static inline struct ast_stmt *
ast_stmt_continue(void)
{
    struct ast_stmt *stmt = ast_stmt_alloc(AST_STMT_CONTINUE);
    return stmt;
}

static inline struct ast_stmt *
ast_stmt_block(struct ast_stmt_block block)
{
    struct ast_stmt *stmt = ast_stmt_alloc(AST_STMT_BLOCK);
    stmt->block = block;
    return stmt;
}

static inline struct ast_stmt *
ast_stmt_if(struct ast_expr *condition, struct ast_stmt_block then_block, struct ast_else_if *else_ifs, size_t else_ifs_count, struct ast_stmt_block else_block)
{
    struct ast_stmt *stmt = ast_stmt_alloc(AST_STMT_IF);
    stmt->if_stmt.condition = condition;
    stmt->if_stmt.then_block = then_block;
    stmt->if_stmt.else_ifs = else_ifs;
    stmt->if_stmt.else_ifs_count = else_ifs_count;
    stmt->if_stmt.else_block = else_block;
    return stmt;
}

static inline struct ast_stmt *
ast_stmt_while(struct ast_expr *condition, struct ast_stmt_block block)
{
    struct ast_stmt *stmt = ast_stmt_alloc(AST_STMT_WHILE);
    stmt->while_stmt.condition = condition;
    stmt->while_stmt.block = block;
    return stmt;
}

static inline struct ast_stmt *
ast_stmt_for(struct ast_stmt *init, struct ast_expr *condition, struct ast_stmt *next, struct ast_stmt_block block)
{
    struct ast_stmt *stmt = ast_stmt_alloc(AST_STMT_FOR);
    stmt->for_stmt.init = init;
    stmt->for_stmt.condition = condition;
    stmt->for_stmt.next = next;
    stmt->for_stmt.block = block;
    return stmt;
}

static inline struct ast_stmt *
ast_stmt_assign(enum token_kind op, struct ast_expr *left_expr, struct ast_expr *right_expr)
{
    struct ast_stmt *stmt = ast_stmt_alloc(AST_STMT_ASSIGN);
    stmt->assign.op = op;
    stmt->assign.left_expr = left_expr;
    stmt->assign.right_expr = right_expr;
    return stmt;
}

static inline struct ast_stmt *
ast_stmt_init(const uint8_t *name, struct ast_expr *expr)
{
    struct ast_stmt *stmt = ast_stmt_alloc(AST_STMT_INIT);
    stmt->init.name = name;
    stmt->init.expr = expr;
    return stmt;
}

static inline struct ast_stmt *
ast_stmt_const(const uint8_t *name, struct ast_expr *expr)
{
    struct ast_stmt *stmt = ast_stmt_alloc(AST_STMT_CONST);
    stmt->init.name = name;
    stmt->init.expr = expr;
    return stmt;
}

static inline struct ast_stmt *
ast_stmt_decl(const uint8_t *name, struct ast_typespec *type, struct ast_expr *expr)
{
    struct ast_stmt *stmt = ast_stmt_alloc(AST_STMT_DECL);
    stmt->decl.name = name;
    stmt->decl.type = type;
    stmt->decl.expr = expr;
    return stmt;
}

static inline struct ast_stmt *
ast_stmt_expr(struct ast_expr *expr)
{
    struct ast_stmt *stmt = ast_stmt_alloc(AST_STMT_EXPR);
    stmt->expr = expr;
    return stmt;
}


static int ast_print_indent;
void ast_print_newline(void)
{
    printf("\n%.*s", 2*ast_print_indent, "                                                        ");
}

static inline void
ast_print_typespec(struct ast_typespec *type)
{
    switch (type->kind) {
    case AST_TYPESPEC_IDENTIFIER:
        printf("%s", type->name);
        break;
    case AST_TYPESPEC_ARRAY:
        printf("(array ");
        ast_print_typespec(type->array.elem);
        if (type->array.size) {
            printf(" ");
            ast_print_expr(type->array.size);
        }
        printf(")");
        break;
    case AST_TYPESPEC_POINTER:
        printf("(ptr ");
        ast_print_typespec(type->ptr.elem);
        printf(")");
        break;
    case AST_TYPESPEC_FUNC:
        printf("(func (");
        for (struct ast_typespec **it = type->func.args;
             it != type->func.args + type->func.args_count;
             ++it) {
            printf(" ");
            ast_print_typespec(*it);
        }
        printf(") ");
        ast_print_typespec(type->func.ret);
        printf(")");
        break;
    default:
        assert(0);
        break;
    }
}

static inline void
ast_print_expr(struct ast_expr *expr)
{
    switch (expr->kind) {
    case AST_EXPR_INT_LITERAL:
        printf("%" PRIu64, expr->int_val);
        break;
    case AST_EXPR_FLOAT_LITERAL:
        printf("%lf", expr->float_val);
        break;
    case AST_EXPR_STRING_LITERAL:
        printf("\"%s\"", expr->string_val);
        break;
    case AST_EXPR_IDENTIFIER:
        printf("%s", expr->name);
        break;
    case AST_EXPR_CALL:
        printf("(call ");
        ast_print_expr(expr->call.expr);
        for (struct ast_expr **it = expr->call.args;
             it != expr->call.args + expr->call.args_count;
             ++it) {
            printf(" ");
            ast_print_expr(*it);
        }
        printf(")");
        break;
    case AST_EXPR_CAST:
        printf("(cast ");
        ast_print_typespec(expr->cast.type);
        printf(" ");
        ast_print_expr(expr->cast.expr);
        printf(")");
        break;
    case AST_EXPR_INDEX:
        printf("(index ");
        ast_print_expr(expr->index.expr);
        printf(" ");
        ast_print_expr(expr->index.index);
        printf(")");
        break;
    case AST_EXPR_FIELD:
        printf("(field ");
        ast_print_expr(expr->field.expr);
        printf(" %s)", expr->field.name);
        break;
    case AST_EXPR_UNARY:
        printf("(%s ", token_kind_str[expr->unary.op]);
        ast_print_expr(expr->unary.expr);
        printf(")");
        break;
    case AST_EXPR_BINARY:
        printf("(%s ", token_kind_str[expr->binary.op]);
        ast_print_expr(expr->binary.left_expr);
        printf(" ");
        ast_print_expr(expr->binary.right_expr);
        printf(")");
        break;
    case AST_EXPR_TERNARY:
        printf("(? ");
        ast_print_expr(expr->ternary.condition);
        printf(" ");
        ast_print_expr(expr->ternary.then_expr);
        printf(" ");
        ast_print_expr(expr->ternary.else_expr);
        printf(")");
        break;
    default:
        assert(0);
        break;
    }
}

static inline void
ast_print_decl(struct ast_decl *decl)
{
    switch (decl->kind) {
    case AST_DECL_ENUM:
        printf("(enum %s", decl->name);
        ++ast_print_indent;
        for (struct ast_enum_item *it = decl->enum_decl.items;
             it != decl->enum_decl.items + decl->enum_decl.items_count;
             it++) {
            ast_print_newline();
            printf("(%s ", it->name);
            if (it->expr) {
                ast_print_expr(it->expr);
            } else {
                printf("null");
            }
            printf(")");
        }
        --ast_print_indent;
        printf(")");
        break;
    case AST_DECL_STRUCT:
        printf("(struct %s", decl->name);
        ++ast_print_indent;
        for (struct ast_struct_item *it = decl->struct_decl.items;
             it != decl->struct_decl.items + decl->struct_decl.items_count;
             it++) {
            ast_print_newline();
            printf("(");
            ast_print_typespec(it->type);
            printf(" %s", it->name);
            printf(")");
        }
        --ast_print_indent;
        printf(")");
        break;
    case AST_DECL_VAR:
        printf("(var %s ", decl->name);
        if (decl->var_decl.type) {
            ast_print_typespec(decl->var_decl.type);
        } else {
            printf("null");
        }
        if (decl->var_decl.expr) {
            printf(" ");
            ast_print_expr(decl->var_decl.expr);
        }
        printf(")");
        break;
    case AST_DECL_CONST:
        printf("(const %s ", decl->name);
        ast_print_expr(decl->const_decl.expr);
        printf(")");
        break;
    case AST_DECL_FUNC:
        printf("(func %s ", decl->name);
        printf("(");
        for (struct ast_func_param *it = decl->func_decl.params;
             it != decl->func_decl.params + decl->func_decl.params_count;
             it++) {
            printf(" %s ", it->name);
            ast_print_typespec(it->type);
        }
        printf(" ) ");
        if (decl->func_decl.ret_type) {
            ast_print_typespec(decl->func_decl.ret_type);
        } else {
            printf("null");
        }
        ++ast_print_indent;
        ast_print_newline();
        ast_print_stmt_block(decl->func_decl.block);
        --ast_print_indent;
        printf(")");
        break;
    case AST_DECL_FUNC_FOREIGN:
        printf("(foreign func %s <-> %s ", decl->name, decl->foreign_func_decl.lib);
        printf("(");
        for (struct ast_func_param *it = decl->foreign_func_decl.params;
             it != decl->foreign_func_decl.params + decl->foreign_func_decl.params_count;
             it++) {
            printf(" %s ", it->name);
            ast_print_typespec(it->type);
        }
        printf(" ) ");
        if (decl->foreign_func_decl.ret_type) {
            ast_print_typespec(decl->foreign_func_decl.ret_type);
        } else {
            printf("null");
        }
        printf(")");
        break;
    default:
        assert(0);
        break;
    }
}

static inline void
ast_print_stmt_block(struct ast_stmt_block block)
{
    printf("(block");
    ++ast_print_indent;
    for (struct ast_stmt **it = block.statements;
         it != block.statements + block.statements_count;
         ++it) {
        ast_print_newline();
        ast_print_stmt(*it);
    }
    --ast_print_indent;
    printf(")");
}

static inline void
ast_print_stmt(struct ast_stmt *stmt)
{
    switch (stmt->kind) {
    case AST_STMT_RETURN:
        printf("(return ");
        if (stmt->return_stmt.expr) {
            ast_print_expr(stmt->return_stmt.expr);
        }
        printf(")");
        break;
    case AST_STMT_BREAK:
        printf("(break)");
        break;
    case AST_STMT_CONTINUE:
        printf("(continue)");
        break;
    case AST_STMT_BLOCK:
        ast_print_stmt_block(stmt->block);
        break;
    case AST_STMT_IF:
        printf("(if ");
        ast_print_expr(stmt->if_stmt.condition);
        ++ast_print_indent;
        ast_print_newline();
        ast_print_stmt_block(stmt->if_stmt.then_block);
        for (struct ast_else_if *it = stmt->if_stmt.else_ifs;
             it != stmt->if_stmt.else_ifs + stmt->if_stmt.else_ifs_count;
             ++it) {
            ast_print_newline();
            printf("elseif ");
            ast_print_expr(it->condition);
            ast_print_newline();
            ast_print_stmt_block(it->block);
        }
        if (stmt->if_stmt.else_block.statements_count != 0) {
            ast_print_newline();
            printf("else ");
            ast_print_newline();
            ast_print_stmt_block(stmt->if_stmt.else_block);
        }
        --ast_print_indent;
        printf(")");
        break;
    case AST_STMT_WHILE:
        printf("(while ");
        ast_print_expr(stmt->while_stmt.condition);
        ++ast_print_indent;
        ast_print_newline();
        ast_print_stmt_block(stmt->while_stmt.block);
        --ast_print_indent;
        printf(")");
        break;
    case AST_STMT_FOR:
        printf("(for ");
        if (stmt->for_stmt.init) {
            ast_print_stmt(stmt->for_stmt.init);
        }
        if (stmt->for_stmt.condition) {
            ast_print_expr(stmt->for_stmt.condition);
        }
        if (stmt->for_stmt.next) {
            ast_print_stmt(stmt->for_stmt.next);
        }
        ++ast_print_indent;
        ast_print_newline();
        ast_print_stmt_block(stmt->for_stmt.block);
        --ast_print_indent;
        printf(")");
        break;
    case AST_STMT_ASSIGN:
        printf("(%s ", token_kind_str[stmt->assign.op]);
        ast_print_expr(stmt->assign.left_expr);
        if (stmt->assign.right_expr) {
            printf(" ");
            ast_print_expr(stmt->assign.right_expr);
        }
        printf(")");
        break;
    case AST_STMT_INIT:
        printf("(:= %s ", stmt->init.name);
        ast_print_expr(stmt->init.expr);
        printf(")");
        break;
    case AST_STMT_CONST:
        printf("(:: %s ", stmt->init.name);
        ast_print_expr(stmt->init.expr);
        printf(")");
        break;
    case AST_STMT_DECL:
        printf("(var %s ", stmt->decl.name);
        ast_print_typespec(stmt->decl.type);
        if (stmt->decl.expr) {
            printf(" ");
            ast_print_expr(stmt->decl.expr);
        }
        printf(")");
        break;
    case AST_STMT_EXPR:
        ast_print_expr(stmt->expr);
        break;
    default:
        assert(0);
        break;
    }
}

#endif
