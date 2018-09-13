#ifndef MAJI_AST_H
#define MAJI_AST_H

#include <assert.h>
#include <inttypes.h>

struct ast_expr;
struct ast_stmt;
struct ast_decl;
struct ast_typespec;

static inline void ast_print_typespec(struct ast_typespec *typespec);
static inline void ast_print_expr(struct ast_expr *typespec);

enum ast_typespec_kind
{
    AST_TYPESPEC_NONE,
    AST_TYPESPEC_IDENTIFIER,
    AST_TYPESPEC_FUNC,
    AST_TYPESPEC_ARRAY,
    AST_TYPESPEC_POINTER,
};

struct ast_typespec
{
    enum ast_typespec_kind kind;
    union {
        const uint8_t *name;
        struct {
            struct ast_typespec *base;
            struct ast_expr *size;
        };
    };
};

static inline struct ast_typespec *
ast_typespec_alloc(enum ast_typespec_kind kind)
{
    struct ast_typespec *typespec = malloc(sizeof(struct ast_typespec));
    memset(typespec, 0, sizeof(struct ast_typespec));
    typespec->kind = kind;
    return typespec;
}

static inline struct ast_typespec *
ast_typespec_identifier(const uint8_t *name)
{
    struct ast_typespec *typespec = ast_typespec_alloc(AST_TYPESPEC_IDENTIFIER);
    typespec->name = name;
    return typespec;
}

static inline struct ast_typespec *
ast_typespec_array(struct ast_typespec *base, struct ast_expr *size)
{
    struct ast_typespec *typespec = ast_typespec_alloc(AST_TYPESPEC_ARRAY);
    typespec->base = base;
    typespec->size = size;
    return typespec;
}

static inline struct ast_typespec *
ast_typespec_pointer(struct ast_typespec *base)
{
    struct ast_typespec *typespec = ast_typespec_alloc(AST_TYPESPEC_POINTER);
    typespec->base = base;
    return typespec;
}

enum ast_decl_kind
{
    AST_DECL_NONE,
    AST_DECL_ENUM,
    AST_DECL_STRUCT,
    AST_DECL_UNION,
    AST_DECL_VAR,
    AST_DECL_CONST,
    AST_DECL_FUNC
};

enum ast_expr_kind
{
    AST_EXPR_NONE,

    AST_EXPR_IDENTIFIER,
    AST_EXPR_INT_LITERAL,
    AST_EXPR_STRING_LITERAL,

    AST_EXPR_CAST,
    AST_EXPR_INDEX,
    AST_EXPR_FIELD,

    AST_EXPR_UNARY,
    AST_EXPR_BINARY,
    AST_EXPR_TERNARY,
};

enum ast_stmt_kind
{
    AST_STMT_NONE,
    AST_STMT_RETURN,
    AST_STMT_BREAK,
    AST_STMT_CONTINUE,
    AST_STMT_IF,
    AST_STMT_WHILE,
    AST_STMT_FOR,
    AST_STMT_ASSIGN,
    AST_STMT_INFER_ASSIGN,
    AST_STMT_BLOCK,
    AST_STMT_EXPR,
};

struct ast_enum_item_decl
{
    const uint8_t *name;
    struct ast_typespec *typespec;
    struct ast_enum_item_decl *prev;
    struct ast_enum_item_decl *next;
};

struct ast_struct_item_decl
{
    const uint8_t *name;
    struct ast_typespec *typespec;
    struct ast_struct_item_decl *prev;
    struct ast_struct_item_decl *next;
};

struct ast_union_item_decl
{
    const uint8_t *name;
    struct ast_typespec *typespec;
    struct ast_union_item_decl *prev;
    struct ast_union_item_decl *next;
};

struct ast_decl
{
    enum ast_decl_kind kind;
    const uint8_t *name;
    union {
        struct ast_enum_item_decl *enum_items;
        struct ast_struct_item_decl *struct_items;
        struct {
            struct ast_typespec *typespec;
            struct ast_expr *expr;
        };
    };
};

struct ast_expr
{
    enum ast_expr_kind kind;
    enum token_kind op;

    union {
        uint64_t int_val;
        double float_val;
        const uint8_t *string_val;
        const uint8_t *name;
        struct {
            struct ast_typespec *cast_typespec;
            struct ast_expr *cast_expr;
        };
        struct {
            struct ast_expr *operand;
            struct ast_expr *index;
            const uint8_t *field;
        };
        struct {
            struct ast_expr *left_expr;
            struct ast_expr *right_expr;
        };
        struct {
            struct ast_expr *condition;
            struct ast_expr *then_expr;
            struct ast_expr *else_expr;
        };
    };
};

struct ast_stmt
{
    enum ast_stmt_kind kind;
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
ast_expr_cast(struct ast_typespec *typespec, struct ast_expr *expr)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_CAST);
    result->cast_typespec = typespec;
    result->cast_expr = expr;
    return result;
}

static inline struct ast_expr *
ast_expr_index(struct ast_expr *operand, struct ast_expr *index)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_INDEX);
    result->operand = operand;
    result->index = index;
    return result;
}


static inline struct ast_expr *
ast_expr_field(struct ast_expr *operand, const uint8_t *field)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_FIELD);
    result->operand = operand;
    result->field = field;
    return result;
}

static inline struct ast_expr *
ast_expr_unary(enum token_kind op, struct ast_expr *expr)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_UNARY);
    result->op = op;
    result->operand = expr;
    return result;
}

static inline struct ast_expr *
ast_expr_binary(enum token_kind op, struct ast_expr *left_expr, struct ast_expr *right_expr)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_BINARY);
    result->op = op;
    result->left_expr = left_expr;
    result->right_expr = right_expr;
    return result;
}

static inline struct ast_expr *
ast_expr_ternary(struct ast_expr *condition, struct ast_expr *then_expr, struct ast_expr *else_expr)
{
    struct ast_expr *result = ast_expr_alloc(AST_EXPR_TERNARY);
    result->condition = condition;
    result->then_expr = then_expr;
    result->else_expr = else_expr;
    return result;
}

static inline void
ast_print_typespec(struct ast_typespec *typespec)
{
    switch (typespec->kind) {
    case AST_TYPESPEC_IDENTIFIER:
        printf("%s", typespec->name);
        break;
    case AST_TYPESPEC_ARRAY:
        printf("(array ");
        ast_print_typespec(typespec->base);
        printf(" ");
        ast_print_expr(typespec->size);
        printf(")");
        break;
    case AST_TYPESPEC_POINTER:
        printf("(ptr ");
        ast_print_typespec(typespec->base);
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
    case AST_EXPR_STRING_LITERAL:
        printf("\"%s\"", expr->string_val);
        break;
    case AST_EXPR_IDENTIFIER:
        printf("%s", expr->name);
        break;
    case AST_EXPR_CAST:
        printf("(cast ");
        ast_print_typespec(expr->cast_typespec);
        printf(" ");
        ast_print_expr(expr->cast_expr);
        printf(")");
        break;
    case AST_EXPR_INDEX:
        printf("(index ");
        ast_print_expr(expr->operand);
        printf(" ");
        ast_print_expr(expr->index);
        printf(")");
        break;
    case AST_EXPR_FIELD:
        printf("(field ");
        ast_print_expr(expr->operand);
        printf(" %s)", expr->field);
        break;
    case AST_EXPR_UNARY:
        printf("(%c ", expr->op);
        ast_print_expr(expr->operand);
        printf(")");
        break;
    case AST_EXPR_BINARY:
        printf("(%c ", expr->op);
        ast_print_expr(expr->left_expr);
        printf(" ");
        ast_print_expr(expr->right_expr);
        printf(")");
        break;
    case AST_EXPR_TERNARY:
        printf("(if ");
        ast_print_expr(expr->condition);
        printf(" ");
        ast_print_expr(expr->then_expr);
        printf(" ");
        ast_print_expr(expr->else_expr);
        printf(")");
        break;
    default:
        assert(0);
        break;
    }
}

#endif
