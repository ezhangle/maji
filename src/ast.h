#ifndef MAJI_AST_H
#define MAJI_AST_H

struct ast_expr;
struct ast_stmt;
struct ast_decl;
struct ast_typespec;

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

enum ast_decl_kind
{
    AST_DECL_ENUM,
    AST_DECL_STRUCT,
    AST_DECL_UNION,
    AST_DECL_VAR,
    AST_DECL_CONST,
    AST_DECL_FUNC
};

enum ast_expr_kind
{
    AST_EXPR_IDENTIFIER,
    AST_EXPR_INT_LITERAL,
    AST_EXPR_STRING_LITERAL,

    AST_EXPR_CAST,

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

#endif
