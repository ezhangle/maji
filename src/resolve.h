#ifndef RESOLVE_H
#define RESOLVE_H

struct type_field;
struct type;
struct symbol;
struct resolved_expr;
struct resolver;

enum type_kind
{
    TYPE_NONE,
    TYPE_INCOMPLETE,
    TYPE_COMPLETING,
    TYPE_VOID,
    TYPE_INT8,
    TYPE_INT16,
    TYPE_INT32,
    TYPE_INT64,
    TYPE_FLOAT32,
    TYPE_FLOAT64,
    TYPE_ARRAY,
    TYPE_ENUM,
    TYPE_PTR,
    TYPE_STRUCT,
    TYPE_FUNC
};

struct type
{
    enum type_kind kind;
    size_t size;
    size_t align;
    bool is_unsigned;
    struct symbol *symbol;
    union {
        struct {
            struct type *elem;
        } ptr;
        struct {
            struct type *elem;
            size_t size;
        } array;
        struct {
            struct type_field *fields;
            size_t fields_count;
        } aggregate;
        struct {
            struct type **params;
            size_t params_count;
            struct type *ret;
        } func;
    };
};

struct type_field
{
    const uint8_t *name;
    struct type *type;
};

enum symbol_kind
{
    SYMBOL_NONE,
    SYMBOL_VAR,
    SYMBOL_CONST,
    SYMBOL_FUNC,
    SYMBOL_FUNC_FOREIGN,
    SYMBOL_TYPE,
};

enum symbol_state
{
    SYMBOL_UNRESOLVED,
    SYMBOL_RESOLVING,
    SYMBOL_RESOLVED
};

#define MAX_LOCAL_SYMBOLS 256

struct scope
{
    struct scope *prev_scope;
    struct symbol **symbols;
};

struct symbol
{
    const uint8_t *name;
    enum symbol_kind kind;
    enum symbol_state state;
    struct ast_decl *decl;
    struct type *type;
    int64_t val;
    struct scope *scope;
    uint64_t address;
};

struct resolved_expr
{
    struct type *type;
    bool is_lvalue;
    bool is_const;
    int64_t val;
};

struct cached_ptr_type
{
    struct type *elem;
    struct type *ptr;
};

struct cached_array_type
{
    struct type *elem;
    size_t size;
    struct type *array;
};

struct cached_func_type
{
    struct type **params;
    size_t params_count;
    struct type *ret;
    struct type *func;
};

struct resolver
{
    struct ast_decl **decls;
    struct symbol **symbols;
    struct symbol **ordered_symbols;
    struct scope *global_scope;
    struct scope *current_scope;
    uint64_t locals_address;
    struct parser parser;
    uint8_t **files;
};

#endif
