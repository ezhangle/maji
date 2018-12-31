#include "resolve.h"
#include "ast.h"

#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include <stddef.h>

#define IS_POW2(x) (((x) != 0) && ((x) & ((x)-1)) == 0)
#define ALIGN_DOWN(n, a) ((n) & ~((a) - 1))
#define ALIGN_UP(n, a) ALIGN_DOWN((n) + (a) - 1, (a))
#define ALIGN_DOWN_PTR(p, a) ((void *)ALIGN_DOWN((uintptr_t)(p), (a)))
#define ALIGN_UP_PTR(p, a) ((void *)ALIGN_UP((uintptr_t)(p), (a)))

struct resolved_expr resolve_expr(struct resolver *resolver, struct ast_expr *expr);
struct resolved_expr resolve_expected_expr(struct resolver *resolver, struct ast_expr *expr, struct type *type);
void resolve_symbol(struct resolver *resolver, struct symbol *symbol);
struct type *resolve_typespec(struct resolver *resolver, struct ast_typespec *typespec);
int64_t resolve_const_expr(struct resolver *resolver, struct ast_expr *expr);
void resolve_statement(struct resolver *resolver, struct ast_stmt *statement, struct type *ret_type);
void complete_type(struct resolver *resolver, struct type *type);

struct type *type_variadic = &(struct type){TYPE_VOID, 0, 0, false};

struct type *type_void  = &(struct type){TYPE_VOID,    0, 0, false};
struct type *type_char  = &(struct type){TYPE_INT8,    1, 1, false};
struct type *type_int   = &(struct type){TYPE_INT32,   4, 4, false};
struct type *type_float = &(struct type){TYPE_FLOAT32, 4, 4, false};

struct type *type_int8   = &(struct type){TYPE_INT8,  1, 1, false};
struct type *type_int16  = &(struct type){TYPE_INT16, 2, 2, false};
struct type *type_int32  = &(struct type){TYPE_INT32, 4, 4, false};
struct type *type_int64  = &(struct type){TYPE_INT64, 8, 8, false};

struct type *type_uint8  = &(struct type){TYPE_INT8,  1, 1, true};
struct type *type_uint16 = &(struct type){TYPE_INT16, 2, 2, true};
struct type *type_uint32 = &(struct type){TYPE_INT32, 4, 4, true};
struct type *type_uint64 = &(struct type){TYPE_INT64, 8, 8, true};

struct type *type_float32 = &(struct type){TYPE_FLOAT32, 4, 4, false};
struct type *type_float64 = &(struct type){TYPE_FLOAT64, 8, 8, false};

const size_t PTR_SIZE  = 8;
const size_t PTR_ALIGN = 8;

void *memdup(void *src, size_t size)
{
    void *dest = malloc(size);
    memcpy(dest, src, size);
    return dest;
}

struct type *type_alloc(enum type_kind kind)
{
    struct type *type = malloc(sizeof(struct type));
    memset(type, 0, sizeof(struct type));
    type->kind = kind;
    return type;
}

struct type *type_incomplete(struct symbol *symbol)
{
    struct type *type = type_alloc(TYPE_INCOMPLETE);
    type->symbol = symbol;
    return type;
}

struct type *type_copy(struct type *type)
{
    struct type *result = malloc(sizeof(struct type));
    memcpy(result, type, sizeof(struct type));
    return result;
}

size_t type_sizeof(struct type *type)
{
    assert(type->kind > TYPE_COMPLETING);
    assert(type->size != 0);
    return type->size;
}

size_t type_alignof(struct type *type)
{
    assert(type->kind > TYPE_COMPLETING);
    assert(IS_POW2(type->align));
    return type->align;
}

struct cached_ptr_type *cached_ptr_types;
struct type *type_ptr(struct type *elem)
{
    for (int i = 0; i < buf_len(cached_ptr_types); ++i) {
        struct cached_ptr_type *it = cached_ptr_types + i;
        if (it->elem == elem) {
            return it->ptr;
        }
    }
    struct type *type = type_alloc(TYPE_PTR);
    type->size = PTR_SIZE;
    type->align = PTR_ALIGN;
    type->ptr.elem = elem;

    buf_push(cached_ptr_types, ((struct cached_ptr_type) {
        .elem = elem,
        .ptr = type
    }));

    return type;
}

struct cached_array_type *cached_array_types;
struct type *type_array(struct resolver *resolver, struct type *elem, size_t size)
{
    for (int i = 0; i < buf_len(cached_array_types); ++i) {
        struct cached_array_type *it = cached_array_types + i;
        if (it->elem == elem && it->size == size) {
            return it->array;
        }
    }
    complete_type(resolver, elem);
    struct type *type = type_alloc(TYPE_ARRAY);
    type->size = size * type_sizeof(elem);
    type->align = type_alignof(elem);
    type->array.elem = elem;
    type->array.size = size;

    buf_push(cached_array_types, ((struct cached_array_type) {
        .elem = elem,
        .size = size,
        .array = type
    }));

    return type;
}

struct cached_func_type *cached_func_types;
struct type *type_func(struct type **params, size_t params_count, struct type *ret, bool is_variadic)
{
    for (int i = 0; i < buf_len(cached_func_types); ++i) {
        struct cached_func_type *it = cached_func_types + i;
        if (it->params_count == params_count && it->ret == ret) {
            bool match = true;
            for (size_t j = 0; j < params_count; ++j) {
                if (it->params[j] != params[j]) {
                    match = false;
                    break;
                }
            }
            if (match) {
                return it->func;
            }
        }
    }

    struct type *type = type_alloc(TYPE_FUNC);
    type->size = PTR_SIZE;
    type->align = PTR_ALIGN;
    type->func.params = memdup(params, params_count * sizeof(*params));
    type->func.params_count = params_count;
    type->func.ret = ret;
    type->func.is_variadic = is_variadic;

    buf_push(cached_func_types, ((struct cached_func_type) {
        .params = params,
        .params_count = params_count,
        .ret = ret,
        .func = type
    }));

    return type;
}

bool is_ptrtype(struct type *type)
{
    return type->kind == TYPE_PTR;
}

bool is_inttype(struct type *type)
{
    switch (type->kind) {
    case TYPE_INT8:
    case TYPE_INT16:
    case TYPE_INT32:
    case TYPE_INT64:
    case TYPE_ENUM:
        return true;
    default:
        return false;
    }
}

bool is_floattype(struct type *type)
{
    return type->kind == TYPE_FLOAT32 || type->kind == TYPE_FLOAT64;
}

bool is_arithmetic_type(struct type *type)
{
    return is_inttype(type) || is_floattype(type);
}

struct type *arithmetic_type_conversion(struct type *t, struct type *u)
{
    assert(is_arithmetic_type(t));
    assert(is_arithmetic_type(u));

    if (t->kind < u->kind) {
        struct type *tmp = t;
        t = u;
        u = tmp;
    }

    if (is_floattype(t)) {
        return t;
    }

    assert(is_inttype(t) && t->size >= type_int8->size);
    assert(is_inttype(u) && u->size >= type_int8->size);

    if (t->size > u->size) {
        return t;
    }

    assert(t->size == u->size);
    if (t->is_unsigned == u->is_unsigned) {
        return t;
    }

    struct type *r = type_copy(t);
    r->is_unsigned = true;
    return r;
}

bool duplicate_fields(struct type_field *fields, size_t fields_count)
{
    for (size_t i = 0; i < fields_count; ++i) {
        for (size_t j = i + 1; j < fields_count; ++j) {
            if (fields[i].name == fields[j].name) {
                return true;
            }
        }
    }
    return false;
}

struct symbol *symbol_new(enum symbol_kind kind, const uint8_t *name, struct ast_decl *decl)
{
    struct symbol *symbol = malloc(sizeof(struct symbol));
    memset(symbol, 0, sizeof(struct symbol));
    symbol->kind = kind;
    symbol->name = name;
    symbol->decl = decl;
    return symbol;
}

struct symbol *symbol_var(const uint8_t *name, struct type *type)
{
    struct symbol *symbol = symbol_new(SYMBOL_VAR, name, NULL);
    symbol->state = SYMBOL_RESOLVED;
    symbol->type = type;
    return symbol;
}

struct symbol *symbol_const(const uint8_t *name, struct type *type)
{
    struct symbol *symbol = symbol_new(SYMBOL_CONST, name, NULL);
    symbol->state = SYMBOL_RESOLVED;
    symbol->type = type;
    return symbol;
}

struct scope *scope_new(struct resolver *resolver)
{
    struct scope *scope = malloc(sizeof(struct scope));
    memset(scope, 0, sizeof(struct scope));
    scope->prev_scope = resolver->current_scope;
    return scope;
}

struct symbol *symbol_get(struct resolver *resolver, const uint8_t *name)
{
    for (struct scope *scope = resolver->current_scope;
         scope != NULL;
         scope = scope->prev_scope) {
        for (int i = 0; i < buf_len(scope->symbols); ++i) {
            struct symbol *it = scope->symbols[i];
            if (it->name == name) {
                return it;
            }
        }
    }

    return NULL;
}

void symbol_push(struct resolver *resolver, struct symbol *symbol)
{
    for (int i = 0; i < buf_len(resolver->current_scope->symbols); ++i) {
        struct symbol *it = resolver->current_scope->symbols[i];
        if (it->name == symbol->name) {
            // TODO: error handling
            printf("duplicate identifier '%s'\n", symbol->name);
            exit(1);
        }
    }

    symbol->scope = resolver->current_scope;
    buf_push(resolver->current_scope->symbols, symbol);
}

struct scope *symbol_enter(struct resolver *resolver)
{
    struct scope *scope = scope_new(resolver);
    scope->prev_scope = resolver->current_scope;
    resolver->current_scope = scope;
    return scope;
}

struct scope *symbol_leave(struct resolver *resolver)
{
    resolver->current_scope = resolver->current_scope->prev_scope;
    return resolver->current_scope;
}

void complete_type_struct(struct resolver *resolver, struct ast_decl *decl, struct type *type)
{
    assert(type->kind == TYPE_COMPLETING);

    struct type_field *fields = NULL;
    for (size_t i = 0; i < decl->struct_decl.items_count; ++i) {
        struct ast_struct_item item = decl->struct_decl.items[i];
        struct type *item_type = resolve_typespec(resolver, item.type);
        complete_type(resolver, item_type);

        if (item.expr) {
            struct resolved_expr rexpr = resolve_expected_expr(resolver, item.expr, item_type);
            if (!rexpr.is_const) {
                // TODO: error handling
                printf("struct init value must be const!\n");
                exit(1);
            }

            if (!is_arithmetic_type(rexpr.type)) {
                // TODO: error handling
                printf("default struct value can only be of type char, int or float\n");
                exit(1);
            }

            if (item_type->kind == TYPE_STRUCT) {
                // TODO: error handling
                printf("default struct value is not allowed for fields of struct-type\n");
                exit(1);
            }
        }

        buf_push(fields, ((struct type_field) {
            .name = item.name,
            .type = item_type,
            .expr = item.expr
        }));
    }

    int fields_count = buf_len(fields);

    if (fields_count == 0) {
        // TODO: error handling
        printf("no fields!\n");
        exit(1);
    }

    if (duplicate_fields(fields, fields_count)) {
        // TODO: error handling
        printf("duplicate fields!\n");
        exit(1);
    }

    type->kind = TYPE_STRUCT;
    type->size = 0;
    type->align = 0;

    for (struct type_field *it = fields; it != fields + fields_count; ++it) {
        type->size = type_sizeof(it->type) + ALIGN_UP(type->size, type_alignof(it->type));
        type->align = MAX(type->align, type_alignof(it->type));
    }

    type->aggregate.fields = memdup(fields, fields_count * sizeof(*fields));
    type->aggregate.fields_count = fields_count;
}

void complete_type_enum(struct resolver *resolver, struct ast_decl *decl, struct type *type)
{
    assert(type->kind == TYPE_COMPLETING);

    struct type_field *fields = NULL;
    for (size_t i = 0; i < decl->enum_decl.items_count; ++i) {
        struct ast_enum_item item = decl->enum_decl.items[i];
        struct type *item_type = type_int;

        if (item.expr) {
            struct resolved_expr rexpr = resolve_expr(resolver, item.expr);
            if (!rexpr.is_const) {
                // TODO: error handling
                printf("enum value must be const!\n");
                exit(1);
            }

            if (!is_inttype(rexpr.type)) {
                // TODO: error handling
                printf("enum value must be int!\n");
                exit(1);
            }

            item_type = rexpr.type;
        }

        complete_type(resolver, item_type);
        buf_push(fields, ((struct type_field) {
            .name = item.name,
            .type = item_type,
            .expr = item.expr
        }));
    }

    int fields_count = buf_len(fields);

    if (fields_count == 0) {
        // TODO: error handling
        printf("no fields!\n");
        exit(1);
    }

    if (duplicate_fields(fields, fields_count)) {
        // TODO: error handling
        printf("duplicate fields!\n");
        exit(1);
    }

    type->kind = TYPE_ENUM;
    type->size = type_sizeof(type_int);
    type->align = type_alignof(type_int);
    type->aggregate.fields = memdup(fields, fields_count * sizeof(*fields));
    type->aggregate.fields_count = fields_count;
}

void complete_type(struct resolver *resolver, struct type *type)
{
    if (type->kind == TYPE_COMPLETING) {
        // TODO: error handling
        printf("type dependency cycle in type '%s'\n", type->symbol->name);
        exit(1);
    }

    if (type->kind != TYPE_INCOMPLETE) {
        return;
    }

    type->kind = TYPE_COMPLETING;

    struct ast_decl *decl = type->symbol->decl;
    assert(decl->kind == AST_DECL_STRUCT || decl->kind == AST_DECL_ENUM);

    if (decl->kind == AST_DECL_ENUM) {
        complete_type_enum(resolver, decl, type);
    } else if (decl->kind == AST_DECL_STRUCT) {
        complete_type_struct(resolver, decl, type);
    }

    buf_push(resolver->ordered_symbols, type->symbol);
}

struct symbol *resolve_name(struct resolver *resolver, const uint8_t *name)
{
    struct symbol *symbol = symbol_get(resolver, name);
    if (!symbol) {
        // TODO: error handling
        printf("non-existant identifier '%s'\n", name);
        exit(1);
    }
    resolve_symbol(resolver, symbol);
    return symbol;
}

struct resolved_expr resolved_rvalue(struct type *type)
{
    return (struct resolved_expr){
        .type = type
    };
}

struct resolved_expr resolved_lvalue(struct type *type)
{
    return (struct resolved_expr){
        .type = type,
        .is_lvalue = true
    };
}

struct resolved_expr resolved_const(int64_t val, struct type *type)
{
    return (struct resolved_expr){
        .type = type,
        .is_const = true,
        .val = val
    };
}

struct type *resolve_typespec(struct resolver *resolver, struct ast_typespec *typespec)
{
    if (!typespec) {
        return type_void;
    }

    switch (typespec->kind) {
    case AST_TYPESPEC_IDENTIFIER: {
        struct symbol *symbol = resolve_name(resolver, typespec->name);
        if (symbol->kind != SYMBOL_TYPE) {
            // TODO: error handling
            printf("'%s' must denote a type", typespec->name);
            exit(1);
        }
        return symbol->type;
    }
    case AST_TYPESPEC_POINTER: {
        return type_ptr(resolve_typespec(resolver, typespec->ptr.elem));
    } break;
    case AST_TYPESPEC_ARRAY: {
        return type_array(resolver, resolve_typespec(resolver, typespec->array.elem), resolve_const_expr(resolver, typespec->array.size));
    }
    case AST_TYPESPEC_FUNC: {
        struct type **args = NULL;
        for (size_t i = 0; i < typespec->func.args_count; ++i) {
            buf_push(args, resolve_typespec(resolver, typespec->func.args[i]));
        }
        struct type *ret = type_void;
        if (typespec->func.ret) {
            ret = resolve_typespec(resolver, typespec->func.ret);
        }
        return type_func(args, buf_len(args), ret, false);
    }
    default: {
        assert(0);
        return NULL;
    }
    }
}

struct resolved_expr resolve_expr_identifier(struct resolver *resolver, struct ast_expr *expr)
{
    assert(expr->kind == AST_EXPR_IDENTIFIER);
    struct symbol *symbol = resolve_name(resolver, expr->name);

    expr->symbol = symbol;

    if (symbol->kind == SYMBOL_VAR) {
        return resolved_lvalue(symbol->type);
    } else if (symbol->kind == SYMBOL_CONST) {
        return resolved_const(symbol->val, symbol->type);
    } else if ((symbol->kind == SYMBOL_FUNC) ||
               (symbol->kind == SYMBOL_FUNC_FOREIGN)) {
        return resolved_rvalue(symbol->type);
    } else if (symbol->kind == SYMBOL_TYPE) {
        return resolved_rvalue(symbol->type);
    } else {
        // TODO: error handling
        printf("'%s' must be a var or const\n", expr->name);
        exit(1);
    }
}

struct resolved_expr ptr_decay(struct resolved_expr expr)
{
    if (expr.type->kind == TYPE_ARRAY) {
        return resolved_rvalue(type_ptr(expr.type->array.elem));
    } else {
        return expr;
    }
}

struct resolved_expr resolve_expr_call(struct resolver *resolver, struct ast_expr *expr)
{
    assert(expr->kind == AST_EXPR_CALL);
    struct resolved_expr func = resolve_expr(resolver, expr->call.expr);

    if (func.type->kind != TYPE_FUNC) {
        // TODO: error handling
        printf("trying to call non-function value");
        exit(1);
    }

    if (!func.type->func.is_variadic) {
        if (expr->call.args_count != func.type->func.params_count) {
            // TODO: error handling
            printf("tried to call function with wrong number of arguments");
            exit(1);
        }
    }

    for (size_t i = 0; i < expr->call.args_count; ++i) {
        struct resolved_expr arg = resolve_expr(resolver, expr->call.args[i]);

        if (i < func.type->func.params_count) {
            struct type *param_type = func.type->func.params[i];
            if (param_type == type_variadic) continue;

            struct type *arg_type = ptr_decay(arg).type;

            if (arg_type->kind == TYPE_PTR && param_type->kind == TYPE_PTR) {
                if (arg_type->ptr.elem == type_void || param_type->ptr.elem == type_void) {
                    continue;
                }
            }

            if (arg_type != param_type) {
                // TODO: error handling
                printf("call argument expression type doesn't match expected param type");
                exit(1);
            }
        }
    }

    return resolved_rvalue(func.type->func.ret);
}

struct resolved_expr resolve_expr_cast(struct resolver *resolver, struct ast_expr *expr)
{
    assert(expr->kind == AST_EXPR_CAST);
    struct type *cast_type = resolve_typespec(resolver, expr->cast.type);
    struct resolved_expr result = ptr_decay(resolve_expr(resolver, expr->cast.expr));
    if (is_ptrtype(cast_type)) {
        if (!is_ptrtype(result.type) && !is_inttype(result.type)) {
            // TODO: error handling
            printf("invalid cast to pointer type");
            exit(1);
        }
    } else if (is_inttype(cast_type)) {
        if (!is_ptrtype(result.type) && !is_arithmetic_type(result.type)) {
            // TODO: error handling
            printf("invalid cast to int type");
            exit(1);
        }
    } else if (!is_arithmetic_type(result.type) && !is_arithmetic_type(cast_type)) {
        // TODO: error handling
        printf("invalid target cast type");
        exit(1);
    }
    return resolved_rvalue(cast_type);
}

struct resolved_expr resolve_expr_sizeof_type(struct resolver *resolver, struct ast_expr *expr)
{
    assert(expr->kind == AST_EXPR_SIZEOF_TYPE);
    struct type *sizeof_type = resolve_typespec(resolver, expr->sizeof_type.type);
    complete_type(resolver, sizeof_type);
    return resolved_rvalue(type_int);
}

struct resolved_expr resolve_expr_offsetof(struct resolver *resolver, struct ast_expr *expr)
{
    assert(expr->kind == AST_EXPR_OFFSETOF);
    struct type *type = resolve_typespec(resolver, expr->offsetof.type);
    complete_type(resolver, type);
    if (type->kind != TYPE_STRUCT) {
        // TODO: error handling
        printf("offsetof can only be used with struct-types");
        exit(1);
    }

    if (expr->offsetof.expr->kind != AST_EXPR_IDENTIFIER) {
        // TODO: error handling
        printf("offsetof expression is not a valid identifier");
        exit(1);
    }

    for (size_t i = 0; i < type->aggregate.fields_count; ++i) {
        struct type_field field = type->aggregate.fields[i];
        if (field.name == expr->offsetof.expr->name) {
            return resolved_rvalue(type_int);
        }
    }

    printf("offsetof no field named '%s' in specified type", expr->offsetof.expr->name);
    exit(1);
}

struct resolved_expr resolve_expr_index(struct resolver *resolver, struct ast_expr *expr)
{
    assert(expr->kind == AST_EXPR_INDEX);
    struct resolved_expr operand = ptr_decay(resolve_expr(resolver, expr->index.expr));
    if (operand.type->kind != TYPE_PTR) {
        // TODO: error handling
        printf("can only index arrays or pointers");
        exit(1);
    }
    struct resolved_expr index = resolve_expr(resolver, expr->index.index);
    if (!is_inttype(index.type)) {
        // TODO: error handling
        printf("index expression must have type int");
        exit(1);
    }
    return resolved_lvalue(operand.type->ptr.elem);
}

struct resolved_expr resolve_expr_field(struct resolver *resolver, struct ast_expr *expr)
{
    assert(expr->kind == AST_EXPR_FIELD);
    struct resolved_expr left = resolve_expr(resolver, expr->field.expr);
    struct type *type = left.type;
    complete_type(resolver, type);

    while (is_ptrtype(type)) {
        type = type->ptr.elem;
        complete_type(resolver, type);
    }

    if (type->kind != TYPE_STRUCT && type->kind != TYPE_ENUM) {
        // TODO: error handling
        printf("can only access fields on aggregate types");
        exit(1);
    }

    for (size_t i = 0; i < type->aggregate.fields_count; ++i) {
        struct type_field field = type->aggregate.fields[i];
        if (field.name == expr->field.name) {
            if (type->kind == TYPE_STRUCT) {
                return left.is_lvalue ? resolved_lvalue(field.type) : resolved_rvalue(field.type);
            } else if (type->kind == TYPE_ENUM) {
                return resolved_rvalue(type);
            }
        }
    }

    // TODO: error handling
    printf("no field named '%s'", expr->field.name);
    exit(1);
}

int64_t eval_int_unary(enum token_kind op, int64_t val)
{
    switch (op) {
    case '+': return +val;
    case '-': return -val;
    case '~': return ~val;
    case '!': return !val;
    default: {
        assert(0);
        return 0;
    } break;
    }
}

double eval_float_unary(enum token_kind op, double val)
{
    switch (op) {
    case '+': return +val;
    case '-': return -val;
    case '!': return !val;
    case '~': {
        printf("invalid argument to unary operator '~'\n");
        exit(1);
    } break;
    default: {
        assert(0);
        return 0;
    } break;
    }
}

struct resolved_expr resolve_expr_unary(struct resolver *resolver, struct ast_expr *expr)
{
    assert(expr->kind == AST_EXPR_UNARY);
    struct resolved_expr operand = resolve_expr(resolver, expr->unary.expr);
    struct type *type = operand.type;
    switch (expr->unary.op) {
    case '*':
        operand = ptr_decay(operand);
        if (type->kind != TYPE_PTR) {
            // TODO: error handling
            printf("cannot deref non-ptr type");
            exit(1);
        }
        return resolved_lvalue(type->ptr.elem);
    case '&':
        if (!operand.is_lvalue) {
            // TODO: error handling
            printf("cannot take address of non-lvalue");
            exit(1);
        }
        return resolved_rvalue(type_ptr(type));
    default:
        if (!is_arithmetic_type(type)) {
            // TODO: error handling
            printf("unary operand must be int or float\n");
            exit(1);
        }

        if (operand.is_const) {
            if (is_floattype(type)) {
                double operand_val = (double)(*(double *)&operand.val);
                double float_val = eval_float_unary(expr->unary.op, operand_val);
                return resolved_const((int64_t)(*(int64_t *)&float_val), type);
            }

            if (is_inttype(type)) {
                return resolved_const(eval_int_unary(expr->unary.op, operand.val), type);
            }
        }

        return resolved_rvalue(type);
    }
}

int64_t eval_int_binary(enum token_kind op, int64_t left, int64_t right)
{
    switch (op) {
    case '*':                  return left * right;
    case '/':                  return right != 0 ? left / right : 0;
    case '%':                  return right != 0 ? left % right : 0;
    case '&':                  return left & right;
    case TOKEN_KIND_LSHIFT:    return left << right;
    case TOKEN_KIND_RSHIFT:    return left >> right;
    case '+':                  return left + right;
    case '-':                  return left - right;
    case '|':                  return left | right;
    case '^':                  return left ^ right;
    case TOKEN_KIND_EQUAL:     return left == right;
    case TOKEN_KIND_NOT_EQUAL: return left != right;
    case '<':                  return left < right;
    case TOKEN_KIND_LT_EQUAL:  return left <= right;
    case '>':                  return left > right;
    case TOKEN_KIND_GT_EQUAL:  return left >= right;
    case TOKEN_KIND_AND:       return left && right;
    case TOKEN_KIND_OR:        return left || right;
    default: {
        assert(0);
        return 0;
    } break;
    }
}

double eval_float_binary(enum token_kind op, double left, double right)
{
    switch (op) {
    case '*':                  return left * right;
    case '/':                  return right != 0 ? left / right : 0;
    case '+':                  return left + right;
    case '-':                  return left - right;
    case TOKEN_KIND_EQUAL:     return left == right;
    case TOKEN_KIND_NOT_EQUAL: return left != right;
    case '<':                  return left < right;
    case TOKEN_KIND_LT_EQUAL:  return left <= right;
    case '>':                  return left > right;
    case TOKEN_KIND_GT_EQUAL:  return left >= right;
    case TOKEN_KIND_AND:       return left && right;
    case TOKEN_KIND_OR:        return left || right;
    case '%': {
        printf("invalid operands to binary operator '%%'\n");
        exit(1);
    } break;
    case '&': {
        printf("invalid operands to binary operator '&'\n");
        exit(1);
    } break;
    case TOKEN_KIND_LSHIFT: {
        printf("invalid operands to binary operator '<<'\n");
        exit(1);
    } break;
    case TOKEN_KIND_RSHIFT: {
        printf("invalid operands to binary operator '>>'\n");
        exit(1);
    } break;
    case '|': {
        printf("invalid operands to binary operator '|'\n");
        exit(1);
    } break;
    case '^': {
        printf("invalid operands to binary operator '^'\n");
        exit(1);
    } break;
    default: {
        assert(0);
        return 0;
    } break;
    }
}

struct resolved_expr resolve_expr_binary(struct resolver *resolver, struct ast_expr *expr)
{
    assert(expr->kind == AST_EXPR_BINARY);
    struct resolved_expr left = resolve_expr(resolver, expr->binary.left_expr);
    struct resolved_expr right = resolve_expr(resolver, expr->binary.right_expr);

    if (is_ptrtype(left.type) && is_ptrtype(right.type)) {
        return resolved_rvalue(left.type);
    }

    if (!is_arithmetic_type(left.type) || !is_arithmetic_type(right.type)) {
        // TODO: error handling
        printf("binary operand must be char, int or float");
        exit(1);
    }

    struct type *result_type = arithmetic_type_conversion(left.type, right.type);

    if (!left.is_const || !right.is_const) {
        return resolved_rvalue(result_type);
    }

    if (is_floattype(result_type)) {
        double left_val = is_inttype(left.type) ? left.val : (double)(*(double *)&left.val);
        double right_val = is_inttype(right.type) ? right.val : (double)(*(double *)&right.val);
        double float_val = eval_float_binary(expr->binary.op, left_val, right_val);
        printf("float val is %f\n", float_val);
        return resolved_const((int64_t)(*(int64_t *)&float_val), result_type);
    }

    return resolved_const(eval_int_binary(expr->binary.op, left.val, right.val), result_type);
}

struct resolved_expr resolve_expr_ternary(struct resolver *resolver, struct ast_expr *expr, struct type *expected_type)
{
    assert(expr->kind == AST_EXPR_TERNARY);
    struct resolved_expr cond = ptr_decay(resolve_expr(resolver, expr->ternary.condition));
    if (!is_inttype(cond.type) && !is_ptrtype(cond.type)) {
        // TODO: error handling
        printf("ternary cond expression must have type int or ptr");
        exit(1);
    }
    struct resolved_expr then_expr = ptr_decay(resolve_expected_expr(resolver, expr->ternary.then_expr, expected_type));
    struct resolved_expr else_expr = ptr_decay(resolve_expected_expr(resolver, expr->ternary.else_expr, expected_type));
    if (then_expr.type != else_expr.type) {
        // TODO: error handling
        printf("ternary then/else expressions must have matching types");
        exit(1);
    }
    if (cond.is_const && then_expr.is_const && else_expr.is_const) {
        return resolved_const(cond.val ? then_expr.val : else_expr.val, type_int);
    } else {
        return resolved_rvalue(then_expr.type);
    }
}

struct resolved_expr resolve_expected_expr(struct resolver *resolver, struct ast_expr *expr, struct type *expected_type)
{
    struct resolved_expr res = {};

    switch (expr->kind) {
    case AST_EXPR_IDENTIFIER: {
        res = resolve_expr_identifier(resolver, expr);
    } break;
    case AST_EXPR_INT_LITERAL: {
        struct type *type = NULL;
        if (expr->int_literal.base == NUMBER_BASE_DECIMAL) {
            type = !(expr->int_literal.val & ~(long)INT_MAX) ? type_int : type_int64;
        } else {
            type = !(expr->int_literal.val & ~(unsigned long)INT_MAX)
                 ? type_int
                 : !(expr->int_literal.val & ~(unsigned long)UINT_MAX)
                 ? type_uint32
                 : !(expr->int_literal.val & ~(unsigned long)LONG_MAX)
                 ? type_int64
                 : type_uint64;
        }
        res = resolved_const(expr->int_literal.val, type);
    } break;
    case AST_EXPR_CHAR_LITERAL: {
        res = resolved_const(expr->int_literal.val, type_char);
    } break;
    case AST_EXPR_FLOAT_LITERAL: {
        res = resolved_const((int64_t)(*(int64_t *)&expr->float_val), type_float64);
    } break;
    case AST_EXPR_STRING_LITERAL: {
        res = resolved_const(0, type_ptr(type_char));
    } break;
    case AST_EXPR_NULL_LITERAL: {
        res = resolved_const(0, type_ptr(type_void));
    } break;
    case AST_EXPR_CALL: {
        res = resolve_expr_call(resolver, expr);
    } break;
    case AST_EXPR_CAST: {
        res = resolve_expr_cast(resolver, expr);
    } break;
    case AST_EXPR_SIZEOF_TYPE: {
        res = resolve_expr_sizeof_type(resolver, expr);
    } break;
    case AST_EXPR_OFFSETOF: {
        res = resolve_expr_offsetof(resolver, expr);
    } break;
    case AST_EXPR_INDEX: {
        res = resolve_expr_index(resolver, expr);
    } break;
    case AST_EXPR_FIELD: {
        res = resolve_expr_field(resolver, expr);
    } break;
    case AST_EXPR_UNARY: {
        res = resolve_expr_unary(resolver, expr);
    } break;
    case AST_EXPR_BINARY: {
        res = resolve_expr_binary(resolver, expr);
    } break;
    case AST_EXPR_TERNARY: {
        res = resolve_expr_ternary(resolver, expr, expected_type);
    } break;
    default: {
        assert(0);
    } break;
    }

    expr->res = res;
    return res;
}

struct resolved_expr resolve_expr(struct resolver *resolver, struct ast_expr *expr)
{
    return resolve_expected_expr(resolver, expr, NULL);
}

int64_t resolve_const_expr(struct resolver *resolver, struct ast_expr *expr)
{
    struct resolved_expr result = resolve_expr(resolver, expr);
    if (!result.is_const) {
        // TODO: error handling
        printf("expected constant expression");
        exit(1);
    }
    return result.val;
}

void resolve_cond_expr(struct resolver *resolver, struct ast_expr *expr)
{
    struct resolved_expr result = resolve_expr(resolver, expr);
    if (!is_inttype(result.type)) {
        // TODO: error handling
        printf("conditional expressions must be of type int!\n");
        exit(1);
    }
}

void resolve_statement_block(struct resolver *resolver, struct ast_stmt_block block, struct type *ret_type)
{
    symbol_enter(resolver);
    for (size_t i = 0; i < block.statements_count; ++i) {
        resolve_statement(resolver, block.statements[i], ret_type);
    }
    symbol_leave(resolver);
}

void resolve_statement(struct resolver *resolver, struct ast_stmt *statement, struct type *ret_type)
{
    switch (statement->kind) {
    case AST_STMT_RETURN: {
        struct type *return_stmt_type = statement->return_stmt.expr
                                      ? resolve_expected_expr(resolver, statement->return_stmt.expr, ret_type).type
                                      : type_void;
        if (return_stmt_type != ret_type) {
            // TODO: error handling
            printf("return type mismatch! returned %s but expected %s\n", return_stmt_type->symbol->name, ret_type->symbol->name);
            exit(1);
        }
    } break;
    case AST_STMT_BREAK:
    case AST_STMT_CONTINUE: {
        // do nothing
    } break;
    case AST_STMT_BLOCK: {
        resolve_statement_block(resolver, statement->block, ret_type);
    } break;
    case AST_STMT_IF: {
        resolve_cond_expr(resolver, statement->if_stmt.condition);
        resolve_statement_block(resolver, statement->if_stmt.then_block, ret_type);
        for (size_t i = 0; i < statement->if_stmt.else_ifs_count; ++i) {
            struct ast_else_if else_if = statement->if_stmt.else_ifs[i];
            resolve_cond_expr(resolver, else_if.condition);
            resolve_statement_block(resolver, else_if.block, ret_type);
        }
        if (statement->if_stmt.else_block.statements) {
            resolve_statement_block(resolver, statement->if_stmt.else_block, ret_type);
        }
    } break;
    case AST_STMT_WHILE: {
        resolve_cond_expr(resolver, statement->while_stmt.condition);
        resolve_statement_block(resolver, statement->while_stmt.block, ret_type);
    } break;
    case AST_STMT_FOR: {
        symbol_enter(resolver);
        if (statement->for_stmt.init) {
            resolve_statement(resolver, statement->for_stmt.init, ret_type);
        }
        if (statement->for_stmt.condition) {
            resolve_cond_expr(resolver, statement->for_stmt.condition);
        }
        resolve_statement_block(resolver, statement->for_stmt.block, ret_type);
        if (statement->for_stmt.next) {
            resolve_statement(resolver, statement->for_stmt.next, ret_type);
        }
        symbol_leave(resolver);
    } break;
    case AST_STMT_ASSIGN: {
        struct resolved_expr left = resolve_expr(resolver, statement->assign.left_expr);

        if (!left.is_lvalue) {
            // TODO: error handling
            printf("cannot assign to non-lvalue\n");
            exit(1);
        }

        if (statement->assign.right_expr) {
            struct resolved_expr right = resolve_expected_expr(resolver, statement->assign.right_expr, left.type);
            if ((is_arithmetic_type(left.type) != is_arithmetic_type(right.type)) ||
                (is_ptrtype(left.type) != is_ptrtype(right.type))) {
                // TODO: error handling
                printf("left-hand side of assignment does not match right-hand side type\n");
                exit(1);
            }
        }

        if (statement->assign.op != '=' && !is_inttype(left.type)) {
            // TODO: error handling
            printf("can only use assignment operators with type int\n");
            exit(1);
        }
    } break;
    case AST_STMT_INIT: {
        struct type *init_type = resolve_expr(resolver, statement->init.expr).type;
        struct symbol *symbol = symbol_var(statement->init.name, init_type);
        statement->init.address = resolver->locals_address;
        resolver->locals_address += init_type->size;
        symbol->address = statement->init.address;
        printf("allocating address '%" PRIu64 "' for local '%s'\n", symbol->address, symbol->name);
        symbol_push(resolver, symbol);
    } break;
    case AST_STMT_DECL: {
        struct type *decl_type = resolve_typespec(resolver, statement->decl.type);
        if (statement->decl.expr) {
            struct resolved_expr init = resolve_expr(resolver, statement->decl.expr);
            if ((is_arithmetic_type(decl_type) != is_arithmetic_type(init.type)) ||
                (is_ptrtype(decl_type) != is_ptrtype(init.type))) {
                // TODO: error handling
                printf("left-hand side of assignment does not match right-hand side type\n");
                exit(1);
            }
        }
        struct symbol *symbol = symbol_var(statement->decl.name, decl_type);
        statement->decl.address = resolver->locals_address;
        resolver->locals_address += decl_type->size;
        symbol->address = statement->decl.address;
        printf("allocating address '%" PRIu64 "' for local decl '%s'\n", symbol->address, symbol->name);
        symbol_push(resolver, symbol);
    } break;
    case AST_STMT_EXPR: {
        struct resolved_expr __unused expr = resolve_expr(resolver, statement->expr);
    } break;
    default:
        printf("GOT UNHANDLED TYPE %d\n", statement->kind);
        assert(0);
        break;
    }
}

struct type *resolve_decl_var(struct resolver *resolver, struct ast_decl *decl)
{
    assert(decl->kind == AST_DECL_VAR);
    struct type *type = NULL;

    if (decl->var_decl.type) {
        type = resolve_typespec(resolver, decl->var_decl.type);
    }

    if (decl->var_decl.expr) {
        struct resolved_expr result = resolve_expected_expr(resolver, decl->var_decl.expr, type);

        if (type) {
            if ((is_arithmetic_type(result.type) != is_arithmetic_type(type)) ||
                (is_ptrtype(result.type) != is_ptrtype(type))) {
                // TODO: error handling
                printf("declared type does not match inferred type\n");
                exit(1);
            }
        }

        if (!type) {
            type = result.type;
        }
    }

    complete_type(resolver, type);
    return type;
}

struct type *resolve_decl_const(struct resolver *resolver, struct ast_decl *decl, int64_t *val)
{
    assert(decl->kind == AST_DECL_CONST);

    struct resolved_expr result = resolve_expr(resolver, decl->const_decl.expr);
    if (!result.is_const) {
        // TODO: error handling
        printf("initializer for const is not a constant expression");
        exit(1);
    }

    *val = result.val;
    return result.type;
}

struct type *resolve_decl_func(struct resolver *resolver, struct ast_decl *decl)
{
    assert(decl->kind == AST_DECL_FUNC);

    struct type **params = NULL;
    for (size_t i = 0; i < decl->func_decl.params_count; ++i) {
        struct type *type = resolve_typespec(resolver, decl->func_decl.params[i].type);
        complete_type(resolver, type);
        buf_push(params, type);

        if (type == type_variadic) {
            decl->func_decl.is_variadic = true;
            printf("VARIADIC FUNCTIONS ARE NOT YET SUPPORTED\n");
            exit(1);
        }
    }

    for (size_t i = 0; i < decl->func_decl.block.statements_count; ++i) {
        struct ast_stmt *statement = decl->func_decl.block.statements[i];
        if (statement->kind == AST_STMT_DECL) {
            struct type *type = resolve_typespec(resolver, statement->decl.type);
            complete_type(resolver, type);
        }
    }

    struct type *ret_type = type_void;
    if (decl->func_decl.ret_type) {
        ret_type = resolve_typespec(resolver, decl->func_decl.ret_type);
        complete_type(resolver, ret_type);
    }

    return type_func(params, buf_len(params), ret_type, decl->func_decl.is_variadic);
}

struct type *resolve_decl_func_foreign(struct resolver *resolver, struct ast_decl *decl)
{
    assert(decl->kind == AST_DECL_FUNC_FOREIGN);

    struct type **params = NULL;
    for (size_t i = 0; i < decl->foreign_func_decl.params_count; ++i) {
        struct type *type = resolve_typespec(resolver, decl->foreign_func_decl.params[i].type);
        complete_type(resolver, type);
        buf_push(params, type);

        if (type == type_variadic) {
            decl->foreign_func_decl.is_variadic = true;
        }
    }

    struct type *ret_type = type_void;
    if (decl->foreign_func_decl.ret_type) {
        ret_type = resolve_typespec(resolver, decl->foreign_func_decl.ret_type);
        complete_type(resolver, ret_type);
    }

    return type_func(params, buf_len(params), ret_type, decl->foreign_func_decl.is_variadic);
}

void resolve_func(struct resolver *resolver, struct symbol *symbol)
{
    struct ast_decl *decl = symbol->decl;
    assert(symbol->kind == SYMBOL_FUNC);
    assert(symbol->state == SYMBOL_RESOLVED);

    symbol->scope = symbol_enter(resolver);
    resolver->locals_address = 0;

    for (size_t i = 0; i < decl->func_decl.params_count; ++i) {
        struct ast_func_param *param = &decl->func_decl.params[i];
        param->address = resolver->locals_address;
        printf("allocating address '%" PRIu64 "' for param '%s'\n", param->address, param->name);
        resolver->locals_address += type_sizeof(resolve_typespec(resolver, param->type));
        struct symbol *param_symbol = symbol_var(param->name, resolve_typespec(resolver, param->type));
        param_symbol->address = param->address;
        symbol_push(resolver, param_symbol);
    }
    if (decl->kind == AST_DECL_FUNC) {
        resolve_statement_block(resolver, decl->func_decl.block, resolve_typespec(resolver, decl->func_decl.ret_type));
    }

    decl->func_decl.ar_size = resolver->locals_address;

    symbol_leave(resolver);
}

void resolve_func_foreign(struct resolver *resolver, struct symbol *symbol)
{
    struct ast_decl *decl = symbol->decl;
    assert(symbol->kind == SYMBOL_FUNC_FOREIGN);
    assert(symbol->state == SYMBOL_RESOLVED);

    symbol->scope = symbol_enter(resolver);
    resolver->locals_address = 0;

    for (size_t i = 0; i < decl->foreign_func_decl.params_count; ++i) {
        struct ast_func_param param = decl->foreign_func_decl.params[i];
        symbol_push(resolver, symbol_var(param.name, resolve_typespec(resolver, param.type)));
    }
    if (decl->kind == AST_DECL_FUNC_FOREIGN) {
        resolve_typespec(resolver, decl->foreign_func_decl.ret_type);
    }

    symbol_leave(resolver);
}

struct symbol *resolve_decl(struct resolver *resolver, struct ast_decl *decl)
{
    enum symbol_kind kind = SYMBOL_NONE;

    switch (decl->kind) {
    case AST_DECL_STRUCT:
    case AST_DECL_ENUM:
    case AST_DECL_UNION:
        kind = SYMBOL_TYPE;
        break;
    case AST_DECL_CONST:
        kind = SYMBOL_CONST;
        break;
    case AST_DECL_VAR:
        kind = SYMBOL_VAR;
        break;
    case AST_DECL_FUNC:
        kind = SYMBOL_FUNC;
        break;
    case AST_DECL_FUNC_FOREIGN:
        kind = SYMBOL_FUNC_FOREIGN;
        break;
    default:
        assert(0);
        break;
    }

    struct symbol *symbol = symbol_new(kind, decl->name, decl);

    if (decl->kind == AST_DECL_STRUCT) {
        symbol->state = SYMBOL_RESOLVED;
        symbol->type = type_incomplete(symbol);
    }

    if (decl->kind == AST_DECL_ENUM) {
        symbol->state = SYMBOL_RESOLVED;
        symbol->type = type_incomplete(symbol);
    }

    buf_push(resolver->symbols, symbol);
    symbol_push(resolver, symbol);

    return symbol;
}

void resolve_symbol(struct resolver *resolver, struct symbol *symbol)
{
    if (symbol->state == SYMBOL_RESOLVED) {
        return;
    }

    if (symbol->state == SYMBOL_RESOLVING) {
        // TODO: error handling
        printf("CYCLIC DEPENDENCY!!!\n");
        exit(1);
    }

    assert(symbol->state == SYMBOL_UNRESOLVED);
    symbol->state = SYMBOL_RESOLVING;

    switch (symbol->kind) {
    case SYMBOL_VAR:
        symbol->type = resolve_decl_var(resolver, symbol->decl);
        break;
    case SYMBOL_CONST:
        symbol->type = resolve_decl_const(resolver, symbol->decl, &symbol->val);
        break;
    case SYMBOL_FUNC:
        symbol->type = resolve_decl_func(resolver, symbol->decl);
        break;
    case SYMBOL_FUNC_FOREIGN:
        symbol->type = resolve_decl_func_foreign(resolver, symbol->decl);
        break;
    default:
        assert(0);
        break;
    }

    symbol->state = SYMBOL_RESOLVED;
    buf_push(resolver->ordered_symbols, symbol);
}

void complete_symbol(struct resolver *resolver, struct symbol *symbol)
{
    resolve_symbol(resolver, symbol);
    if (symbol->kind == SYMBOL_TYPE) {
        complete_type(resolver, symbol->type);
    } else if (symbol->kind == SYMBOL_FUNC) {
        resolve_func(resolver, symbol);
    } else if (symbol->kind == SYMBOL_FUNC_FOREIGN) {
        resolve_func_foreign(resolver, symbol);
    }
}

struct symbol *symbol_type(struct resolver *resolver, const uint8_t *name, struct type *type)
{
    struct symbol *symbol = symbol_new(SYMBOL_TYPE, name, NULL);
    symbol->state = SYMBOL_RESOLVED;
    symbol->type = type;
    buf_push(resolver->symbols, symbol);
    symbol_push(resolver, symbol);
    return symbol;
}

void resolver_init(struct resolver *resolver)
{
    memset(resolver, 0, sizeof(struct resolver));
    struct scope *scope = scope_new(resolver);
    resolver->global_scope = scope;
    resolver->current_scope = scope;

    type_void->symbol = symbol_type(resolver, intern_string(u8"void"), type_void);
    type_char->symbol = symbol_type(resolver, intern_string(u8"char"), type_char);

    type_int->symbol = symbol_type(resolver, intern_string(u8"int"), type_int);
    type_int8->symbol = symbol_type(resolver, intern_string(u8"s8"), type_int8);
    type_int16->symbol = symbol_type(resolver, intern_string(u8"s16"), type_int16);
    type_int32->symbol = symbol_type(resolver, intern_string(u8"s32"), type_int32);
    type_int64->symbol = symbol_type(resolver, intern_string(u8"s64"), type_int64);

    type_uint8->symbol = symbol_type(resolver, intern_string(u8"u8"), type_uint8);
    type_uint16->symbol = symbol_type(resolver, intern_string(u8"u16"), type_uint16);
    type_uint32->symbol = symbol_type(resolver, intern_string(u8"u32"), type_uint32);
    type_uint64->symbol = symbol_type(resolver, intern_string(u8"u64"), type_uint64);

    type_float->symbol = symbol_type(resolver, intern_string(u8"float"), type_float);
    type_float32->symbol = symbol_type(resolver, intern_string(u8"f32"), type_float32);
    type_float64->symbol = symbol_type(resolver, intern_string(u8"f64"), type_float64);

    type_variadic->symbol = symbol_type(resolver, intern_string(u8"var_args"), type_variadic);
}
