#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

typedef enum {Atom, Pair, Primitive, Procedure} Type;

typedef struct Object *Object;
struct Object {
    Type type;

    // Union is appropriate for the items below,
    // but skipped for simplicity.

    Object car;
    Object cdr;

    char atom[16];

    Object (*primitive)(Object arguments);

    Object procedure;
    Object environment;
};

#define car(o)          ((o)->car)
#define cdr(o)          ((o)->cdr)
#define set_car(o, val) ((o)->car = val)
#define set_cdr(o, val) ((o)->cdr = val)
#define is_atom(o)      ((o)->type == Atom)
#define is_null(o)      ((o) == NULL)
#define is_pair(o)      ((o)->type == Pair)
#define is_primitive(o) ((o)->type == Primitive)
#define is_procedure(o) ((o)->type == Procedure)
#define is_eq(o1, o2)   (strcmp((o1)->atom, (o2)->atom) == 0)
#define Object_new()    malloc(sizeof(struct Object))

Object atom(char *s) {
    Object o = Object_new();
    strcpy(o->atom, s);
    return o;
}

Object cons(Object car, Object cdr) {
    Object o = Object_new();
    o->type = Pair;
    o->car = car;
    o->cdr = cdr;
    return o;
}

Object procedure(Object lambda, Object env) {
    Object o = Object_new();
    o->type = Procedure;
    o->procedure = lambda;
    o->environment = env;
    return o;
}

Object primitive(Object (*p)(Object arguments)) {
    Object o = Object_new();
    o->type = Primitive;
    o->primitive = p;
    return o;
}

Object cons_primitive(Object arguments) {
    return cons(car(arguments), car(cdr(arguments)));
}

Object car_primitive(Object arguments) {
    return car(car(arguments));
}

Object cdr_primitive(Object arguments) {
    return cdr(car(arguments));
}

Object is_atom_primitive(Object arguments) {
    return is_atom(car(arguments)) ? atom("#t") : atom("#f");
}

Object is_null_primitive(Object arguments) {
    return is_null(car(arguments)) ? atom("#t") : atom("#f");
}

Object is_eq_primitive(Object arguments) {
    return is_eq(car(arguments), car(cdr(arguments))) ? atom("#t") : atom("#f");
}

Object add1_primitive(Object arguments) {
    Object n = atom("");
    sprintf(n->atom, "%i", (atoi(car(arguments)->atom) + 1));
    return n;
}

Object sub1_primitive(Object arguments) {
    Object n = atom("");
    sprintf(n->atom, "%i", (atoi(car(arguments)->atom) - 1));
    return n;
}

void write(FILE *out, Object o);
void write_pair(FILE *out, Object pair) {
    write(out, car(pair));
    if (is_null(cdr(pair))) {
        return;
    } else if (is_pair(cdr(pair))) {
        fputs(" ", out);
        write_pair(out, cdr(pair));
    } else {
        fputs(" . ", out);
        write(out, cdr(pair));
    }
}

void write(FILE *out, Object o) {
    if (is_null(o)) {
        fputs("()", out);
    } else if (is_atom(o) && is_eq(o, atom("#<void>"))) {
    } else if (is_atom(o)) {
        fputs(o->atom, out);
    } else if (is_pair(o)) {
        fputs("(", out);
        write_pair(out, o);
        fputs(")", out);
    } else if (is_primitive(o)) {
        fputs("#<primitive>", out);
    } else if (is_procedure(o)) {
        //fputs("#<procedure>", out);
        write(out, o->procedure);
    } else {
        fputs("#<wtf?>", out);
    }
}

char is_delimiter(int c) {
    return isspace(c) || c == EOF || c == '(' || c == ')' || c == ';';
}

void skip_space(FILE *in) {
    int c;

    while (c = getc(in), c != EOF) {
        if (isspace(c)) {
            continue;
        }
        else if (c == ';') {
            while (c = getc(in), c != EOF && c != '\n')
                ;
            continue;
        }
        ungetc(c, in);
        break;
    }
}

int peek(FILE *in) {
    int c = getc(in);
    ungetc(c, in);
    return c;
}

Object read(FILE *in);
Object read_pair(FILE *in) {
    int c;
    Object car_obj;
    Object cdr_obj;

    skip_space(in);
    c = getc(in);
    if (c == ')') {
        return NULL;
    }
    ungetc(c, in);
    car_obj = read(in);
    skip_space(in);
    c = getc(in);
    if (c == '.') {
        c = peek(in);
        if (!is_delimiter(c)) {
            fprintf(stderr, "dot not followed by delimiter\n");
            exit(1);
        }
        cdr_obj = read(in);
        skip_space(in);
        c = getc(in);
        if (c != ')') {
            fprintf(stderr, "missing right paren\n");
            exit(1);
        }
        return cons(car_obj, cdr_obj);
    }
    else { /* read list */
        ungetc(c, in);
        cdr_obj = read_pair(in);
        return cons(car_obj, cdr_obj);
    }
}

Object read(FILE *in) {
    int c;
    int i;
    Object a;

    skip_space(in);
    c = getc(in);
    if (c == '(') {
        return read_pair(in);
    } else if (c == '\'') {
        return cons(atom("quote"), cons(read(in), NULL));
    } else if (c == EOF) {
        fprintf(stderr, "^D");
        exit(1);
    } else {
        a = atom("");
        i = 0;
        while (!is_delimiter(c)) {
            if (i < 15) {
                a->atom[i++] = c;
            }
            c = getc(in);
        }
        a->atom[i] = '\0';
        ungetc(c, in);
        return a;
    }
}

Object extend_environment(Object vars, Object vals, Object base_env) {
    return cons(cons(vars, vals), base_env);
}

char is_self_evaluating(Object o) {
    return is_atom(o) && (isdigit(o->atom[0]) || o->atom[0] == '#');
}

char is_tagged(Object o, Object tag) {
    return is_pair(o) && is_atom(car(o)) && is_eq(car(o), tag);
}

Object lookup(Object var, Object env) {
    while (!is_null(env)) {
        Object frame = car(env);
        Object vars = car(frame);
        Object vals = cdr(frame);
        while (!is_null(vars)) {
            if (is_eq(car(vars), var)) {
                return car(vals);
            }
            vars = cdr(vars);
            vals = cdr(vals);
        }
        env = cdr(env);
    }
    return atom("#<unbound>");
}

Object define(Object var, Object val, Object env) {
    Object l = lookup(var, env);
    if (is_atom(l) && !is_eq(l, atom("#<unbound>"))) {
        fprintf(stderr, "can't redefine\n");
    }
    //Object binding = cons(var, val);
    Object frame = car(env);
    set_car(frame, cons(var, car(frame)));
    set_cdr(frame, cons(val, cdr(frame)));
    return atom("#<void>");
}

Object set(Object var, Object val, Object env) {
    while (!is_null(env)) {
        Object frame = car(env);
        Object vars = car(frame);
        Object vals = cdr(frame);
        while (!is_null(vars)) {
            if (is_eq(car(vars), var)) {
                set_car(vals, val);
                return atom("#<void>");
            }
            vars = cdr(vars);
            vals = cdr(vals);
        }
        env = cdr(env);
    }
    fprintf(stderr, "unbound variable\n");
}

Object eval(Object o, Object env);
Object eval_operands(Object exp, Object env) {
    if (is_null(exp)) {
        return NULL;
    } else {
        return cons(eval(car(exp), env), eval_operands(cdr(exp), env));
    }
}

Object eval(Object o, Object env) {

tailcall:
    if (is_self_evaluating(o)) {
        return o;
    } else if (is_atom(o) && is_eq(o, atom("environment"))) {
        return env;
    } else if (is_atom(o)) {
        return lookup(o, env);
    } else if (is_tagged(o, atom("quote"))) {
        return car(cdr(o));
    } else if (is_tagged(o, atom("define"))) {
        return define(car(cdr(o)), eval(car(cdr(cdr(o))), env), env);
    } else if (is_tagged(o, atom("set!"))) {
        return set(car(cdr(o)), eval(car(cdr(cdr(o))), env), env);
    } else if (is_tagged(o, atom("if"))) {
        Object cond = eval(car(cdr(o)), env);
        o = is_atom(cond) && is_eq(cond, atom("#f")) ? car(cdr(cdr(cdr(o)))) :
                    car(cdr(cdr(o)));
        goto tailcall;
    } else if (is_tagged(o, atom("lambda"))) {
        return procedure(o, env);
    } else if (is_pair(o)) {
        Object proc = eval(car(o), env);
        Object args = eval_operands(cdr(o), env);
        if (is_primitive(proc)) {
            return (eval(car(o), env)->primitive)(eval_operands(cdr(o), env));
        } else {
            env = extend_environment(car(cdr(proc->procedure)),
                                     args,
                                     proc->environment);
            o = car(cdr(cdr(proc->procedure)));
            //exp = make_begin(procedure->data.compound_proc.body);
            goto tailcall;
        }
    }
    fprintf(stderr, "eval illegal state\n");
}

Object make_environment(void) {
    //Object e = cons(cons(cons(atom("pi"), atom("3")), NULL), NULL);
    Object e = extend_environment(NULL, NULL, NULL);
    define(atom("cons"),  primitive(cons_primitive), e);
    define(atom("car"),   primitive(car_primitive), e);
    define(atom("cdr"),   primitive(cdr_primitive), e);
    define(atom("atom?"), primitive(is_atom_primitive), e);
    define(atom("null?"), primitive(is_null_primitive), e);
    define(atom("eq?"),   primitive(is_eq_primitive), e);
    define(atom("add1"),  primitive(add1_primitive), e);
    define(atom("sub1"),  primitive(sub1_primitive), e);
    return e;
}

int main(void) {
    Object environment = make_environment();
    while (1) {
        printf("> ");
        write(stdout, eval(read(stdin), environment));
        printf("\n");
    }
}
