#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

typedef enum {Atom, Pair, Primitive, Procedure} Type;

typedef struct Cell *Cell;
struct Cell {
    Type type;

    // Union is appropriate for the items below,
    // but skipped for simplicity.

    Cell car;
    Cell cdr;

    char atom[16];

    Cell (*primitive)(Cell arguments);
};

Cell null;

#define car(o)          ((o)->car)
#define cdr(o)          ((o)->cdr)
#define set_car(o, val) ((o)->car = val)
#define set_cdr(o, val) ((o)->cdr = val)
#define is_atom(o)      ((o)->type == Atom)
#define is_null(o)      ((o) == null)
#define is_pair(o)      ((o)->type == Pair)
#define is_primitive(o) ((o)->type == Primitive)
#define is_procedure(o) ((o)->type == Procedure)
#define is_eq(o1, o2)   (strcmp((o1)->atom, (o2)->atom) == 0)
#define Cell_new()    malloc(sizeof(struct Cell))

Cell atom(char *s) {
    Cell o = Cell_new();
    o->type = Atom;
    strcpy(o->atom, s);
    return o;
}

#define cons(car, cdr)         make(Pair, car, cdr)
#define procedure(lambda, env) make(Procedure, lambda, env)
Cell make(Type type, Cell car, Cell cdr) {
    Cell o = Cell_new();
    o->type = type;
    o->car = car;
    o->cdr = cdr;
    return o;
}

Cell primitive(Cell (*p)(Cell arguments)) {
    Cell o = Cell_new();
    o->type = Primitive;
    o->primitive = p;
    return o;
}

Cell eval(Cell o, Cell env);
Cell eval_primitive(Cell arguments) {
    return eval(car(arguments), car(cdr(arguments)));
}

Cell cons_primitive(Cell arguments) {
    return cons(car(arguments), car(cdr(arguments)));
}

Cell car_primitive(Cell arguments) {
    return car(car(arguments));
}

Cell cdr_primitive(Cell arguments) {
    return cdr(car(arguments));
}

Cell set_car_primitive(Cell arguments) {
    set_car(car(arguments), car(cdr(arguments)));
    return atom("#<void>");
}

Cell set_cdr_primitive(Cell arguments) {
    set_cdr(car(arguments), car(cdr(arguments)));
    return atom("#<void>");
}

Cell is_atom_primitive(Cell arguments) {
    return is_atom(car(arguments)) ? atom("#t") : atom("#f");
}

Cell is_null_primitive(Cell arguments) {
    return is_null(car(arguments)) ? atom("#t") : atom("#f");
}

Cell is_eq_primitive(Cell arguments) {
    return is_eq(car(arguments), car(cdr(arguments))) ? atom("#t") : atom("#f");
}

Cell add1_primitive(Cell arguments) {
    Cell n = atom("");
    sprintf(n->atom, "%i", (atoi(car(arguments)->atom) + 1));
    return n;
}

Cell sub1_primitive(Cell arguments) {
    Cell n = atom("");
    sprintf(n->atom, "%i", (atoi(car(arguments)->atom) - 1));
    return n;
}

Cell read(FILE *in);
Cell read_primitive(Cell arguments) {
    return read(stdin);
}

void write(FILE *out, Cell o);
Cell write_primitive(Cell arguments) {
    write(stdout, car(arguments));
    return car(arguments);
}

void write_pair(FILE *out, Cell pair) {
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

void write(FILE *out, Cell o) {
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
        write(out, o->car);
    } else {
        fputs("#<wtf?>", out);
    }
}

char is_delimiter(int c) {
    return isspace(c) || c == EOF
                      || c == '(' || c == ')'
                      || c == ';';
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

Cell read_pair(FILE *in) {
    int c;
    Cell car_obj;
    Cell cdr_obj;

    skip_space(in);
    c = getc(in);
    if (c == ')' || c == ']') {
        return null;
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
        if (c != ')' && c != ']') {
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

Cell read(FILE *in) {
    int c;
    int i;
    Cell a;

    skip_space(in);
    c = getc(in);
    if (c == '(') {
        return read_pair(in);
    } else if (c == '\'') {
        return cons(atom("quote"), cons(read(in), null));
    } else if (c == EOF) {
        return atom("#<void>");
    } else if (c == '"') {
        c = getc(in);
        a = atom("");
        i = 0;
        while (c != '"') {
            if (c == '\\') {
                c = getc(in);
                c = c == 'n' ? '\n' : c;
            }
            if (i < 15) {
                a->atom[i++] = c;
            }
            c = getc(in);
        }
        a->atom[i] = '\0';
        return cons(atom("quote"), cons(a, null));
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

Cell extend_env(Cell vars, Cell vals, Cell base_env) {
    return cons(cons(vars, vals), base_env);
}

char is_self_evaluating(Cell o) {
    return is_atom(o) && (isdigit(o->atom[0]) || o->atom[0] == '#');
}

char is_tagged(Cell o, Cell tag) {
    return is_pair(o) && is_atom(car(o)) && is_eq(car(o), tag);
}

Cell lookup(Cell var, Cell env) {
    while (!is_null(env)) {
        Cell frame = car(env);
        Cell vars = car(frame);
        Cell vals = cdr(frame);
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

Cell define(Cell var, Cell val, Cell env) {
    Cell l = lookup(var, env);
    if (is_atom(l) && !is_eq(l, atom("#<unbound>"))) {
        fprintf(stderr, "can't redefine\n");
    }
    //Cell binding = cons(var, val);
    Cell frame = car(env);
    set_car(frame, cons(var, car(frame)));
    set_cdr(frame, cons(val, cdr(frame)));
    return atom("#<void>");
}

Cell set(Cell var, Cell val, Cell env) {
    while (!is_null(env)) {
        Cell frame = car(env);
        Cell vars = car(frame);
        Cell vals = cdr(frame);
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

Cell eval_operands(Cell exp, Cell env) {
    if (is_null(exp)) {
        return null;
    } else {
        Cell e = eval(car(exp), env);
        return cons(e, eval_operands(cdr(exp), env));
    }
}

Cell eval(Cell exp, Cell env) {

    if (is_self_evaluating(exp)) {
        return exp;
    } else if (is_atom(exp)) {
        return lookup(exp, env);
    } else if (is_tagged(exp, atom("define"))) {
        return define(car(cdr(exp)), eval(car(cdr(cdr(exp))), env), env);
    } else if (is_tagged(exp, atom("set!"))) {
        return set(car(cdr(exp)), eval(car(cdr(cdr(exp))), env), env);
    } else if (is_tagged(exp, atom("if"))) {
        Cell cond = eval(car(cdr(exp)), env);
        if (is_atom(cond) && is_eq(cond, atom("#f"))) {
           exp = car(cdr(cdr(cdr(exp))));
        } else {
           exp = car(cdr(cdr(exp)));
        }
        return eval(exp, env);
    } else if (is_tagged(exp, atom("vau"))) {
        return procedure(exp, env);
    } else if (is_pair(exp)) {
        Cell proc = eval(car(exp), env);
        if (is_primitive(proc)) {
            return (proc->primitive)(eval_operands(cdr(exp), env));
        } else if (is_procedure(proc)) {
            Cell src = car(proc);
            Cell e = car(cdr(cdr(src)));
            Cell para = cons(e, cons(car(cdr(src)), null));
            Cell args = cons(env, cons(cdr(exp), null));
            Cell body = car(cdr(cdr(cdr(src))));
            return eval(body, extend_env(para, args, cdr(proc)));
        }
    }
    write(stderr, exp);
    fprintf(stderr, ": eval illegal state\n");
    return atom("#<void>");
}

Cell make_env(void) {
    //Cell e = cons(cons(cons(atom("pi"), atom("3")), null), null);
    Cell e = extend_env(null, null, null);
    define(atom("cons"),  primitive(cons_primitive), e);
    define(atom("car"),   primitive(car_primitive), e);
    define(atom("cdr"),   primitive(cdr_primitive), e);
    define(atom("set-car!"),   primitive(set_car_primitive), e);
    define(atom("set-cdr!"),   primitive(set_cdr_primitive), e);
    define(atom("atom?"), primitive(is_atom_primitive), e);
    define(atom("null?"), primitive(is_null_primitive), e);
    define(atom("eq?"),   primitive(is_eq_primitive), e);
    define(atom("add1"),  primitive(add1_primitive), e);
    define(atom("sub1"),  primitive(sub1_primitive), e);
    define(atom("eval"),  primitive(eval_primitive), e);
    define(atom("read"),  primitive(read_primitive), e);
    define(atom("write"),  primitive(write_primitive), e);
    return e;
}

int main(int argc, char *argv[]) {
    Cell env = make_env();

    for (int i = 1; i < argc; i++) {
        FILE* file = fopen(argv[i], "r");
        while (peek(file) != EOF) {
            eval(read(file), env);
        }
    }
}
