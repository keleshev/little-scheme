#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


typedef enum {Atom, Pair, Primitive, Procedure} Type;


typedef struct Object *Object;
struct Object {
    Type type;
    Object car;
    Object cdr;
    char atom[16];
    Object (*primitive)(Object arguments);
};


#define car(o) ((o)->car)
#define cdr(o) ((o)->cdr)
#define is_atom(o) ((o)->type == Atom)
#define is_null(o) ((o) == NULL)
#define is_pair(o) ((o)->type == Pair)
#define is_list(o) (is_null(o) || is_pair(o))
#define is_primitive(o) ((o)->type == Primitive)
#define is_procedure(o) ((o)->type == Procedure)
#define is_eq(o1, o2) (strcmp((o1)->atom, (o2)->atom) == 0)
#define Object_new() malloc(sizeof(struct Object))

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

Object primitive(Object (*p)(Object arguments)) {
    Object o = Object_new();
    o->type = Primitive;
    o->primitive = p;
    return o;
}

/*
 * Built-in procedures
 */

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
    } else if (is_atom(o)) {
        fputs(o->atom, out);
    } else if (is_pair(o)) {
        fputs("(", out);
        write_pair(out, o);
        fputs(")", out);
    } else if (is_primitive(o)) {
        fputs("#<primitive>", out);
    } else if (is_procedure(o)) {
        fputs("#<procedure>", out);
    } else {
        fputs("cannot write unknown type\n", stderr);
        exit(1);
    }
}

int main(void) {
    printf("Welcome to Ideal Scheme.\n");
    write(stdout, cons(atom("#t"), cons(atom("a"), atom("b"))));
    write(stdout, cons(atom("a"), cons(atom("b"), cons(atom("c"), NULL))));
    write(stdout, NULL);
    write(stdout, add1_primitive(cons(atom("100"), NULL)));
    write(stdout, sub1_primitive(cons(atom("100"), NULL)));
    write(stdout, is_eq_primitive(cons(atom("a"), cons(atom("b"), NULL))));
    write(stdout, is_eq_primitive(cons(atom("a"), cons(atom("a"), NULL))));
}
