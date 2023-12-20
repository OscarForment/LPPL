/*****************************************************************************/
/**   Ejemplo de un posible fichero de cabeceras donde situar las           **/
/** definiciones de constantes, variables y estructuras para MenosC. Los    **/
/** alumnos deberan adaptarlo al desarrollo de su propio compilador.    **/
/*****************************************************************************/
#ifndef _HEADER_H
#define _HEADER_H

/****************************************************** Constantes generales */
#define TRUE  1
#define FALSE 0
#define TALLA_TIPO_SIMPLE 1 /* Talla asociada a los tipos simples */
#define TALLA_SEGENLACES 2 /* Talla del segmento de Enlaces de Control */

#define OP_NOT 0
#define OP_MAS 1
#define OP_MENOS 2
#define OP_POR 3
#define OP_DIV 4
#define OP_INCRE 5
#define OP_DECRE 6
#define OP_MAYOR 7
#define OP_MENOR 8
#define OP_MAYORIG 9
#define OP_MENORIG 10
#define OP_IGUAL 11
#define OP_DISTINTO 12
#define OP_OR 13
#define OP_AND 14

typedef struct lista{
    int ref;
    int talla;
} Lista;

typedef struct texp{
   int t;
   int d;
} Expresion;
/************************************* Variables externas definidas en el AL */
extern int yylex();
extern int yyparse();

extern FILE *yyin;                           /* Fichero de entrada           */
extern int   yylineno;                       /* Contador del numero de linea */
extern char *yytext;                         /* Patron detectado             */
/********* Funciones y variables externas definidas en el Programa Principal */
extern void yyerror(const char * msg) ;   /* Tratamiento de errores          */

extern int verbosidad;                   /* Flag si se desea una traza       */
extern int numErrores;              /* Contador del numero de errores        */


/************************ Variables externas definidas en Programa Principal */
extern int verTdS; /* Flag para saber si mostrar la TdS */


#endif  /* _HEADER_H */
/*****************************************************************************/
