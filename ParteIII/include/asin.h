/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_ASIN_H_INCLUDED
# define YY_YY_ASIN_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    ALLA_ = 258,                   /* ALLA_  */
    CLLA_ = 259,                   /* CLLA_  */
    APAR_ = 260,                   /* APAR_  */
    CPAR_ = 261,                   /* CPAR_  */
    ACOR_ = 262,                   /* ACOR_  */
    CCOR_ = 263,                   /* CCOR_  */
    PUNTO_ = 264,                  /* PUNTO_  */
    COMA_ = 265,                   /* COMA_  */
    PCOMA_ = 266,                  /* PCOMA_  */
    MAS_ = 267,                    /* MAS_  */
    MENOS_ = 268,                  /* MENOS_  */
    POR_ = 269,                    /* POR_  */
    DIV_ = 270,                    /* DIV_  */
    INCRE_ = 271,                  /* INCRE_  */
    DECRE_ = 272,                  /* DECRE_  */
    ASIG_ = 273,                   /* ASIG_  */
    AND_ = 274,                    /* AND_  */
    OR_ = 275,                     /* OR_  */
    NOT_ = 276,                    /* NOT_  */
    MAYOR_ = 277,                  /* MAYOR_  */
    MENOR_ = 278,                  /* MENOR_  */
    MAYORIG_ = 279,                /* MAYORIG_  */
    MENORIG_ = 280,                /* MENORIG_  */
    IGUAL_ = 281,                  /* IGUAL_  */
    DISTINTO_ = 282,               /* DISTINTO_  */
    WHILE_ = 283,                  /* WHILE_  */
    IF_ = 284,                     /* IF_  */
    ELSE_ = 285,                   /* ELSE_  */
    INT_ = 286,                    /* INT_  */
    BOOL_ = 287,                   /* BOOL_  */
    READ_ = 288,                   /* READ_  */
    PRINT_ = 289,                  /* PRINT_  */
    RETURN_ = 290,                 /* RETURN_  */
    STRUCT_ = 291,                 /* STRUCT_  */
    TRUE_ = 292,                   /* TRUE_  */
    FALSE_ = 293,                  /* FALSE_  */
    CTE_ = 294,                    /* CTE_  */
    ID_ = 295                      /* ID_  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_ASIN_H_INCLUDED  */
