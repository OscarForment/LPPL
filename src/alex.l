%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "asin.h"

#define  retornar(x) {if (verbosidad) ECHO ; return (x) ; }
%}
/*-----------------------------------------------------------------------------
  Estas opciones evitan "warnings" innecesarios y permiten obtener el
  numero de linea actual en la variable global "yylineno".
-----------------------------------------------------------------------------*/
%option noinput       
%option nounput       
%option yylineno  

delimitador    [ \t]+
digito         [0-9]
entero         {digito}+
letra          [a-zA-Z]
ident          {letra}({letra}|{digito}|_)*
constante      ({entero}"."|{entero}|"."{entero}|{entero}"."{entero}) 
comentario     \/\/.*

%%
"\n"            {if (verbosidad) {ECHO;fprintf(stdout,"%3d.- ", yylineno);};}
{delimitador}   {if (verbosidad) ECHO; }
{comentario}    {if (verbosidad) ECHO; }
"="             { retornar (ASIG_);    }
"+"             { retornar (MAS_);     }
"++"            { retornar (INCRE_);   }
"-"             { retornar (MENOS_);   }
"--"            { retornar (DECRE_);   }
"*"             { retornar (POR_);     }
"/"             { retornar (DIV_);     }
"("             { retornar (APAR_);    }
")"             { retornar (CPAR_);    }
"{"             { retornar (ALLA_);    }
"}"             { retornar (CLLA_);    }
"["             { retornar (ACOR_);    }
"]"             { retornar (CCOR_);    }
"."             { retornar (PUNTO_);   }
","             { retornar (COMA_);    }
";"             { retornar (PCOMA_);   }

"=="            { retornar (IGUAL_);   }
"!="            { retornar(DISTINTO_); }
">"             { retornar(MAYOR_); }
"<"             { retornar(MENOR_); }
">="            { retornar(MAYORIG_); }
"<="            { retornar(MENORIG_); }
"&&"            { retornar(AND_); }
"||"            { retornar(OR_); }
"!"             { retornar(NOT_); }

"true"          { retornar(TRUE_); }
"false"         { retornar(FALSE_); }
"if"            { retornar(IF_); }
"else"          { retornar(ELSE_); }
"while"         { retornar(WHILE_); }
"int"           { retornar(INT_); }
"bool"          { retornar(BOOL_); }
"struct"        { retornar(STRUCT_); }
"return"        { retornar(RETURN_); }
"read"          { retornar(READ_); }
"print"         { retornar(PRINT_); }

{constante}     { retornar (CTE_);     }
{ident}         { retornar (ID_);      }  
.		{ yyerror("Caracter desconocido") ;}
%%
