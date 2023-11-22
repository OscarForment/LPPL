%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "libtds.h"
%}

%union{
       char *ident;
       int cent;
       Lista tlista;
	Expresion texp;
}

/*Separadores*/
%token ALLA_  CLLA_ APAR_ CPAR_ ACOR_ CCOR_ PUNTO_ COMA_ PCOMA_
/*Operadores*/
%token MAS_ MENOS_ POR_ DIV_ INCRE_ DECRE_ ASIG_
/*Logica*/
%token AND_ OR_ NOT_
%token MAYOR_ MENOR_ MAYORIG_ MENORIG_ IGUAL_ DISTINTO_
/*Palabras reservadas*/
%token WHILE_ IF_ ELSE_
%token INT_     BOOL_
%token READ_    PRINT_	RETURN_ STRUCT_ TRUE_   FALSE_

%token<ident> ID_
%token<cent> CTE_

%%

/*##################################*/
programa   : 
              {dvar=0; niv=0; cargaContexto(niv)}
              listDecla
              {if(verTdS) mostrarTdS();}
       ;
listDecla   : decla
       | listDecla decla
       ;
decla   : declaVar
       | declaFunc
       ;
declaVar   : tipoSimp ID_ PCOMA_
       {insTdS($2, VARIABLE, $1, niv, dvar, -1);
       dvar += TALLA_TIPO_SIMPLE}
       | tipoSimp ID_ ACOR_ CTE_ CCOR_ PCOMA_
       { int numelem = $4;
        if ($4 <= 0) {
              yyerror("Talla inapropiada del array");
              numelem = 0;
        }
        int refe = insTdA($1, numelem);
        if ( !insTdS($2, VARIABLE, T_ARRAY, niv, dvar, refe) )
              yyerror ("Identificador repetido");
        else dvar += numelem * TALLA_TIPO_SIMPLE;
       }
       | STRUCT_ ALLA_ listCamp CLLA_ ID_ PCOMA_
       ;

tipoSimp   : INT_
       {$$ = T_ENTERO;}
       | BOOL_
       {$$ = T_LOGICO;}
       ;
listCamp   : tipoSimp ID_ PCOMA_
       {insTdS($2, VARIABLE, $1, niv, dvar, -1);
       dvar += TALLA_TIPO_SIMPLE}
       | listCamp tipoSimp ID_ PCOMA_
       {insTdS($2, VARIABLE, $1, niv, dvar, -1);
       dvar += TALLA_TIPO_SIMPLE}
       ;

declaFunc   : tipoSimp ID_ APAR_ paramForm CPAR_ ALLA_ declaVarLocal listInst RETURN_ expre PCOMA_ CLLA_
       {insTdS($2, FUNCION, $1, niv, dvar, $5)}
       ;

paramForm   :
       | listParamForm
       ;
listParamForm   : tipoSimp ID_
       | tipoSimp ID_ COMA_ listParamForm
       ;
declaVarLocal   :
       | declaVarLocal declaVar
       ;
listInst   :
       | listInst inst
       ;

inst   : ALLA_ listInst CLLA_
       | instExpre
       | instEntSal
       | instSelec
       | instIter
       ;

instExpre   : expre PCOMA_ | PCOMA_

instEntSal   : READ_ APAR_ ID_ CPAR_ PCOMA_
       | PRINT_ APAR_ expre CPAR_ PCOMA_
       ;

instSelec   : IF_ APAR_ expre CPAR_ inst ELSE_ inst
       ;

instIter   : WHILE_ APAR_ expre CPAR_ inst
       ;

expre   : expreLogic
       | ID_ ASIG_ expre
       | ID_ ACOR_ expre CCOR_ ASIG_ expre
       | ID_ PUNTO_ ID_ ASIG_ expre
       ;

expreLogic   : expreIgual
       | expreLogic opLogic expreIgual
       ;

expreIgual   : expreRel
       | expreIgual opIgual expreRel
       ;

expreRel   : expreAd
       | expreRel opRel expreAd
       ;

expreAd   : expreMul
       | expreAd opAd expreMul
       ;

expreMul   : expreUna
       | expreMul opMul expreUna
       ;

expreUna   : expreSufi
       | opUna expreUna
       | opIncre ID_
       ;

expreSufi   : const
       | APAR_ expre CPAR_
       | ID_
       | ID_ opIncre
       | ID_ PUNTO_ ID_
       | ID_ ACOR_ expre CCOR_
       | ID_ APAR_ paramAct CPAR_
       ;

const   : CTE_
       | TRUE_
       | FALSE_
       ;

paramAct   :
       | listParamAct
       ;

listParamAct   : expre
       | expre COMA_ listParamAct
       ;

opLogic   : AND_
       | OR_
       ;

opIgual   : IGUAL_
       | DISTINTO_
       ;

opRel   : MAYOR_
       | MENOR_
       | MAYORIG_
       | MENORIG_
       ;

opAd   : MAS_
       | MENOS_
       ;

opMul   : POR_
       | DIV_
       ;

opUna   : MAS_
       | MENOS_
       | NOT_
       ;

opIncre   : INCRE_
       | DECRE_
       ;

%%
