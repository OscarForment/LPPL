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

%type<tlista> paramForm listParamForm listCamp

%type<cent> listDecla decla tipoSimp declaFunc declaVar
              opAd opIgual opIncre opLogic opMul opRel opUna

%type<texp> expre expreAd expreIgual expreLogic expreMul expreRel expreSufi expreUna const

%%

/*##################################*/
programa   : 
              {dvar=0; niv=0; cargaContexto(niv);}
              listDecla
              {if(verTdS) mostrarTdS();}
       ;
listDecla   : decla {$$ = $1;}
       | listDecla decla {$$ = $1 + $2;}
       ;
decla   : declaVar {$$=0;}
       | declaFunc {$$=$1;}
       ;
declaVar   : tipoSimp ID_ PCOMA_
       {insTdS($2, VARIABLE, $1, niv, dvar, -1);
       dvar += TALLA_TIPO_SIMPLE;}
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
       {
              insTdS($5, VARIABLE, T_RECORD, niv, dvar, -1,);
              dvar += $3.talla;
       }
       ;

tipoSimp   : INT_
       {$$ = T_ENTERO;}
       | BOOL_
       {$$ = T_LOGICO;}
       ;
listCamp   : tipoSimp ID_ PCOMA_
       {insTdS($2, VARIABLE, $1, niv, dvar, -1);
       dvar += TALLA_TIPO_SIMPLE;
       int refe = insTdR($$.ref, $2, $1, dvar);
       $$.talla = TALLA_TIPO_SIMPLE;
       $$.ref=refe;
       }
       | listCamp tipoSimp ID_ PCOMA_
       {insTdS($3, VARIABLE, $2, niv, dvar, -1);
       dvar += TALLA_TIPO_SIMPLE;
       $$.talla = $1.talla + TALLA_TIPO_SIMPLE;
       int refe = insTdR($1.ref, $3, $2, dvar);
       $$.ref=refe;
       }
       ;

declaFunc   : tipoSimp ID_ 
              {niv+=1; $<cent>$ = dvar; dvar = 0; cargaContexto(niv);}
              APAR_ paramForm CPAR_ 
              {insTdS($2, FUNCION, $1, niv, dvar, $5.ref);}
              ALLA_ declaVarLocal listInst RETURN_ expre PCOMA_ CLLA_
              {//si obtTdS($10).t != tipoSimp then error
                     if (verTdS){
                            mostrarTdS();
                     }
                     descargaContexto(niv);
                     niv-=1;
                     dvar=$<cent>2;
              }
       ;

paramForm   :
       {$$.ref = insTdD(-1, T_VACIO);
       $$.talla = 0;}
       | listParamForm
       {$$.ref = $1.ref;
       $$.talla = $1.talla;}
       ;
listParamForm   : tipoSimp ID_
       {$$.ref = insTdD(-1, $1);
       $$.talla = TALLA_TIPO_SIMPLE + TALLA_SEGENLACES;
       if(!insTdS($2, PARAMETRO, $1, niv, -$$.talla, -1)) yyerror("Ya existe un parametro con el mismo identificador.");
       }
       | tipoSimp ID_ COMA_ listParamForm
       {
       $$.ref = insTdD($4.ref, $1);
       $$.talla = $4.talla + TALLA_TIPO_SIMPLE;
       if(!insTdS($2, PARAMETRO, $1, niv, -$$.talla, -1))  yyerror("Ya existe un parametro con el mismo identificador.");
	}
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

expre   : expreLogic {$$.t = $1.t;}
       | ID_ ASIG_ expre
       { SIMB sim = obtTdS($1);
       if (sim.t == T_ERROR) yyerror("Objeto no declarado");
       else if (! (((sim.t == T_ENTERO) && ($3.t == T_ENTERO)) ||
                     ((sim.t == T_LOGICO) && ($3.t == T_LOGICO))) )
              yyerror("Error de tipos en la ‘instrucci´on de asignaci´on’");
       }

       | ID_ ACOR_ expre CCOR_ ASIG_ expre
       {
              SIMB simb = obtTdS($1);
              DIM ar = obtTdA($1);
       
       }
       | ID_ PUNTO_ ID_ ASIG_ expre
       {
              SIMB simb = obtTdS($1);
              CAMP reg = obtTdR(simb1.ref, $3);

       }
       ;

expreLogic   : expreIgual {$$.t = $1.t;}
       | expreLogic opLogic expreIgual
       {$$.t = T_LOGICO;}
       ;

expreIgual   : expreRel {$$.t = $1.t;}
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

expreUna   : expreSufi {$$.t = $1.t;}
       | opUna expreUna
       {
              if($2.t == T_ENTERO){
                     $$.t=$2.t;
              }else if ($2.t == T_LOGICO) {
                     $$.t==$2.t;
              }
       }
       | opIncre ID_
       {
              SIMB simb = obtTdS($2);
              $$.t=simb.t;
       }
       ;

expreSufi   : const {$$.t=$1.t;}
       | APAR_ expre CPAR_ {$$.t = $2.t;}
       | ID_ {
              SIMB simb = obtTdS($1);
              $$.t=simb.t;
       }
       | ID_ opIncre {
              SIMB simb = obtTdS($1);
              $$.t=simb.t;
       }
       | ID_ PUNTO_ ID_
       | ID_ ACOR_ expre CCOR_ {
              SIMB simb = obtTdS($1);
              DIM dim = obtTdA(simb.ref);
              $$.t = dim.telem;
       }
       | ID_ APAR_ paramAct CPAR_ {
              SIMB simb = obtTdS($1);
              INF inf = obtTdD(simb.ref);
              $$.t=inf.t;
       }
       ;

const   : CTE_  {$$.t=T_ENTERO;}
       | TRUE_  {$$.t=T_LOGICO;}
       | FALSE_ {$$.t=T_LOGICO;}
       ;

paramAct   :
       | listParamAct
       ;

listParamAct   : expre
       | expre COMA_ listParamAct
       ;

opLogic   : AND_ {$$ = OP_AND;}
       | OR_ {$$ = OP_OR;}
       ;

opIgual   : IGUAL_ {$$ = OP_IGUAL;}
       | DISTINTO_ {$$ = OP_DISTINTO;}
       ;

opRel   : MAYOR_ {$$ = OP_MAYOR;}
       | MENOR_ {$$ = OP_MENOR;}
       | MAYORIG_ {$$ = OP_MAYORIG;}
       | MENORIG_ {$$ = OP_MENORIG;}
       ;

opAd   : MAS_ {$$ = OP_MAS;}
       | MENOS_ {$$ = OP_MENOS;}
       ;

opMul   : POR_ {$$ = OP_POR;}
       | DIV_ {$$ = OP_DIV;}
       ;

opUna   : MAS_ {$$ = OP_MAS;}
       | MENOS_ {$$ = OP_MENOS;}
       | NOT_ {$$ = OP_NOT;}
       ;

opIncre   : INCRE_ {$$ = OP_INCRE;}
       | DECRE_ {$$ = OP_DECRE;}
       ;

%%
