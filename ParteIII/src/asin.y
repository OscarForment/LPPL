%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "libtds.h"
#include "libgci.h"
%}

%union{
       char *ident;
       int cent;
       Lista tlista;
	Expresion texp;
       AUX aux;
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
              opAd opIgual opIncre opLogic opMul opRel opUna paramAct listParamAct

%type<texp> expre expreAd expreIgual expreLogic expreMul expreRel expreSufi expreUna const

%%

/*##################################*/
programa   :
              {dvar=0; niv=0; cargaContexto(niv);si=0;
              $<aux>$.ref1 = creaLans(si);
              emite(INCTOP, crArgNul(), crArgNul(), crArgEnt(-1));
              $<aux>$.ref2 = creaLans(si);
              emite(GOTOS, crArgNul(), crArgNul(), crArgEtq(-1));
              }
              listDecla
              { //char m = "main";
              if(obtTdS("main").t==T_ERROR) yyerror("No existe la funcion main");
              if(verTdS) mostrarTdS();
              completaLans($<aux>1.ref1, crArgEnt(dvar));
              SIMB simb = obtTdS("main");
              $<aux>$.ref3 = simb.d;
              completaLans($<aux>1.ref2, crArgEtq($<aux>$.ref3));
              }
       ;
listDecla   : decla {$$ = $1;}
       | listDecla decla {$$ = $1 + $2;}
       ;
decla   : declaVar {$$=0;}
       | declaFunc {$$=$1;}
       ;
declaVar   : tipoSimp ID_ PCOMA_
       {if(!insTdS($2, VARIABLE, $1, niv, dvar, -1)){
            yyerror ("Identificador repetido");
       } else {
            dvar += TALLA_TIPO_SIMPLE;
       }
    }
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
              if ( !insTdS($5, VARIABLE, T_RECORD, niv, dvar, $3.ref))
                    yyerror ("Identificador repetido");
             // else dvar += $3.talla;
       }
       ;

tipoSimp   : INT_
       {$$ = T_ENTERO;}
       | BOOL_
       {$$ = T_LOGICO;}
       ;
listCamp   : tipoSimp ID_ PCOMA_
       {
       $$.ref= insTdR(-1, $2, $1, 0);
       $$.talla = TALLA_TIPO_SIMPLE;

       }


       | listCamp tipoSimp ID_ PCOMA_
       {
       int refe = insTdR($1.ref, $3, $2, $1.talla);
       if(refe<0){
              yyerror ("Campo repetido");
       }else{
              $$.talla = $1.talla + TALLA_TIPO_SIMPLE;
              $$.ref=refe;
       }

       }
       ;

declaFunc   : tipoSimp ID_
              {niv+=1; $<cent>$ = dvar; dvar = 0; cargaContexto(niv);}
              APAR_ paramForm CPAR_
              {if (!insTdS($2, FUNCION, $1, niv-1, si, $5.ref)){
                     yyerror("La funcion esta repetida");
              }
              emite( PUSHFP, crArgNul(), crArgNul(), crArgNul() );
              emite( FPTOP, crArgNul(), crArgNul(), crArgNul() );
              $<cent>$ = creaLans(si);
              emite( INCTOP, crArgNul(), crArgNul(), crArgEnt(-1) );  
              }
              ALLA_ declaVarLocal listInst RETURN_ expre PCOMA_ CLLA_
              {
                     
                     if($12.t != $1){
                            yyerror("Error de tipos del return");
                     }
                     completaLans($<cent>7, crArgEnt(dvar));
                     int dvr = TALLA_SEGENLACES + TALLA_TIPO_SIMPLE + $5.talla;
                     emite(EASIG, crArgPos(niv, $12.d), crArgNul(), crArgPos(niv, -dvr));
                     emite(TOPFP, crArgNul(), crArgNul(), crArgNul() );
                     emite(FPPOP, crArgNul(), crArgNul(), crArgNul() );
                     if (strcmp($2,"main") == 0){
                            $$ = -1;
                            emite(FIN, crArgNul(), crArgNul(), crArgNul());
                     } else {
                            $$ = 0;
                            emite(RET, crArgNul(), crArgNul(), crArgNul());
                     }

                     
                     if (verTdS){
                            mostrarTdS();
                     }
                     descargaContexto(niv);
                     niv-=1;
                     dvar=$<cent>3;
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
       ;

instEntSal   : READ_ APAR_ ID_ CPAR_ PCOMA_
                     {
                            SIMB simb = obtTdS($3);
                            if(simb.t != T_ENTERO)
                                   {
                                      yyerror("El argumento de entrada no es de tipo entero.");
                                   }
                            emite(EREAD, crArgNul(), crArgNul(),crArgPos(simb.n , simb.d));  
                     }
       | PRINT_ APAR_ expre CPAR_ PCOMA_
              {
                     if($3.t != T_ERROR && $3.t != T_ENTERO)
                            {
                                   yyerror("El argumento de salida no es un entero");
                            }
                     emite(EWRITE, crArgNul(), crArgNul(), crArgPos(niv, $3.d)); 
              }
       ;

instSelec   : IF_ APAR_ expre CPAR_
              {
                     if ($3.t != T_ERROR && $3.t != T_LOGICO)
                     {
                            yyerror("La expresion a evaluar no es una expresión lógica.");
                     }
                     $<aux>$.valor = creaLans(si);  
                     emite(EIGUAL, crArgPos(niv, $3.d), crArgEnt(0), crArgEtq(-1));  
              }
              inst {
                     $<aux>$.valor = creaLans(si); 
                     emite(GOTOS, crArgNul(), crArgNul(), crArgEtq(-1));  
                     completaLans($<aux>5.valor, crArgEtq(si));
              }
              
              ELSE_ inst { completaLans($<aux>7.valor, crArgEtq(si)); }    
       ;

instIter   : WHILE_
              {
              $<aux>$.valor = si;

              } 
              APAR_ expre CPAR_
              {

                     if($4.t != T_ERROR && $4.t != T_LOGICO)
                     {
                            yyerror("La expresion a evaluar no es una expresion logica.");
                     }
                     $<aux>$.valor = creaLans(si);
                     emite(EIGUAL, crArgPos(niv, $4.d), crArgEnt(0), crArgEtq(-1));
              }
              inst
              {
                     emite(GOTOS, crArgNul(), crArgNul(), crArgEtq($<aux>2.valor)); 
                     completaLans($<aux>6.valor, crArgEtq(si));
              }              
       ;

expre   : expreLogic {$$.t = $1.t; $$.d = $1.d;}
       | ID_ ASIG_ expre
              {
              $$.t=T_ERROR;
              SIMB simb = obtTdS($1);
              if(!($3.t==T_ERROR)){
                  if (simb.t == T_ERROR) yyerror("Objeto no declarado");
                  else if (! (((simb.t == T_ENTERO) && ($3.t == T_ENTERO)) ||
                         ((simb.t == T_LOGICO) && ($3.t == T_LOGICO))) )
                  yyerror("Error de tipos en la instrucción de asignación");
                  }else {$$.t=$3.t;}
                  emite(EASIG,crArgPos(niv,$3.d),crArgNul(),crArgPos(niv,simb.d));
              }
       | ID_ ACOR_ expre CCOR_ ASIG_ expre
       {
              $$.t=T_ERROR;
              SIMB simb = obtTdS($1);
              DIM dim;
              if (simb.t != T_ARRAY) {
                yyerror("La variable no es un vector, no se puede acceder mediante indices.");
              } else {
                dim = obtTdA(simb.ref);
              
                if ($3.t != T_ERROR && $6.t != T_ERROR) {
                  if (simb.t == T_ERROR) {
                      yyerror("No existe ninguna variable con ese identificador.");
                  } else if (! ($3.t == T_ENTERO)) {
                      yyerror("El indice debe ser un entero o positivo.");
                  } else if (! ($6.t == dim.telem)) {
                      yyerror("Error de tipos en la instrucción de asignación");
                  }else {$$.t=$6.t;}
              }
              
              }
              emite(EVA, crArgPos(simb.n, simb.d) , crArgPos(niv, $3.d), crArgPos(niv, $6.d));
       }
       | ID_ PUNTO_ ID_ ASIG_ expre
       {
              $$.t=T_ERROR;
              SIMB simb = obtTdS($1);
              CAMP reg;
              if (simb.t != T_RECORD) {
                 yyerror("La variable no es un registro.");
              } else {
                 reg = obtTdR(simb.ref, $3);
              }

              if (reg.t != T_ERROR && $5.t != T_ERROR) {
                if (simb.t == T_ERROR) {
                    yyerror("No existe ninguna variable con ese identificador.");
                } else if (! ($5.t == reg.t)) {
                    yyerror("Error de tipos en la instrucción de asignación");
                }else{$$.t=$5.t;}
            }
       }
       ;

expreLogic   : expreIgual {$$.t = $1.t; $$.d = $1.d;}
       | expreLogic opLogic expreIgual
       {    $$.t = T_ERROR;
            if(!($1.t==T_ERROR)){
              if ($1.t != T_ERROR || $3.t != T_ERROR) {
                     if (!($1.t == $3.t && $1.t == T_LOGICO)) {
                            yyerror("Incompatibilidad de tipos.(Expresión Lógica)");
                     } else {
                            $$.t = T_LOGICO;
                     }
              }
            }
             $$.d = creaVarTemp();
              if ($2 == EMULT) { 
                     emite(EMULT, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));
              }else {
                     emite(ESUM, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));
                     emite(EMENEQ, crArgPos(niv, $$.d), crArgEnt(1), crArgEtq(si+2));
                     emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv, $$.d));
              }
       }
       ;

expreIgual   : expreRel {$$.t = $1.t; $$.d = $1.d;}
       | expreIgual opIgual expreRel
              {
                     $$.t = T_ERROR;
                     if ($1.t != T_ERROR && $3.t != T_ERROR)
                     {
                            if ($1.t != $3.t )
                            {
                                   yyerror("Tipos incompatibles");
                            }
                            else if ($3.t != T_LOGICO || $3.t != T_ENTERO)
                            {
                                   yyerror("La expresión no es lógica o entera");
                            }
                            else {
                                   $$.t = T_LOGICO;
                            }
                     }
                     emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv, $$.d));
                     emite($2, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgEtq(si + 2));
                     emite(EASIG, crArgEnt(0), crArgNul(), crArgPos(niv, $$.d));
              }
       ;

expreRel   : expreAd {$$.t = $1.t; $$.d = $1.d;}
       | expreRel opRel expreAd
       {
              $$.t = T_ERROR;
              if ($1.t != T_ERROR && $3.t != T_ERROR)
              {
                     if (($1.t == $3.t && $1.t == T_ENTERO)) {
              $$.t = T_LOGICO;
                     }
                     else {yyerror("Alguno de ellos no es de tipo entero.");}
              }
              $$.d = creaVarTemp();
              emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv, $$.d));
              emite($2, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgEtq(si + 2));
              emite(EASIG, crArgEnt(0), crArgNul(), crArgPos(niv, $$.d));
       }
       ;

expreAd   : expreMul { $$.t = $1.t; $$.d = $1.d; }
       | expreAd opAd expreMul
       {
              $$.t = T_ERROR;
              if (($1.t == T_ENTERO) && ($3.t == T_ENTERO)) $$.t = T_ENTERO;
              else yyerror("Error de tipos en la expresion aditiva");
              $$.d = creaVarTemp();
              /***************** Expresion a partir de un operador aritmetico */
              emite($2, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));

       }
       ;

expreMul   : expreUna  {$$.t = $1.t; $$.d = $1.d;}
       | expreMul opMul expreUna
       {
              $$.t = T_ERROR;
              if ($1.t != T_ERROR && $3.t != T_ERROR) {
			if (($1.t == $3.t && $1.t == T_ENTERO)) {
					$$.t = T_ENTERO;
                            }
                     else {yyerror("Alguno de ellos no es de tipo entero.");}
              }

       $$.d = creaVarTemp();
       emite($2, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));
       }
       ;

expreUna   : expreSufi {$$.t = $1.t; $$.d = $1.d;}
       | opUna expreUna
       {
              $$.t = T_ERROR;
              if($2.t == T_ENTERO){
                     if ($1 == OP_NOT) {
                            yyerror("No se puede aplicar la operacion not si no es tipo logico.");
                     }
                     else {$$.t=$2.t;}
              }
              else if ($2.t == T_LOGICO) {
                            if ($1 == OP_MAS || $1 == OP_MENOS)
                            {
                                   yyerror("No se puede aplicar la operacion suma o resta si no es tipo entero.");
                            }
                            else {$$.t = T_LOGICO;}
              }
              else
              {
                     yyerror("No es de tipo logico o entero.");
              }
              $$.d = creaVarTemp();
              if ($1 == ESIG) {
                emite(EDIF, crArgEnt(1), crArgPos(niv, $2.d), crArgPos(niv, $$.d));    
              } else {
                emite($1, crArgEnt(0), crArgPos(niv, $2.d), crArgPos(niv, $$.d));
              }
       }
       | opIncre ID_
       {
              SIMB simb = obtTdS($2);
              $$.t=T_ERROR;

              if(simb.t == T_ERROR)
              {
                     yyerror("El identificador no se encuentra registrado en la tabla de símbolos.");
              }
              else if(simb.t != T_ENTERO)
              {
                     yyerror("El tipo no es entero y no puede aplicarsele una operacion incremental.");
              }
              else{
                     $$.t = simb.t;
              }
              $$.d = creaVarTemp();
              emite($1, crArgPos(simb.n, simb.d), crArgEnt(1), crArgPos(simb.n, simb.d));
              emite(EASIG, crArgPos(simb.n, simb.d), crArgNul(), crArgPos(niv, $$.d));
       }
       ;

expreSufi   : const {$$.t=$1.t;
       $$.d = creaVarTemp();
       emite(EASIG, crArgEnt($1.d), crArgNul(), crArgPos(niv, $$.d)); 
       }
       | APAR_ expre CPAR_ {$$.t = $2.t; $$.d = $2.d;}
       | ID_ {
              SIMB simb = obtTdS($1);
              $$.t = T_ERROR;
              if(simb.t == T_ERROR)
              {
                     yyerror("El identificador no se encuentra registrado en la tabla de símbolos.");
              }
              else {$$.t = simb.t;}
              $$.d = creaVarTemp();
              emite(EASIG, crArgPos(niv, simb.d), crArgNul(), crArgPos(niv, $$.d));
       }
       | ID_ opIncre {
              SIMB simb = obtTdS($1);
              $$.t=T_ERROR;
              if(simb.t == T_ERROR)
              {
                     yyerror("El identificador no se encuentra registrado en la tabla de símbolos.");
              }
              else if (simb.t == T_ENTERO)
              {
                     $$.t = simb.t;
              }
              else
              {
                     yyerror("El tipo no es entero y no puede aplicarsele una operacion incremental.");
              }
              $$.d = creaVarTemp();
              emite(EASIG, crArgPos(simb.n, simb.d), crArgNul(), crArgPos(niv, $$.d)); 
              emite($2, crArgPos(simb.n, simb.d), crArgEnt(1), crArgPos(simb.n, simb.d));
       }
       | ID_ PUNTO_ ID_ {
              SIMB simb = obtTdS($1);
              $$.t = T_ERROR;
              if(simb.t == T_ERROR || simb.t != T_RECORD)
                     {
                            yyerror("El identificador no pertenece a ningun simbolo.");
                     }  else
                     {
                            CAMP c = obtTdR(simb.ref,$3);
                            if(c.t == T_ERROR){
                                   yyerror("Campo no existe");
                            }else{
                                   $$.t = c.t;
                            }

                     }
              }
       | ID_ ACOR_ expre CCOR_ {
              SIMB simb = obtTdS($1);
              $$.t = T_ERROR;
              if(simb.t == T_ERROR)
              {
                     yyerror("El identificador no pertenece a ningun simbolo.");
              }
              else if($3.t != T_ENTERO)
              {
                     yyerror("El indice no es de tipo entero, o no es positivo.");
              }
              else
              {
                     DIM dim = obtTdA(simb.ref);
                     $$.t = dim.telem;
              }
              $$.d = creaVarTemp();
              emite(EAV, crArgPos(simb.n, simb.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));
       }
       | ID_ APAR_
       {
       emite(INCTOP, crArgNul(), crArgNul(), crArgEnt(TALLA_TIPO_SIMPLE));
       } 
       paramAct CPAR_ {
              SIMB simb = obtTdS($1);
              $$.t = T_ERROR;
              INF inf = obtTdD(simb.ref);
              if(simb.t == T_ERROR)
              {
                     yyerror("El identificador no tiene correspondencia.");
              } else{
                  
                  if (inf.tipo == T_ERROR)
                  {
                         yyerror("El identificador no tiene correspondencia.");
                  }
                     else if (!(cmpDom(simb.ref, $4))) {
                         yyerror("Error en el dominio de los parametros actuales");
                  } else {
                          $$.t = inf.tipo;
                  }
       }
       
       emite(CALL, crArgNul(), crArgNul(), crArgEtq(simb.d)); 
       emite(DECTOP, crArgNul(), crArgNul(), crArgEnt(inf.tsp)); 
       $$.d = creaVarTemp();
       emite(EPOP, crArgNul(), crArgNul(), crArgPos(niv, $$.d));
       }
       ;

const   : CTE_  {$$.t=T_ENTERO; $$.d = $1;}
       | TRUE_  {$$.t=T_LOGICO; $$.d = 1;}
       | FALSE_ {$$.t=T_LOGICO; $$.d = 0;}
       ;

paramAct   : { $$ = insTdD(-1,T_VACIO); }
       | listParamAct { $$ = $1; }
       ;

listParamAct   : expre
        { $$ = insTdD(-1,$1.t);
       emite(EPUSH, crArgNul(), crArgNul(), crArgPos(niv, $1.d)); }
       | expre COMA_ 
       {emite(EPUSH, crArgNul(), crArgNul(), crArgPos(niv, $1.d));}
       listParamAct
        { $$ = insTdD($4,$1.t);}
       ;

opLogic   : AND_ {$$ = EMULT;}
       | OR_ {$$ = ESUM;}
       ;

opIgual   : IGUAL_ {$$ = EIGUAL;}
       | DISTINTO_ {$$ = EDIST;}
       ;

opRel   : MAYOR_ {$$ = EMAY;}
       | MENOR_ {$$ = EMEN;}
       | MAYORIG_ {$$ = EMAYEQ;}
       | MENORIG_ {$$ = EMENEQ;}
       ;

opAd   : MAS_ {$$ = ESUM;}
       | MENOS_ {$$ = EDIF;}
       ;

opMul   : POR_ {$$ = EMULT;}
       | DIV_ {$$ = EDIVI;}
       ;

opUna   : MAS_ {$$ = ESUM;}
       | MENOS_ {$$ = EDIF;}
       | NOT_ {$$ = ESIG;}
       ;

opIncre   : INCRE_ {$$ = ESUM;}
       | DECRE_ {$$ = EDIF;}
       ;

%%
