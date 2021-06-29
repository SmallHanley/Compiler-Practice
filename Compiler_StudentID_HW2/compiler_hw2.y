/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    #define YYDEBUG 1
    int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    typedef struct symbol {
        int index;
        char *name;
        char *type;
        int address;
        int lineno;
        char *elem_type;
    }symbol_t;

    symbol_t table[10][20];
    int table_size[10];

    int level = 0;
    int address = 0;

    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }


    /* Symbol table function - you can add new function if needed. */
    static void create_symbol();
    static void insert_symbol(char *name, char *type, char *elem_type);
    static symbol_t lookup_symbol(char *name);
    static void dump_symbol();
    static int check_redeclared(char *name);
    static bool isUndefined(char *name);
%}

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    /* ... */
}

/* Token without return */
%token ADD SUB MUL QUO REM INC DEC 
%token GTR LSS GEQ LEQ EQL NEQ
%token ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token AND OR NOT LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE SEMICOLON COMMA
%token COMMENT PRINT RETURN IF ELSE FOR WHILE
%token TRUE FALSE CONTINUE BREAK
%token INT FLOAT STRING BOOL

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <s_val> IDENT

/* Nonterminal with return, which need to sepcify type */
%type <s_val> Type Literal ExpressionStmt
%type <s_val> UnaryOp AssignOp
%type <s_val> LogicalOrOp LogicalAndOp ComparisonOp AdditionOp MultiplicationOp
%type <s_val> PrimaryExpression Operand IndexExprression ConversionExpression
%type <s_val> LogicalOrExpression LogicalAndExpression ComparisonExpression
%type <s_val> AdditionExpression MultiplicationExpression UnaryExpression

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program 
    : StatementList
;

StatementList
    : Statement StatementList
    | Statement
    | LBRACE Statement RBRACE
;

Statement
    : DeclarationStmt SEMICOLON
    | ExpressionStmt SEMICOLON
    | AssignmentStmt SEMICOLON
    | IncDecStmt
    | Block
    | IfStmt
    | WhileStmt
    | ForStmt
    | PrintStmt
;

ExpressionStmt
    : LogicalOrExpression
;

DeclarationStmt
    : Type IDENT { 
        int check = check_redeclared($2);
        if (check < 1) {
            insert_symbol($2, $1, "-");
        }
        else {
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n",
                yylineno, $2, check);
        }
    }
    | Type IDENT ASSIGN ExpressionStmt {
        int check = check_redeclared($2);
        if (check < 1) {
            insert_symbol($2, $1, "-");
        }
        else {
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n",
                yylineno, $2, check);
        }
    }
    | Type IDENT LBRACK ExpressionStmt RBRACK { 
        int check = check_redeclared($2);
        if (check < 1) {
            insert_symbol($2, "array", $1);
        }
        else {
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n",
                yylineno, $2, check);
        }
    }
    | Type IDENT LBRACK ExpressionStmt RBRACK ASSIGN ExpressionStmt { 
        int check = check_redeclared($2);
        if (check < 1) {
            insert_symbol($2, "array", $1);
        }
        else {
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n",
                yylineno, $2, check);
        }
    }

AssignmentStmt
    : ExpressionStmt AssignOp ExpressionStmt {
        if (strcmp($1, $3)) {
            printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n",
                yylineno, $2, $1, $3);
        }
        printf("%s\n", $2); 
    }
    | INT_LIT AssignOp IDENT {
        printf("INT_LIT %d\n", $1);
        printf("IDENT (name=%s, address=%d)\n", $3, lookup_symbol($3).address);
        printf("error:%d: cannot assign to int\n", yylineno);
        printf("%s\n", $2);
    }
;

IncDecStmt
    : IncDecExpression SEMICOLON
;

Block
    : BlockBegin StatementList RBRACE {
        dump_symbol();
        table_size[level] = 0;
        level--;
    }
;

BlockBegin
    : LBRACE { level++; }
;

IfStmt
    : IfBegin RPAREN Block
    | IfBegin RPAREN Block ELSE IfStmt
    | IfBegin RPAREN Block ELSE Block
;

IfBegin
    : IF LPAREN ExpressionStmt {
        if (strcmp($3, "bool")) {
            printf("error:%d: non-bool (type %s) used as for condition\n",
                yylineno + 1, $3);
        }
    }
;

WhileStmt
    : WhileBegin RPAREN Block
;

WhileBegin
    : WHILE LPAREN ExpressionStmt {
        if (strcmp($3, "bool")) {
            printf("error:%d: non-bool (type %s) used as for condition\n",
                yylineno + 1, $3);
        }
    }
;

ForStmt
    : FOR LPAREN ForClause RPAREN Block
;

ForClause
    : InitStmt SEMICOLON ExpressionStmt SEMICOLON PostStmt
;

InitStmt
    : AssignmentStmt
    | ExpressionStmt
    | IncDecExpression
;

PostStmt
    : AssignmentStmt
    | ExpressionStmt
    | IncDecExpression
;

PrintStmt
    : PRINT LPAREN ExpressionStmt RPAREN SEMICOLON { 
        printf("PRINT %s\n", $3);
    }
;

UnaryExpression
    : PrimaryExpression
    | UnaryOp UnaryExpression { 
        $$ = $2;
        printf("%s\n", $1); 
    }
;

PrimaryExpression
    : Operand
    | IndexExprression
    | ConversionExpression
;

IndexExprression
    : PrimaryExpression LBRACK ExpressionStmt RBRACK
;

ConversionExpression
    : LPAREN Type RPAREN UnaryExpression {
        $$ = $2;
        char *type, *new_type;
        if (strcmp($2, "int")) {
            new_type = "I";
        }
        else if (strcmp($2, "float")) {
            new_type = "F";
        }
        if (strcmp($4, "int")) {
            type = "I";
        }
        else if (strcmp($4, "float")) {
            type = "F";
        }
        printf("%s to %s\n", new_type, type);
    }
;

IncDecExpression
    : ExpressionStmt INC { printf("INC\n"); }
    | ExpressionStmt DEC { printf("DEC\n"); }
;

LogicalOrExpression
    : LogicalAndExpression
    | LogicalOrExpression LogicalOrOp LogicalAndExpression { 
        if (strcmp($1, "bool")) {
            printf("error:%d: invalid operation: (operator OR not defined on %s)\n",
                yylineno, $1);
        }
        else if (strcmp($3, "bool")) {
            printf("error:%d: invalid operation: (operator OR not defined on %s)\n",
                yylineno, $3);
        }
        printf("%s\n", $2); 
    }
;

LogicalAndExpression
    : ComparisonExpression
    | LogicalAndExpression LogicalAndOp ComparisonExpression { 
        if (strcmp($1, "bool")) {
            printf("error:%d: invalid operation: (operator AND not defined on %s)\n",
                yylineno, $1);
        }
        else if (strcmp($3, "bool")) {
            printf("error:%d: invalid operation: (operator AND not defined on %s)\n",
                yylineno, $3);
        }
        printf("%s\n", $2); 
    }
;

ComparisonExpression
    : AdditionExpression
    | ComparisonExpression ComparisonOp AdditionExpression { 
        $$ = "bool";
        printf("%s\n", $2); 
    }
;

AdditionExpression
    : MultiplicationExpression
    | AdditionExpression AdditionOp MultiplicationExpression { 
        if (strcmp($1, $3)) {
            printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n",
                yylineno, $2, $1, $3);
        }
        printf("%s\n", $2); 
    }
;

MultiplicationExpression
    : UnaryExpression
    | MultiplicationExpression MultiplicationOp UnaryExpression { 
        if (strcmp($2, "REM") == 0) {
            if (strcmp($1, "float") == 0 || strcmp($3, "float") == 0) {
                printf("error:%d: invalid operation: (operator REM not defined on float)\n",
                    yylineno);
            }
        }
        printf("%s\n", $2); 
    }
;

Operand
    : Literal
    | IDENT {
        if(isUndefined($1)) {
            printf("error:%d: undefined: %s\n", yylineno, $1);
            $$ = "int";
        }
        else {
            printf("IDENT (name=%s, address=%d)\n", $1, lookup_symbol($1).address);
            if (!strcmp(lookup_symbol($1).type, "array")) {
                $$ = lookup_symbol($1).elem_type;
            }
            else {
                $$ = lookup_symbol($1).type;
            }
        }
    }
    | LPAREN ExpressionStmt RPAREN { $$ = $2; }
;

AssignOp
    : ASSIGN { $$ = "ASSIGN"; }
    | ADD_ASSIGN { $$ = "ADD_ASSIGN"; }
    | SUB_ASSIGN { $$ = "SUB_ASSIGN"; }
    | MUL_ASSIGN { $$ = "MUL_ASSIGN"; }
    | QUO_ASSIGN { $$ = "QUO_ASSIGN"; }
    | REM_ASSIGN { $$ = "REM_ASSIGN"; }
;

UnaryOp
    : ADD { $$ = "POS"; };
    | SUB { $$ = "NEG"; };
    | NOT { $$ = "NOT"; };
;

LogicalOrOp
    : OR { $$ = "OR"; }
;

LogicalAndOp
    : AND { $$ = "AND"; }
;

ComparisonOp
    : GTR { $$ = "GTR"; }
    | LSS { $$ = "LSS"; }
    | GEQ { $$ = "GEQ"; }
    | LEQ { $$ = "LEQ"; }
    | EQL { $$ = "EQL"; }
    | NEQ { $$ = "NEQ"; }
;

AdditionOp
    : ADD { $$ = "ADD"; }
    | SUB { $$ = "SUB"; }
;

MultiplicationOp
    : MUL { $$ = "MUL"; }
    | QUO { $$ = "QUO"; }
    | REM { $$ = "REM"; }
;

Type
    : INT { $$ = "int"; }
    | FLOAT { $$ = "float"; }
    | STRING { $$ = "string"; }
    | BOOL { $$ = "bool"; }
;

Literal
    : INT_LIT { printf("INT_LIT %d\n", $1); $$ = "int"; }
    | FLOAT_LIT { printf("FLOAT_LIT %f\n", $1); $$ = "float"; }
    | STRING_LIT { printf("STRING_LIT %s\n", $1); $$ = "string"; }
    | TRUE { printf("TRUE\n"); $$ = "bool"; }
    | FALSE { printf("FALSE\n"); $$ = "bool"; }
;

%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    create_symbol();
    yyparse();
    dump_symbol();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}

static void create_symbol() {
    memset(table_size, 0, sizeof(table_size));
}

static void insert_symbol(char *name, char *type, char *elem_type) {
    printf("> Insert {%s} into symbol table (scope level: %d)\n", name, level);
    table[level][table_size[level]].index = table_size[level];
    table[level][table_size[level]].name = name;
    table[level][table_size[level]].type = type;
    table[level][table_size[level]].address = address++;
    table[level][table_size[level]].lineno = yylineno;
    table[level][table_size[level]].elem_type = elem_type;
    table_size[level]++;
}

static symbol_t lookup_symbol(char *name) {
    for(int i = level; i >= 0; i--){
        for (int j = 0; j < table_size[i]; j++) {
            if (strcmp(table[i][j].name, name) == 0){
                return table[i][j];
            }
        }
    }
    exit(1);
}

static void dump_symbol() {
    printf("> Dump symbol table (scope level: %d)\n", level);
    printf("%-10s%-10s%-10s%-10s%-10s%-10s\n",
           "Index", "Name", "Type", "Address", "Lineno", "Element type");
    for (int i = 0; i < table_size[level]; i++) {
        printf("%-10d%-10s%-10s%-10d%-10d%s\n",
           table[level][i].index,
           table[level][i].name,
           table[level][i].type,
           table[level][i].address,
           table[level][i].lineno,
           table[level][i].elem_type);
    }
}

static int check_redeclared(char *name) {
    for (int i = 0; i < table_size[level]; i++) {
        if (strcmp(table[level][i].name, name) == 0){
            return table[level][i].lineno;
        }
    }
    return -1;
}

static bool isUndefined(char *name) {
    for(int i = level; i >= 0; i--){
        for (int j = 0; j < table_size[i]; j++) {
            if (strcmp(table[i][j].name, name) == 0){
                return false;
            }
        }
    }
    return true;
}