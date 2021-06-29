/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    #define codegen(...) \
        do { \
            for (int i = 0; i < INDENT; i++) { \
                fprintf(fout, "\t"); \
            } \
            fprintf(fout, __VA_ARGS__); \
        } while (0)

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;
    
    /* Other global variables */
    FILE *fout = NULL;
    bool HAS_ERROR = false;
    int INDENT = 0;

    /* User define variavles */
    typedef struct symbol {
        int index;
        char *name;
        char *type;
        int address;
        int lineno;
        char *elem_type;
        struct symbol *next;
    } symbol_t;

    typedef struct table_node {
        int index;
        symbol_t *node;
        struct table_node *prev;
        struct table_node *next;
    } table_node_t;

    table_node_t *table;

    int level = 0;
    int address = 0;
    int label = 0;
    int loop = 0;
    int loopM = 0;
    int iflabel = 0;
    int iflabelM = 0;
    char buf[500][50];
    int idx = 0;
    
    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    /* Symbol table function - you can add new function if needed. */
    static symbol_t* create_symbol(char *name, char *type, char *elem_type);
    static void insert_symbol(symbol_t *node);
    static symbol_t* lookup_symbol(char *name);
    static void dump_symbol();
    static int check_redeclared(char *name);
    static bool isUndefined(char *name);
    static void table_level_up();
    static void table_level_down();
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
%type <s_val> UnaryOp AssignOp AssignProcess
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
            symbol_t *new_symbol = create_symbol($2, $1, "-");
            insert_symbol(new_symbol);
            if (!strcmp($1, "int")) {
                codegen("ldc 0\n");
                codegen("istore %d\n", address - 1);
            }
            else if (!strcmp($1, "float")) {
                codegen("ldc 0.0\n");
                codegen("fstore %d\n", address - 1);
            }
            else if (!strcmp($1, "string")) {
                codegen("ldc \"\"\n");
                codegen("astore %d\n", address - 1);
            } 
        }
        else {
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n",
                yylineno, $2, check);
            HAS_ERROR = true;
        }
    }
    | Type IDENT ASSIGN ExpressionStmt {
        int check = check_redeclared($2);
        if (check < 1) {
            symbol_t *new_symbol = create_symbol($2, $1, "-");
            insert_symbol(new_symbol);
            if (!strcmp($1, "int")) {
                codegen("istore %d\n", address - 1);
            }
            else if (!strcmp($1, "float")) {
                codegen("fstore %d\n", address - 1);
            } 
            else if (!strcmp($1, "string")) {
                codegen("astore %d\n", address - 1);
            }
            else if (!strcmp($1, "bool")) {
                codegen("istore %d\n", address - 1);
            } 
        }
        else {
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n",
                yylineno, $2, check);
            HAS_ERROR = true;
        }
    }
    | Type IDENT LBRACK ExpressionStmt RBRACK { 
        int check = check_redeclared($2);
        if (check < 1) {
            symbol_t *new_symbol = create_symbol($2, "array", $1);
            insert_symbol(new_symbol);
            codegen("newarray %s\n", $1);
            codegen("astore %d\n", address - 1);
        }
        else {
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n",
                yylineno, $2, check);
            HAS_ERROR = true;
        }
    }
    | Type IDENT LBRACK ExpressionStmt RBRACK ASSIGN ExpressionStmt { 
        int check = check_redeclared($2);
        if (check < 1) {
            symbol_t *new_symbol = create_symbol($2, "array", $1);
            insert_symbol(new_symbol);
        }
        else {
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n",
                yylineno, $2, check);
            HAS_ERROR = true;
        }
    }

AssignmentStmt
    : AssignProcess ExpressionStmt {
        char str[10], op[10];
        int addr;
        sscanf($1, "%d %s %s", &addr, str, op);
        if (!strcmp(str, "int")) {
            if (strcmp(op, "ASSIGN")) {
                codegen("i%s\n", op);
            }
            codegen("istore %d\n", addr);
        }
        else if (!strcmp(str, "float")) {
            if (strcmp(op, "ASSIGN")) {
                codegen("f%s\n", op);
            }
            codegen("fstore %d\n", addr);
        }
        else if (!strcmp(str, "string")) {
            codegen("astore %d\n", addr);
        }
        else if (!strcmp(str, "bool")) {
            codegen("istore %d\n", addr);
        }
    }
    | INT_LIT AssignOp IDENT {
        printf("INT_LIT %d\n", $1);
        printf("IDENT (name=%s, address=%d)\n", $3, lookup_symbol($3)->address);
        printf("error:%d: cannot assign to int\n", yylineno);
        printf("%s\n", $2);
        HAS_ERROR = true;
    }
    | PrimaryExpression LBRACK ExpressionStmt RBRACK AssignOp ExpressionStmt {
        char str[10];
        int addr;
        sscanf($1, "%d %s", &addr, str);
        if (!strcmp(str, "int")) {
            codegen("iastore\n");
        }
        else if (!strcmp(str, "float")) {
            codegen("fastore\n");
        }
    }
;

AssignProcess
    : IDENT AssignOp {
        if (isUndefined($1)) {
            printf("error:%d: undefined: %s\n", yylineno, $1);
            HAS_ERROR = true;
        }
        else {
            if (!strcmp(lookup_symbol($1)->type, "int")) {
                if (strcmp($2, "ASSIGN")) {
                    codegen("iload %d\n", lookup_symbol($1)->address);
                }
            }
            else if (!strcmp(lookup_symbol($1)->type, "float")) {
                if (strcmp($2, "ASSIGN")) {
                    codegen("fload %d\n", lookup_symbol($1)->address);
                }
            }
            sprintf(buf[idx], "%d %s %s", lookup_symbol($1)->address, lookup_symbol($1)->type, $2);
            $$ = buf[idx++];
        }
    }
;

IncDecStmt
    : IncDecExpression SEMICOLON
;

Block
    : BlockBegin StatementList RBRACE {
        dump_symbol();
        table_level_down();
        level--;
    }
;

BlockBegin
    : LBRACE { 
        table_level_up();
        level++; 
    }
;

IfStmt
    : IfBlock {
        fprintf(fout, "L_if_exit_%d:\n", iflabel);
        iflabel--;
    }
    | IfBlock ELSE IfStmt {
        fprintf(fout, "L_if_exit_%d:\n", iflabel);
        iflabel--;
    }
    | IfBlock ELSE Block {
        fprintf(fout, "L_if_exit_%d:\n", iflabel);
        iflabel--;
    }
;

IfBlock
    : IfBegin RPAREN Block {
        codegen("goto L_if_exit_%d\n", iflabel);
        fprintf(fout, "L_if_false_%d:\n", iflabel);
    }
;

IfBegin
    : IF LPAREN ExpressionStmt {
        if (strcmp($3, "-1 bool")) {
            printf("error:%d: non-bool (type %s) used as for condition\n",
                yylineno + 1, $3);
            HAS_ERROR = true;
        }
        iflabel = iflabelM++;
        codegen("ifeq L_if_false_%d\n", iflabel);
    }
;

WhileStmt
    : WhileBegin RPAREN Block {
        codegen("goto Loop_%d\nLoop_exit_%d:\n", loop, loop);
        loop--;
    }
;

WhileBegin
    : PreWhile LPAREN ExpressionStmt {
        if (strcmp($3, "-1 bool")) {
            printf("error:%d: non-bool (type %s) used as for condition\n",
                yylineno + 1, $3);
            HAS_ERROR = true;
        }
        codegen("ifeq Loop_exit_%d\n", loop);
    }
;

PreWhile
    : WHILE { 
        loop = loopM++;
        fprintf(fout, "Loop_%d:\n", loop); 
    }
;

ForStmt
    : FOR LPAREN ForClause PostFor RPAREN Block {
        codegen("goto L%d\n", label - 2);
        fprintf(fout, "Loop_exit_%d:\n", loop);
        loop--;
    }
;

ForClause
    : PreFor SEMICOLON ExpressionStmt SEMICOLON {
        if (strcmp($3, "-1 bool")) {
            printf("error:%d: non-bool (type %s) used as for condition\n",
                yylineno + 1, $3);
            HAS_ERROR = true;
        }
        codegen("ifeq Loop_exit_%d\n", loop);
        codegen("goto L%d\n", label + 1);
        fprintf(fout, "L%d:\n", label++);
    }
;

PreFor
    : InitStmt {
        loop = loopM++;
        fprintf(fout, "Loop_%d:\n", loop); 
    }
;

InitStmt
    : AssignmentStmt
    | ExpressionStmt
    | IncDecExpression
;

PostFor
    : PostStmt {
        codegen("goto Loop_%d\n", loop);
        fprintf(fout, "L%d:\n", label++);
    }
;

PostStmt
    : AssignmentStmt
    | ExpressionStmt
    | IncDecExpression
;

PrintStmt
    : PRINT LPAREN ExpressionStmt RPAREN SEMICOLON { 
        char str[10];
        int addr;
        sscanf($3, "%d %s", &addr, str);
        if (!strcmp(str, "int")) {
            codegen("getstatic java/lang/System/out Ljava/io/PrintStream;\n\tswap\n\tinvokevirtual java/io/PrintStream/print(I)V\n");
        }
        else if (!strcmp(str, "float")) {
            codegen("getstatic java/lang/System/out Ljava/io/PrintStream;\n\tswap\n\tinvokevirtual java/io/PrintStream/print(F)V\n");
        } 
        else if (!strcmp(str, "string")) {
            codegen("getstatic java/lang/System/out Ljava/io/PrintStream;\n\tswap\n\tinvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
        }
        else if (!strcmp(str, "bool")) {
            codegen("ifne L%d\n", label);
            codegen("ldc \"false\"\n");
            codegen("goto L%d\nL%d:\n", label + 1, label);
            codegen("ldc \"true\"\nL%d:\n", ++label);
            label++;
            codegen("getstatic java/lang/System/out Ljava/io/PrintStream;\n\tswap\n\tinvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
        } 
    }
;

UnaryExpression
    : PrimaryExpression
    | UnaryOp UnaryExpression { 
        $$ = $2;
        char str[10];
        int addr;
        sscanf($2, "%d %s", &addr, str);
        if (!strcmp($1, "NEG")) {
            if (!strcmp(str, "int")) {
                codegen("ineg\n");
            }
            else if (!strcmp(str, "float")) {
                codegen("fneg\n");
            }
        }
        else if (!strcmp($1, "NOT")) {
            codegen("iconst_1\n\tixor\n");
        }
    }
;

PrimaryExpression
    : Operand
    | IndexExprression
    | ConversionExpression
;

IndexExprression
    : PrimaryExpression LBRACK ExpressionStmt RBRACK {
        char str[10];
        int addr;
        sscanf($1, "%d %s", &addr, str);
        if (!strcmp(str, "int")) {
            codegen("iaload\n");
        }
        else if (!strcmp(str, "float")) {
            codegen("faload\n");
        }
    }
;

ConversionExpression
    : LPAREN Type RPAREN UnaryExpression {
        char str[50];
        sprintf(buf[idx], "%d %s", -1, $2);
        $$ = buf[idx++];
        int addr;
        sscanf($4, "%d %s", &addr, str);
        if (!strcmp($2, "int") && !strcmp(str, "float")) {
            codegen("f2i\n");
        }
        else if (!strcmp($2, "float") && !strcmp(str, "int")) {
            codegen("i2f\n");
        }
    }
;

IncDecExpression
    : ExpressionStmt INC {
        char str[10];
        int addr;
        sscanf($1, "%d %s", &addr, str);
        if (!strcmp(str, "int")) {
            codegen("ldc 1\n\tiadd\n\tistore %d\n", addr);
        }
        else if (!strcmp(str, "float")) {
            codegen("ldc 1.0\n\tfadd\n\tfstore %d\n", addr);
        }
    }
    | ExpressionStmt DEC { 
        char str[10];
        int addr;
        sscanf($1, "%d %s", &addr, str);
        if (!strcmp(str, "int")) {
            codegen("ldc 1\n\tisub\n\tistore %d\n", addr);
        }
        else if (!strcmp(str, "float")) {
            codegen("ldc 1.0\n\tfsub\n\tfstore %d\n", addr);
        }
    }
;

LogicalOrExpression
    : LogicalAndExpression
    | LogicalOrExpression LogicalOrOp LogicalAndExpression { 
        if (strcmp($1, "-1 bool")) {
            printf("error:%d: invalid operation: (operator OR not defined on %s)\n",
                yylineno, $1);
            HAS_ERROR = true;
        }
        else if (strcmp($3, "-1 bool")) {
            printf("error:%d: invalid operation: (operator OR not defined on %s)\n",
                yylineno, $3);
            HAS_ERROR = true;
        }
        codegen("ior\n");
    }
;

LogicalAndExpression
    : ComparisonExpression
    | LogicalAndExpression LogicalAndOp ComparisonExpression { 
        if (strcmp($1, "-1 bool")) {
            printf("error:%d: invalid operation: (operator AND not defined on %s)\n",
                yylineno, $1);
            HAS_ERROR = true;
        }
        else if (strcmp($3, "-1 bool")) {
            printf("error:%d: invalid operation: (operator AND not defined on %s)\n",
                yylineno, $3);
            HAS_ERROR = true;
        }
        codegen("iand\n");
    }
;

ComparisonExpression
    : AdditionExpression
    | ComparisonExpression ComparisonOp AdditionExpression { 
        $$ = "-1 bool";
        char str[10];
        int addr;
        sscanf($1, "%d %s", &addr, str);
        if (!strcmp(str, "int")) {
            codegen("isub\n");   
        } 
        else if (!strcmp(str, "float")) {
            codegen("fcmpl\n");
        } 
        codegen("%s L%d\n", $2, label);
        codegen("iconst_0\n");
        codegen("goto L%d\nL%d:\n", label + 1, label);
        codegen("iconst_1\nL%d:\n", ++label);
        label++;
    }
;

AdditionExpression
    : MultiplicationExpression
    | AdditionExpression AdditionOp MultiplicationExpression { 
        char str1[10], str2[10];
        int addr;
        sscanf($1, "%d %s", &addr, str1);
        sscanf($3, "%d %s", &addr, str2);
        if (strcmp(str1, str2)) {
            printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n",
                yylineno, $2, str1, str2);
            HAS_ERROR = true;
        }
        if (!strcmp(str1, "int")) {
            codegen("i%s\n", $2);
        }
        else if (!strcmp(str1, "float")) {
            codegen("f%s\n", $2);
        }
    }
;

MultiplicationExpression
    : UnaryExpression
    | MultiplicationExpression MultiplicationOp UnaryExpression { 
        char str1[10], str2[10];
        int addr;
        sscanf($1, "%d %s", &addr, str1);
        sscanf($3, "%d %s", &addr, str2);
        if (!strcmp($2, "rem")) {
            if (!strcmp(str1, "float") || !strcmp(str2, "float")) {
                printf("error:%d: invalid operation: (operator REM not defined on float)\n",
                    yylineno);
                HAS_ERROR = true;
            }
        }
        if (!strcmp(str1, "int")) {
            codegen("i%s\n", $2);
        }
        else if (!strcmp(str1, "float")) {
            codegen("f%s\n", $2);
        }
    }
;

Operand
    : Literal
    | IDENT {
        if (isUndefined($1)) {
            printf("error:%d: undefined: %s\n", yylineno, $1);
            HAS_ERROR = true;
            $$ = "int";
        }
        else {
            if (!strcmp(lookup_symbol($1)->type, "array")) {
                codegen("aload %d\n", lookup_symbol($1)->address);
                sprintf(buf[idx], "%d %s", lookup_symbol($1)->address, lookup_symbol($1)->elem_type);
                $$ = buf[idx++];
            }
            else {
                if (!strcmp(lookup_symbol($1)->type, "int")) {
                    codegen("iload %d\n", lookup_symbol($1)->address);
                }
                else if (!strcmp(lookup_symbol($1)->type, "float")) {
                    codegen("fload %d\n", lookup_symbol($1)->address);
                }
                else if (!strcmp(lookup_symbol($1)->type, "string")) {
                    codegen("aload %d\n", lookup_symbol($1)->address);
                }
                else if (!strcmp(lookup_symbol($1)->type, "bool")) {
                    codegen("iload %d\n", lookup_symbol($1)->address);
                }
                sprintf(buf[idx], "%d %s", lookup_symbol($1)->address, lookup_symbol($1)->type);
                $$ = buf[idx++];
            }
        }
    }
    | LPAREN ExpressionStmt RPAREN { $$ = $2; }
;

AssignOp
    : ASSIGN { $$ = "ASSIGN"; }
    | ADD_ASSIGN { $$ = "add"; }
    | SUB_ASSIGN { $$ = "sub"; }
    | MUL_ASSIGN { $$ = "mul"; }
    | QUO_ASSIGN { $$ = "div"; }
    | REM_ASSIGN { $$ = "rem"; }
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
    : GTR { $$ = "ifgt"; }
    | LSS { $$ = "iflt"; }
    | GEQ { $$ = "ifge"; }
    | LEQ { $$ = "ifle"; }
    | EQL { $$ = "ifeq"; }
    | NEQ { $$ = "ifne"; }
;

AdditionOp
    : ADD { $$ = "add"; }
    | SUB { $$ = "sub"; }
;

MultiplicationOp
    : MUL { $$ = "mul"; }
    | QUO { $$ = "div"; }
    | REM { $$ = "rem"; }
;

Type
    : INT { $$ = "int"; }
    | FLOAT { $$ = "float"; }
    | STRING { $$ = "string"; }
    | BOOL { $$ = "bool"; }
;

Literal
    : INT_LIT { codegen("ldc %d\n", $1); $$ = "-1 int"; }
    | FLOAT_LIT { codegen("ldc %f\n", $1); $$ = "-1 float"; }
    | STRING_LIT { codegen("ldc \"%s\"\n", $1); $$ = "-1 string"; }
    | TRUE { codegen("iconst_1\n"); $$ = "-1 bool"; }
    | FALSE { codegen("iconst_0\n"); $$ = "-1 bool"; }
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

    /* Codegen output init */
    char *bytecode_filename = "hw3.j";
    fout = fopen(bytecode_filename, "w");
    codegen(".source hw3.j\n");
    codegen(".class public Main\n");
    codegen(".super java/lang/Object\n");
    codegen(".method public static main([Ljava/lang/String;)V\n");
    codegen(".limit stack 100\n");
    codegen(".limit locals 100\n");
    INDENT++;

    table = malloc(sizeof(table_node_t));
    table->next = NULL;
    table->prev = NULL;
    table->node = NULL;
    table->index = 0;

    yyparse();
    
    dump_symbol();
    table_level_down();

	/* printf("Total lines: %d\n", yylineno); */

    /* Codegen end */
    codegen("return\n");
    INDENT--;
    codegen(".end method\n");
    fclose(fout);
    fclose(yyin);

    if (HAS_ERROR) {
        remove(bytecode_filename);
    }
    return 0;
}

static symbol_t* create_symbol(char *name, char *type, char *elem_type) {
    symbol_t *node = malloc(sizeof(symbol_t));
    node->name = name;
    node->type = type;
    node->address = address++;
    node->lineno = yylineno;
    node->elem_type = elem_type;
    node->next = NULL;
    return node;
}

static void insert_symbol(symbol_t *node) {
    printf("> Insert {%s} into symbol table (scope level: %d)\n", node->name, level);
    node->index = table->index++;
    symbol_t *tmp_node = table->node;
    if (table->node == NULL) {
        table->node = node;
        return;
    }
    while (tmp_node->next != NULL) {
        tmp_node = tmp_node->next;
    }
    tmp_node->next = node;
}

static symbol_t* lookup_symbol(char *name) {
    table_node_t *tmp_table = table;
    while (tmp_table != NULL) {
        symbol_t *tmp_node = tmp_table->node;
        while (tmp_node != NULL) {
            if (!strcmp(tmp_node->name, name)) {
                return tmp_node;
            }
            tmp_node = tmp_node->next;
        }
        tmp_table = tmp_table->prev;
    }
    exit(1);
}

static void dump_symbol() {
    printf("> Dump symbol table (scope level: %d)\n", level);
    printf("%-10s%-10s%-10s%-10s%-10s%-10s\n",
           "Index", "Name", "Type", "Address", "Lineno", "Element type");
    while (table->node != NULL) {
        printf("%-10d%-10s%-10s%-10d%-10d%s\n",
           table->node->index,
           table->node->name,
           table->node->type,
           table->node->address,
           table->node->lineno,
           table->node->elem_type);
        table->node = table->node->next;
    }
}

static int check_redeclared(char *name) {
    symbol_t *tmp_node = table->node;
    while (tmp_node != NULL) {
        if (!strcmp(tmp_node->name, name)) {
            return tmp_node->lineno;
        }
        tmp_node = tmp_node->next;
    }
    return -1;
}

static bool isUndefined(char *name) {
    table_node_t *tmp_table = table;
    while (tmp_table != NULL) {
        symbol_t *tmp_node = tmp_table->node;
        while (tmp_node != NULL) {
            if (!strcmp(tmp_node->name, name)) {
                return false;
            }
            tmp_node = tmp_node->next;
        }
        tmp_table = tmp_table->prev;
    }
    return true;
}

static void table_level_up() {
    table->next = malloc(sizeof(table_node_t));
    table->next->prev = table;
    table = table->next;
    table->next = NULL;
    table->node = NULL;
    table->index = 0;
}

static void table_level_down() {
    symbol_t *tmp_node = table->node;
    while (tmp_node != NULL) {
        symbol_t *free_node = tmp_node;
        tmp_node = tmp_node->next;
        free(free_node);
    }
    table_node_t *free_table_node = table;
    table = table->prev;
    if (table != NULL) {
        table->next = NULL;
    }
    free(free_table_node);
}