#ifndef PROIECT_SRC_H
#define PROIECT_SRC_H

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
extern int yylineno;

enum type
{
    INT,
    BOOL,
    FLOAT,
    CHAR,
    STRING
};

struct ExprInfo
{
    enum type valueType;
    int value;
    char *sValue;
    float fValue;
    char cValue;
    bool bValue;
};

struct exprList
{
    struct ExprInfo **exprArray;
    int exprCount;
};

struct symbolTableEntry
{
    char *name;
    int isArray;
    struct ExprInfo *arrayValues;
    int size;
    struct ExprInfo val;
};

struct symbolList
{
    struct symbolTableEntry **symbolTable;
    int symbolCount;
};

struct functionSymbolTableEntry
{
    enum type returnType;
    char *name;
    struct symbolList *parameters;
};

struct functionSymbolList
{
    struct functionSymbolTableEntry **functionSymbolTable;
    int functionsSymbolCount;
};

struct symbolList *programSymbols;
struct functionSymbolList *programFunctions;

char **output;
int outputLinesCount;

static int compilationStatus = 1;

// init
void init();

// functii pentru tabelul de simboluri
struct symbolTableEntry *createSymbolEntry(char *name);
struct symbolList *allocateSymbolList();
void addEntry(struct symbolList *list, struct symbolTableEntry *entry);
struct symbolList *createSymbolList(struct symbolTableEntry *entry);
void printSymbolList(struct symbolList *list);
void addSymbol(enum type symbolType, struct symbolList *fromSymbols, struct symbolList *toSymbols);

// functii pentru modificarea/accesarea elementului din tabel
void assignValue(char *identifier, struct ExprInfo *expr_ptr);
void addName(struct symbolTableEntry *entry, char *name);
void addType(struct symbolTableEntry *entry, char *valType);
struct ExprInfo *getExpr(char *identifier);
int getValue(char *identifier);
enum type getType(char *identifier);

// functii pentru tabelul de simboluri de functii
struct functionSymbolList *allocateFunctionSymbolList();
struct functionSymbolTableEntry *createFunctionSymbolEntry(char *name, enum type returnType, struct symbolList *parameters);
struct functionSymbolList *createFunctionSymbolList(struct functionSymbolTableEntry *entry);
void addFunctionEntry(struct functionSymbolList *list, struct functionSymbolTableEntry *entry);

// functii pentru expresii
void addExpr(struct exprList *list, struct ExprInfo *expr);
struct exprList *createExprList(struct ExprInfo *expr);
struct ExprInfo *createIntExpr(int val);
struct ExprInfo *createStrExpr(char *val);
struct ExprInfo *createFloatExpr(float val);
struct ExprInfo *createCharExpr(char val);
struct ExprInfo *createBoolExpr(int val);
struct ExprInfo *createExpr(char *identifier);
struct ExprInfo *createCompExpr(struct ExprInfo *f, char *op, struct ExprInfo *s);
void TypeOf(struct ExprInfo *expr);
void Eval(struct ExprInfo *expr);

// functii pentru erori de compilare
void printCompilationStatus();
int alreadyUsed(char *identifier);
int alreadyUsedFunction(struct functionSymbolTableEntry *function);
int alreadyUsedFunctionIdentifier(char *identifier);
void checkDeclaration(char *identifier);
void checkDoubleDeclaration(char *identifier);
void checkFunctionDoubleDeclaration(struct functionSymbolTableEntry *function);
void checkCall(char *functionIdentifier, struct exprList *list);

// utility
char *getTypeString(enum type t);
enum type decideType(char *t);
char *toString(int val);
void printSymbolList(struct symbolList *list);
void printFunctionList(struct functionSymbolList *list);

void init()
{
    programSymbols = allocateSymbolList();
    programFunctions = allocateFunctionSymbolList();

    output = (char **)malloc(1000 * 1000);

    for (int i = 0; i < 1000; i++)
        output[i] = (char *)malloc(1000);
}

struct symbolList *allocateSymbolList()
{
    struct symbolList *list = (struct symbolList *)malloc(sizeof(struct symbolList));

    list->symbolTable = (struct symbolTableEntry **)malloc(1000 * sizeof(struct symbolTableEntry *));

    for (int i = 0; i < 1000; i++)
        list->symbolTable[i] = (struct symbolTableEntry *)malloc(sizeof(struct symbolTableEntry));

    list->symbolCount = 0;

    return list;
}
struct symbolTableEntry *createSymbolEntry(char *name)
{
    struct symbolTableEntry *newEntry = (struct symbolTableEntry *)malloc(sizeof(struct symbolTableEntry));

    newEntry->name = (char *)malloc(100);
    strcpy(newEntry->name, name);

    newEntry->val.sValue = (char *)malloc(100);
    newEntry->isArray = 0;
    newEntry->size = 0;

    return newEntry;
}
struct symbolTableEntry *createArraySymbolEntry(char *name, int size)
{
    struct symbolTableEntry *newEntry = (struct symbolTableEntry *)malloc(sizeof(struct symbolTableEntry));

    newEntry->name = (char *)malloc(100);
    strcpy(newEntry->name, name);

    newEntry->isArray = 1;
    newEntry->size = size;
    newEntry->arrayValues = (struct ExprInfo *)malloc(sizeof(struct ExprInfo *));

    return newEntry;
}
void addEntry(struct symbolList *list, struct symbolTableEntry *entry)
{
    list->symbolTable[list->symbolCount] = entry;
    list->symbolCount++;
}
struct symbolList *createSymbolList(struct symbolTableEntry *entry)
{
    struct symbolList *newList = allocateSymbolList();

    addEntry(newList, entry);

    return newList;
}
void addSymbol(enum type symbolType, struct symbolList *fromSymbols, struct symbolList *toSymbols)
{
    for (int i = 0; i < fromSymbols->symbolCount; i++)
    {
        checkDoubleDeclaration(fromSymbols->symbolTable[i]->name);
        if (!alreadyUsed(fromSymbols->symbolTable[i]->name))
        {
            if (fromSymbols->symbolTable[i]->isArray == 0)
            {
                fromSymbols->symbolTable[i]->val.valueType = symbolType;
            }
            else
            {
                for (int j = 0; j < fromSymbols->symbolTable[i]->size; j++)
                    fromSymbols->symbolTable[i]->arrayValues[j].valueType = symbolType;
            }
            addEntry(toSymbols, fromSymbols->symbolTable[i]);
        }
    }
}

void assignValue(char *identifier, struct ExprInfo *expr_ptr)
{
    if (identifier[strlen(identifier) - 1] != ']')
    {
        checkDeclaration(identifier);
        if (alreadyUsed(identifier))
        {
            for (int i = 0; i < programSymbols->symbolCount; i++)
            {
                if (!strcmp(programSymbols->symbolTable[i]->name, identifier))
                {
                    if (programSymbols->symbolTable[i]->val.valueType == expr_ptr->valueType)
                    {
                        switch (expr_ptr->valueType)
                        {
                        case INT:
                            programSymbols->symbolTable[i]->val.value = expr_ptr->value;
                            break;
                        case STRING:
                            strcpy(programSymbols->symbolTable[i]->val.sValue, expr_ptr->sValue);
                            break;
                        case FLOAT:
                            programSymbols->symbolTable[i]->val.fValue = expr_ptr->fValue;
                            break;
                        case CHAR:
                            programSymbols->symbolTable[i]->val.cValue = expr_ptr->cValue;
                            break;
                        case BOOL:
                            programSymbols->symbolTable[i]->val.bValue = expr_ptr->bValue;
                            break;
                        }
                    }
                    else
                    {
                        printf("Can't assign a %s to a variable of type %s, at line: %d.\n", getTypeString(expr_ptr->valueType), getTypeString(programSymbols->symbolTable[i]->val.valueType), yylineno);
                        compilationStatus = 0;
                    }
                }
            }
        }
    }
    else
    {
        char *actualIdentifier = (char *)malloc(100);
        strncpy(actualIdentifier, identifier, strchr(identifier, '[') - identifier);
        checkDeclaration(actualIdentifier);
        if (alreadyUsed(actualIdentifier))
        {
            char *ind = (char *)malloc(10);
            strcpy(ind, strchr(identifier, '[') + 1);
            ind[strlen(ind) - 1] = '\0';

            int index = atoi(ind);

            for (int i = 0; i < programSymbols->symbolCount; i++)
                if (!strcmp(programSymbols->symbolTable[i]->name, actualIdentifier))
                    if (programSymbols->symbolTable[i]->val.valueType == expr_ptr->valueType)
                    {
                        switch (expr_ptr->valueType)
                        {
                        case INT:
                            programSymbols->symbolTable[i]->arrayValues[index].value = expr_ptr->value;
                            break;
                        case STRING:
                            strcpy(programSymbols->symbolTable[i]->arrayValues[index].sValue, expr_ptr->sValue);
                            break;
                        case FLOAT:
                            programSymbols->symbolTable[i]->arrayValues[index].fValue = expr_ptr->fValue;
                            break;
                        case CHAR:
                            programSymbols->symbolTable[i]->arrayValues[index].cValue = expr_ptr->cValue;
                            break;
                        case BOOL:
                            programSymbols->symbolTable[i]->arrayValues[index].bValue = expr_ptr->bValue;
                            break;
                        }
                    }
                    else
                    {
                        printf("Can't assign a %s to a variable of type %s at line: %d.\n", getTypeString(expr_ptr->valueType), getTypeString(programSymbols->symbolTable[i]->val.valueType), yylineno);
                        compilationStatus = 0;
                    }
        }
    }
}
int getValue(char *identifier)
{
    checkDeclaration(identifier);
    if (alreadyUsed(identifier))
    {
        for (int i = 0; i < programSymbols->symbolCount; i++)
            if (!strcmp(programSymbols->symbolTable[i]->name, identifier))
                return programSymbols->symbolTable[i]->val.value;
    }
    return -1;
}
enum type getType(char *identifier)
{
    for (int i = 0; i < programSymbols->symbolCount; i++)
        if (!strcmp(programSymbols->symbolTable[i]->name, identifier))
            return programSymbols->symbolTable[i]->val.valueType;
}
struct ExprInfo *getExpr(char *identifier)
{
    if (identifier[strlen(identifier) - 1] != ']')
    {
        for (int i = 0; i < programSymbols->symbolCount; i++)
            if (!strcmp(programSymbols->symbolTable[i]->name, identifier))
                return &programSymbols->symbolTable[i]->val;
    }
    else
    {
        char *actualIdentifier = (char *)malloc(100);
        strncpy(actualIdentifier, identifier, strchr(identifier, '[') - identifier);
        char *ind = (char *)malloc(10);
        strcpy(ind, strchr(identifier, '[') + 1);
        ind[strlen(ind) - 1] = '\0';

        int index = atoi(ind);

        for (int i = 0; i < programSymbols->symbolCount; i++)
            if (!strcmp(programSymbols->symbolTable[i]->name, actualIdentifier))
                return &programSymbols->symbolTable[i]->arrayValues[index];
    }
    return NULL;
}
void addName(struct symbolTableEntry *entry, char *name)
{
    strcpy(entry->name, name);
}
void addType(struct symbolTableEntry *entry, char *valType)
{
    entry->val.valueType = decideType(valType);
}

struct functionSymbolList *allocateFunctionSymbolList()
{
    struct functionSymbolList *list = (struct functionSymbolList *)malloc(sizeof(struct functionSymbolList));

    list->functionSymbolTable = (struct functionSymbolTableEntry **)malloc(1000 * sizeof(struct functionSymbolTableEntry *));

    for (int i = 0; i < 1000; i++)
        list->functionSymbolTable[i] = (struct functionSymbolTableEntry *)malloc(sizeof(struct symbolTableEntry));

    for (int i = 0; i < 1000; i++)
    {
        list->functionSymbolTable[i]->parameters = allocateSymbolList();
    }

    list->functionsSymbolCount = 0;

    return list;
}
struct functionSymbolTableEntry *createFunctionSymbolEntry(char *name, enum type returnType, struct symbolList *parameters)
{
    struct functionSymbolTableEntry *newEntry = (struct functionSymbolTableEntry *)malloc(sizeof(struct functionSymbolTableEntry));

    newEntry->name = (char *)malloc(100);
    strcpy(newEntry->name, name);

    newEntry->returnType = returnType;

    newEntry->parameters = allocateSymbolList();
    newEntry->parameters = parameters;

    return newEntry;
}
struct functionSymbolList *createFunctionSymbolList(struct functionSymbolTableEntry *entry)
{
    struct functionSymbolList *newList = allocateFunctionSymbolList();

    addFunctionEntry(newList, entry);

    return newList;
}
void addFunctionEntry(struct functionSymbolList *list, struct functionSymbolTableEntry *entry)
{
    checkFunctionDoubleDeclaration(entry);
    if (!alreadyUsedFunction(entry))
    {
        list->functionSymbolTable[list->functionsSymbolCount] = entry;
        list->functionsSymbolCount++;
    }
}

void addExpr(struct exprList *list, struct ExprInfo *expr)
{
    list->exprArray[list->exprCount] = expr;
    list->exprCount++;
}
struct exprList *createExprList(struct ExprInfo *expr)
{
    struct exprList *list = (struct exprList *)malloc(sizeof(struct exprList));

    list->exprArray = (struct ExprInfo **)malloc(sizeof(struct ExprInfo *));

    for (int i = 0; i < 1000; i++)
        list->exprArray[i] = (struct ExprInfo *)malloc(sizeof(struct ExprInfo));

    addExpr(list, expr);

    return list;
}
struct ExprInfo *createIntExpr(int val)
{
    struct ExprInfo *expr = (struct ExprInfo *)malloc(sizeof(struct ExprInfo));

    expr->valueType = INT;
    expr->value = val;

    return expr;
}
struct ExprInfo *createStrExpr(char *val)
{
    struct ExprInfo *expr = (struct ExprInfo *)malloc(sizeof(struct ExprInfo));

    expr->sValue = (char *)malloc(100);

    expr->valueType = STRING;

    val = val + 1;
    val[strlen(val) - 1] = '\0';

    strcpy(expr->sValue, val);

    return expr;
}
struct ExprInfo *createFloatExpr(float val)
{
    struct ExprInfo *expr = (struct ExprInfo *)malloc(sizeof(struct ExprInfo));

    expr->valueType = FLOAT;
    expr->fValue = val;

    return expr;
}
struct ExprInfo *createCharExpr(char val)
{
    struct ExprInfo *expr = (struct ExprInfo *)malloc(sizeof(struct ExprInfo));

    expr->valueType = CHAR;
    expr->cValue = val;

    return expr;
}
struct ExprInfo *createBoolExpr(int val)
{
    struct ExprInfo *expr = (struct ExprInfo *)malloc(sizeof(struct ExprInfo));

    expr->valueType = BOOL;
    expr->bValue = val;

    return expr;
}
struct ExprInfo *createExpr(char *identifier)
{
    struct ExprInfo *expr = (struct ExprInfo *)malloc(sizeof(struct ExprInfo));

    char *actualIdentifier = (char *)malloc(100);

    if (identifier[strlen(identifier) - 1] == ']')
    {
        strncpy(actualIdentifier, identifier, strchr(identifier, '[') - identifier);
    }
    else
        strcpy(actualIdentifier, identifier);

    if (alreadyUsed(actualIdentifier))
    {
        expr = getExpr(identifier);
    }
    else
    {
        for (int i = 0; i < programFunctions->functionsSymbolCount; i++)
            if (!strcmp(programFunctions->functionSymbolTable[i]->name, identifier))
            {
                expr->valueType = programFunctions->functionSymbolTable[i]->returnType;
                switch (expr->valueType)
                {
                case INT:
                    expr->value = 0;
                    break;
                case STRING:
                    expr->sValue = (char *)malloc(100);
                    strcpy(expr->sValue, "");
                    break;
                case FLOAT:
                    expr->fValue = 0.0f;
                    break;
                case CHAR:
                    expr->cValue = ' ';
                    break;
                case BOOL:
                    expr->bValue = 0;
                    break;
                }
            }
    }

    return expr;
}
struct ExprInfo *createCompExpr(struct ExprInfo *f, char *op, struct ExprInfo *s)
{
    struct ExprInfo *expr = (struct ExprInfo *)malloc(sizeof(struct ExprInfo));
    if(f == NULL)
    {
        if(s->valueType == INT)
        {
            expr->valueType = INT;
            expr->value = -s->value;
        }
        if(s->valueType == FLOAT)
        {
            expr->valueType = FLOAT;
            expr->fValue = -s->fValue;
        }
    }
    else
    if (f->valueType == s->valueType)
    {
        expr->valueType = f->valueType;
        if (f->valueType == INT)
        {
            int ok = 0;
            if (!strcmp(op, "+"))
            {
                expr->value = f->value + s->value;
                ok = 1;
            }
            if (!strcmp(op, "-"))
            {
                expr->value = f->value - s->value;
                ok = 1;
            }
            if (!strcmp(op, "*"))
            {
                expr->value = f->value * s->value;
                ok = 1;
            }
            if (!strcmp(op, "/"))
            {
                if (s->value != 0)
                    expr->value = f->value / s->value;
                else
                {
                    compilationStatus = 0;
                    printf("Division by 0 is not allowed at line %d\n", yylineno);
                }
                ok = 1;
            }
            if (!strcmp(op, "=="))
            {
                expr->valueType = BOOL;
                expr->bValue = (f->value == s->value ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, ">"))
            {
                expr->valueType = BOOL;
                expr->bValue = (f->value > s->value ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, "<"))
            {
                expr->valueType = BOOL;
                expr->bValue = (f->value < s->value ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, ">="))
            {
                expr->valueType = BOOL;
                expr->bValue = (f->value >= s->value ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, "<="))
            {
                expr->valueType = BOOL;
                expr->bValue = (f->value <= s->value ? 1 : 0);
                ok = 1;
            }
            if (ok == 0)
            {
                compilationStatus = 0;
                printf("Operator %s can't be used on operators of type int at line: %d\n", op, yylineno);
                return NULL;
            }
        }
        if (f->valueType == FLOAT)
        {
            int ok = 0;
            if (!strcmp(op, "+"))
            {
                expr->fValue = f->fValue + s->fValue;
                ok = 1;
            }
            if (!strcmp(op, "-"))
            {
                expr->fValue = f->fValue - s->fValue;
                ok = 1;
            }
            if (!strcmp(op, "*"))
            {
                expr->fValue = f->fValue * s->fValue;
                ok = 1;
            }
            if (!strcmp(op, "/"))
            {
                if (s->fValue != 0.0f)
                    expr->fValue = f->fValue / s->fValue;
                else
                {
                    compilationStatus = 0;
                    printf("Division by 0 is not allowed at line: %d\n", yylineno);
                }
                ok = 1;
            }
            if (!strcmp(op, "=="))
            {
                expr->valueType = BOOL;
                expr->bValue = (f->fValue == s->fValue ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, ">"))
            {
                expr->valueType = BOOL;
                expr->bValue = (f->fValue > s->fValue ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, "<"))
            {
                expr->valueType = BOOL;
                expr->bValue = (f->fValue < s->fValue ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, ">="))
            {
                expr->valueType = BOOL;
                expr->bValue = (f->fValue >= s->fValue ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, "<="))
            {
                expr->valueType = BOOL;
                expr->bValue = (f->fValue <= s->fValue ? 1 : 0);
                ok = 1;
            }
            if (ok == 0)
            {
                compilationStatus = 0;
                printf("Operator %s can't be used on operators of type int at line: %d\n", op, yylineno);
                return NULL;
            }
        }

        if (f->valueType == STRING)
        {
            expr->sValue = (char *)malloc(100);
            int ok = 0;
            if (!strcmp(op, "+"))
            {
                strcpy(expr->sValue, f->sValue);
                strcat(expr->sValue, s->sValue);
                ok = 1;
            }
            if (!strcmp(op, "=="))
            {
                expr->valueType = BOOL;
                if (!strcmp(f->sValue, s->sValue))
                    expr->bValue = 1;
                else
                    expr->bValue = 0;
                ok = 1;
            }
            if (ok == 0)
            {
                compilationStatus = 0;
                printf("Operator %s can't be used on operators of type STRING at line: %d.\n", op, yylineno);
                return NULL;
            }
        }

        if (f->valueType == BOOL)
        {
            int ok = 0;
            if (!strcmp(op, "=="))
            {
                expr->bValue = (f->bValue == s->bValue ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, ">"))
            {
                expr->bValue = (f->bValue > s->bValue ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, "<"))
            {
                expr->bValue = (f->bValue < s->bValue ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, ">="))
            {
                expr->bValue = (f->bValue >= s->bValue ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, "<="))
            {
                expr->bValue = (f->bValue <= s->bValue ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, "and"))
            {
                expr->bValue = (((f->bValue + s->bValue) == 2) ? 1 : 0);
                ok = 1;
            }
            if (!strcmp(op, "or"))
            {
                expr->bValue = (((f->bValue + s->bValue) == 1) ? 1 : 0);
                ok = 1;
            }
            if (ok == 0)
            {
                compilationStatus = 0;
                printf("Operator %s can't be used on operators of type bool at line: %d.\n", op, yylineno);
            }
        }

        if (f->valueType == CHAR)
        {
            int ok = 0;
            if (!strcmp(op, "=="))
            {
                expr->valueType = BOOL;
                expr->bValue = (f->cValue == s->cValue ? 1 : 0);
                ok = 1;
            }
            if (ok == 0)
            {
                compilationStatus = 0;
                printf("Operator %s can't be used on operators of type bool at line: %d.\n", op, yylineno);
                return NULL;
            }
        }
    }
    else
    {
        compilationStatus = 0;
        printf("Operands do not have the same type at line: %d.\n", yylineno);
    }

    return expr;
}
void TypeOf(struct ExprInfo *expr)
{
    if (expr != NULL)
    {
        strcpy(output[outputLinesCount], "The type of expression is: ");
        strcat(output[outputLinesCount], getTypeString(expr->valueType));
        outputLinesCount++;
    }
}
void Eval(struct ExprInfo *expr)
{
    if (expr != NULL)
    {
        strcpy(output[outputLinesCount], "The value of the expression is: ");
        switch (expr->valueType)
        {
        case INT:
            strcat(output[outputLinesCount], toString(expr->value));
            break;
        case STRING:
            strcat(output[outputLinesCount], expr->sValue);
            break;
        case FLOAT:
            char *Nr = (char *)malloc(100);
            gcvt(expr->fValue, 8, Nr);
            strcat(output[outputLinesCount], Nr);
            break;
        case CHAR:
            strcat(output[outputLinesCount], &expr->cValue);
            break;
        case BOOL:
            strcat(output[outputLinesCount], toString(expr->bValue));
            break;
        }
        outputLinesCount++;
    }
}

void printCompilationStatus()
{
    if (compilationStatus == 1)
    {
        for (int i = 0; i < outputLinesCount; i++)
            printf("%s\n", output[i]);
        printf("Compiled succesfully.\n");
    }
    else
        printf("Compilation failed. Please fix the issues and try again.\n");
}
int alreadyUsed(char *identifier)
{
    for (int i = 0; i < programSymbols->symbolCount; i++)
        if (!strcmp(programSymbols->symbolTable[i]->name, identifier))
            return 1;
    return 0;
}
int alreadyUsedFunctionIdentifier(char *identifier)
{
    for (int i = 0; i < programFunctions->functionsSymbolCount; i++)
        if (!strcmp(programFunctions->functionSymbolTable[i]->name, identifier))
            return 1;
    return 0;
}
int alreadyUsedFunction(struct functionSymbolTableEntry *function)
{
    if (alreadyUsedFunctionIdentifier(function->name))
    {
        struct functionSymbolTableEntry *duplicate = (struct functionSymbolTableEntry *)malloc(sizeof(struct functionSymbolTableEntry));
        for (int i = 0; i < programFunctions->functionsSymbolCount; i++)
        {
            if (!strcmp(programFunctions->functionSymbolTable[i]->name, function->name))
                duplicate = programFunctions->functionSymbolTable[i];
        }

        if (function->parameters->symbolCount == duplicate->parameters->symbolCount)
        {
            int ok = 0;
            for (int i = 0; i < function->parameters->symbolCount; i++)
                if (function->parameters->symbolTable[i]->val.valueType != duplicate->parameters->symbolTable[i]->val.valueType)
                    ok = 1;
            if (ok == 0)
            {
                return 1;
            }
        }
    }
    return 0;
}
void checkDeclaration(char *identifier)
{
    int exists = alreadyUsed(identifier);

    if (exists == 0)
    {
        compilationStatus = 0;
        printf("%s was not declared at line: %d.\n", identifier, yylineno);
    }
}
void checkDoubleDeclaration(char *identifier)
{
    int exists = alreadyUsed(identifier);

    if (exists == 1)
    {
        compilationStatus = 0;
        printf("%s is declared twice at line: %d.\n", identifier, yylineno);
    }
}
void checkFunctionDoubleDeclaration(struct functionSymbolTableEntry *function)
{
    int exists = alreadyUsedFunction(function);

    if (exists == 1)
    {
        compilationStatus = 0;
        printf("Same function defined twice at line: %d.\n", yylineno);
    }
}
void checkCall(char *functionIdentifier, struct exprList *list)
{
    int existsCall = 0;
    struct functionSymbolTableEntry *function = (struct functionSymbolTableEntry *)malloc(sizeof(struct functionSymbolTableEntry));
    for (int i = 0; i < programFunctions->functionsSymbolCount; i++)
    {
        if (!strcmp(programFunctions->functionSymbolTable[i]->name, functionIdentifier))
        {
            function = programFunctions->functionSymbolTable[i];
            int ok = 1;

            for (int i = 0; i < function->parameters->symbolCount; i++)
            {
                if (function->parameters->symbolTable[i]->val.valueType != list->exprArray[i]->valueType)
                {
                    ok = 0;
                }
            }
            if (ok == 1)
                existsCall = 1;
        }
    }
    if (existsCall == 0)
    {
        compilationStatus = 0;
        printf("The call parameters do not match the function definition at line: %d.\n", yylineno);
    }
}

enum type decideType(char *t)
{
    if (!strcmp("int", t))
        return INT;
    if (!strcmp("bool", t))
        return BOOL;
    if (!strcmp("char", t))
        return CHAR;
    if (!strcmp("string", t))
        return STRING;
    if (!strcmp("float", t))
        return FLOAT;
}
char *getTypeString(enum type t)
{
    char *stringType = (char *)malloc(10);
    switch (t)
    {
    case 0:
        strcpy(stringType, "int");
        return stringType;
    case 1:
        strcpy(stringType, "bool");
        return stringType;
    case 2:
        strcpy(stringType, "float");
        return stringType;
    case 3:
        strcpy(stringType, "char");
        return stringType;
    case 4:
        strcpy(stringType, "string");
        return stringType;
    }
}
char *toString(int val)
{
    int len = 0, aux = val;
    char* str = (char*)malloc(10);
    if (val == 0)
    {
        str[0] = '0';
        len = 1;
    }
    else
    {
        while (aux)
        {
            len++;
            aux /= 10;
        }
        aux = val;
        int k = 1;
        while (aux)
        {
            str[len - k] = '0' + aux % 10;
            aux /= 10;
            k++;
        }
    }
    str[len + 1] = '\0';
    return str;
}
void printSymbolList(struct symbolList *list)
{
    FILE *f;
    f = fopen("symbol_table.txt", "w");

    fprintf(f, "Simbolurile sunt : \n");
    for (int i = 0; i < list->symbolCount; i++)
    {
        if (list->symbolTable[i]->isArray == 0)
        {
            fprintf(f, "Numele identificatorului este : %s, tipul este : %s\n",
                    list->symbolTable[i]->name,
                    getTypeString(list->symbolTable[i]->val.valueType));

            switch (list->symbolTable[i]->val.valueType)
            {
            case INT:
                fprintf(f, "Valoarea este: %d\n", list->symbolTable[i]->val.value);
                break;
            case STRING:
                fprintf(f, "Valoarea este: %s\n", list->symbolTable[i]->val.sValue);
                break;
            case FLOAT:
                fprintf(f, "Valoarea este: %f\n", list->symbolTable[i]->val.fValue);
                break;
            case CHAR:
                fprintf(f, "Valoarea este: %c\n", list->symbolTable[i]->val.cValue);
                break;
            case BOOL:
                fprintf(f, "Valoarea este: %d\n", list->symbolTable[i]->val.bValue);
                break;
            }
        }
        else
        {
            fprintf(f, "Numele identificatorului este : %s, tipul este : %s, acesta este un array cu dimensiunea de : %d\n",
                    list->symbolTable[i]->name,
                    getTypeString(list->symbolTable[i]->val.valueType),
                    list->symbolTable[i]->size);
        }
    }

    fclose(f);
}
void printFunctionList(struct functionSymbolList *list)
{
    FILE *f;
    f = fopen("symbol_table_functions.txt", "w");

    fprintf(f, "Functiile sunt : \n");

    for (int i = 0; i < list->functionsSymbolCount; i++)
    {
        fprintf(f, "Numele functiei este : %s, iar return-typeul este : %s\n",
                list->functionSymbolTable[i]->name,
                getTypeString(list->functionSymbolTable[i]->returnType));

        fprintf(f, "Parametrii sunt : \n");
        for (int j = 0; j < list->functionSymbolTable[i]->parameters->symbolCount; j++)
        {
            if (list->functionSymbolTable[i]->parameters->symbolTable[j]->isArray == 0)
            {
                fprintf(f, "Parametrul are numele: %s si tipul: %s \n",
                        list->functionSymbolTable[i]->parameters->symbolTable[j]->name,
                        getTypeString(list->functionSymbolTable[i]->parameters->symbolTable[j]->val.valueType));
            }
            else
            {
                fprintf(f, "Parametrul are numele: %s si tipul: %s, iar acesta este un array cu size-ul: %d\n",
                        list->functionSymbolTable[i]->parameters->symbolTable[j]->name,
                        getTypeString(list->functionSymbolTable[i]->parameters->symbolTable[j]->val.valueType),
                        list->functionSymbolTable[i]->parameters->symbolTable[j]->size);
            }
        }
    }

    fclose(f);
}

#endif