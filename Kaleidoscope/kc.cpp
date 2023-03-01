#include    "llvm/ADT/APFloat.h"
#include    "llvm/ADT/STLExtras.h"
#include    "llvm/IR/BasicBlock.h"
#include    "llvm/IR/Constants.h"
#include    "llvm/IR/DerivedTypes.h"
#include    "llvm/IR/Function.h"
#include    "llvm/IR/IRBuilder.h"
#include    "llvm/IR/LLVMContext.h"
#include    "llvm/IR/Module.h"
#include    "llvm/IR/Type.h"
#include    "llvm/IR/Verifier.h"
#include    <algorithm>
#include    <cctype>
#include    <cstdio>
#include    <cstdlib>
#include    <map>
#include    <memory>
#include    <string>
#include    <vector>

using namespace llvm;

///=================================================================
/// Defines
///=================================================================
#define is_included(t,w)    ((t).find((w))!=(t).end())

///=================================================================
/// Lexer
///=================================================================
#include    "lexer.h"

static std::string  Identifier;
static double       NumVal;
static FILE*        in;
static int          LineNumber = 1;

static std::map<std::string,int> keyword;

static int  read_char(void)
{
    int c = fgetc(in);

    LineNumber += (c=='\n')?1:0;

    return  c;
}

static int  gettok(void)
{
    static int  LastChar = ' ';

    for(;isspace(LastChar);LastChar=read_char());

    if( isalpha(LastChar) )
    {
        Identifier = LastChar;
        for(;(isalnum(LastChar=read_char()));Identifier+=LastChar);
        
        return  (is_included(keyword,Identifier))?
                        keyword[Identifier]:TOK_IDENTIFIER;
    }

    if( isdigit(LastChar) || LastChar == '.' )
    {
        std::string NumStr;

        do{
            NumStr += LastChar;
            LastChar = read_char();
        }while(isdigit(LastChar) || LastChar == '.');

        NumVal = strtod(NumStr.c_str(),nullptr);
        return  TOK_NUMBER;
    }

    if( LastChar == '#' )
    {
        do{
            LastChar = read_char();
        }while(LastChar!=EOF&&LastChar!='\n'&&LastChar!='\r');

        if( LastChar != EOF )
        {
            return  gettok();
        }
    }

    if( LastChar == EOF )
    {
        return  TOK_EOF;
    }

    int ret = LastChar;

    LastChar = read_char();
    return  ret;
}

///=================================================================
/// Parser
///=================================================================
#include    "parser.h"

static int  CurTok;
static int  getNextToken(void) {return CurTok = gettok();}

static std::map<char,int>   BinOpPrecedence;

static int  GetTokPrecedence(void)
{
    int ret = -1;

    if( isascii(CurTok) && 
            is_included(BinOpPrecedence,CurTok) )
    {
        ret = BinOpPrecedence[CurTok];
    }
    
    return  ret;
}

std::unique_ptr<ExprAST>    LogError(const char* Str)
{
    fprintf(stderr,"Error: %s\n",Str);
    return  nullptr;
}

std::unique_ptr<PrototypeAST>   LogErrorP(const char* Str)
{
    LogError(Str);
    return  nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr()
{
    auto    Result = std::make_unique<NumberExprAST>(NumVal);
    getNextToken();
    return  std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr()
{
    getNextToken();
    auto    V = ParseExpression();
    
    if(!V) return nullptr;
    if(CurTok!=')') return LogError("expected ')'");

    getNextToken();
    return  V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr()
{
    std::string IdName = Identifier;

    getNextToken();
    if( CurTok != '(' )
    {
        return  std::make_unique<VariableExprAST>(IdName);
    }

    getNextToken();
    std::vector<std::unique_ptr<ExprAST>>   Args;

    if( CurTok != ')' )
    {
        for(;;getNextToken())
        {
            if( auto Arg = ParseExpression() )
            {
                Args.push_back(std::move(Arg));
            }
            else
            {
                return  nullptr;
            }

            if(CurTok==')') break;
            if(CurTok!=',') return LogError("Expected ')' or ',' in argument list");
        }
    }

    getNextToken();
    return  std::make_unique<CallExprAST>(IdName,std::move(Args));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary()
{
    switch(CurTok)
    {
        case    TOK_IDENTIFIER:
            return  ParseIdentifierExpr();
        case    TOK_NUMBER:
            return  ParseNumberExpr();
        case    '(':
            return  ParseParenExpr();
        default:
            break;
    }

    return  LogError("unknown token when expecting an expression");
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(
            int ExprPrec,std::unique_ptr<ExprAST> LHS)
{
    for(;;)
    {
        int TokPrec = GetTokPrecedence();

        if( TokPrec < ExprPrec )
        {
            return  LHS;
        }

        int BinOp = CurTok;
        getNextToken();

        auto    RHS = ParsePrimary();
        if( !RHS )
        {
            return  nullptr;
        }

        int NextPrec = GetTokPrecedence();
        if( TokPrec < NextPrec )
        {
            RHS = ParseBinOpRHS(TokPrec+1,std::move(RHS));
            if(!RHS) return nullptr;
        }

        LHS = std::make_unique<BinaryExprAST>(BinOp,
                            std::move(LHS),std::move(RHS));
    }

    return  nullptr;
}

/// expression
///   ::= primary binoprhs
static std::unique_ptr<ExprAST> ParseExpression()
{
    auto    LSH = ParsePrimary();
    if(!LSH) return nullptr;
    return  ParseBinOpRHS(0,std::move(LSH));
}

/// prototype
///   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST>    ParsePrototype()
{
    if( CurTok != TOK_IDENTIFIER )
    {
        return  LogErrorP("Expected function name in prototype");
    }

    std::string FuncName = Identifier;
    getNextToken();

    if( CurTok != '(' )
    {
        return  LogErrorP("Expected '(' in prototype");
    }

    std::vector<std::string>    ArgNames;

    for(;getNextToken()==TOK_IDENTIFIER;ArgNames.push_back(Identifier));
    if(CurTok!=')') return LogErrorP("Expected ')' in prototype");

    getNextToken();
    return  std::make_unique<PrototypeAST>(FuncName,std::move(ArgNames));
}

/// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition()
{
    getNextToken();
    auto    Proto = ParsePrototype();

    if(!Proto) return nullptr;
    if( auto E = ParseExpression() )
    {
        return  std::make_unique<FunctionAST>(std::move(Proto),std::move(E));
    }

    return  nullptr;
}

static std::unique_ptr<FunctionAST> ParseTopLevelExpr()
{
    if( auto E = ParseExpression() )
    {
        auto    Proto = std::make_unique<PrototypeAST>
                            ("__anon_expr",std::vector<std::string>());
        return  std::make_unique<FunctionAST>(std::move(Proto),std::move(E));
    }

    return  nullptr;
}

/// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST>    ParseExtern()
{
    getNextToken();
    return  ParsePrototype();
}

///=================================================================
/// Parser
///=================================================================
static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module>      TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string,Value*> NamedValues;

Value*  LogErrorV(const char* str)
{
    LogError(str);
    return  nullptr;
}

Value*  NumberExprAST::codegen()
{
    return  ConstantFP::get(*TheContext,APFloat(Val));
}

Value*  VariableExprAST::codegen()
{
    Value*  V = NamedValues[Name];

    if(!V) return LogErrorV("Unknown variable name");
    return  V;
}

Value*  BinaryExprAST::codegen()
{
    Value*  L = LHS->codegen();
    Value*  R = LHS->codegen();

    if(!L||!R) return nullptr;

    switch(Op)
    {
        case    '+':
            return  Builder->CreateFAdd(L,R,"addtmp");
        case    '-':
            return  Builder->CreateFSub(L,R,"subtmp");
        case    '*':
            return  Builder->CreateFMul(L,R,"multmp");
        case    '<':
            return  Builder->CreateUIToFP(L,
                        Type::getDoubleTy(*TheContext),"booltmp");
        default:
            break;
    }

    return  LogErrorV("invalid binary operator");
}

Value*  CallExprAST::codegen()
{
    Function*   CalleeF = TheModule->getFunction(Callee);

    if( !CalleeF )
    {
        return  LogErrorV("Unknown function referenced");
    }

    if( CalleeF->arg_size() != Args.size() )
    {
        return  LogErrorV("Incorrect # arguments passed");
    }

    std::vector<Value*> ArgsV;

    for(int i=0;i<Args.size();i++)
    {
        ArgsV.push_back(Args[i]->codegen());
        if(!ArgsV.back()) return nullptr;
    }

    return  Builder->CreateCall(CalleeF,ArgsV,"calltmp");
}

Function*   PrototypeAST::codegen()
{
    std::vector<Type*>  Doubles(Args.size(),
                                Type::getDoubleTy(*TheContext));
    FunctionType*       FT = FunctionType::get(
                                Type::getDoubleTy(*TheContext),Doubles,false);
    Function*           F = Function::Create(FT, 
                                Function::ExternalLinkage,Name,TheModule.get());
    
    int i = 0;
    for(auto& Arg:F->args()) Arg.setName(Args[i++]);

    return  F;
}

Function*   FunctionAST::codegen()
{
    Function*   TheFunction = TheModule->getFunction(Proto->getName());

    if(!TheFunction) TheFunction = Proto->codegen();
    if(!TheFunction) return nullptr;

    BasicBlock* BB = BasicBlock::Create(*TheContext,"entry",TheFunction);
    Builder->SetInsertPoint(BB);

    NamedValues.clear();
    for(auto& Arg:TheFunction->args())
        NamedValues[std::string(Arg.getName())] = &Arg;
    
    if( Value* RetVal = Body->codegen() )
    {
        Builder->CreateRet(RetVal);
        verifyFunction(*TheFunction);
        return  TheFunction;
    }

    TheFunction->eraseFromParent();
    return  nullptr;
}

///=================================================================
/// Top_Level parsing and JIT Driver
///=================================================================
static void InitializeModule()
{
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("my cool jit",*TheContext);
    Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

static void HandleDefinition()
{
    if( auto FuncAST = ParseDefinition() )
    {
        if( auto* FuncIR = FuncAST->codegen() )
        {
            fprintf(stderr,"Read function definition:");
            FuncIR->print(errs());
            fprintf(stderr,"\n");
        }
    }
    else
    {
        getNextToken();
    }
}

static void HandleExtern()
{
    if( auto ProtoAST = ParseExtern() )
    {
        if( auto* FuncIR = ProtoAST->codegen() )
        {
            fprintf(stderr,"Read extern: ");
            FuncIR->print(errs());
            fprintf(stderr,"\n");
        }
    }
    else
    {
        getNextToken();
    }
}

static void HandleTopLevelExpression(void)
{
    if( auto FuncAST = ParseTopLevelExpr() )
    {
        if( auto* FuncIR = FuncAST->codegen() )
        {
            fprintf(stderr,"Read top-level expression:");
            FuncIR->print(errs());
            fprintf(stderr,"\n");

            FuncIR->eraseFromParent();
        }
    }
    else
    {
        getNextToken();
    }
}

/// top ::= definition | external | expression | ';'
static void MainLoop(void)
{
    for(;;)
    {
        fprintf(stderr,"ready> ");

        switch(CurTok)
        {
            case    TOK_EOF:return;
            case    ';':
                getNextToken();break;
            case    TOK_DEF:
                HandleDefinition();break;
            case    TOK_EXTERN:
                HandleExtern();break;
            default:
                HandleTopLevelExpression();break;
        }
    }
}

///=================================================================
/// Main Driver Code
///=================================================================
int main(int argc,char** argv)
{
    in = fopen(argv[1],"r");

    if( !in )
    {
        fprintf(stderr,"Can't open %s\n",argv[1]);
        return  0;
    }

    keyword["def"] = TOK_DEF;
    keyword["extern"] = TOK_EXTERN;

    BinOpPrecedence['<'] = 10;
    BinOpPrecedence['+'] = 20;
    BinOpPrecedence['-'] = 20;
    BinOpPrecedence['*'] = 40;

    fprintf(stderr,"ready> ");
    getNextToken();

    InitializeModule();
    MainLoop();
    TheModule->print(errs(),nullptr);

    return  0;
}