#include    "KaleidoscopeJIT.h"
#include    "llvm/ADT/APFloat.h"
#include    "llvm/ADT/STLExtras.h"
#include    "llvm/IR/BasicBlock.h"
#include    "llvm/IR/Constants.h"
#include    "llvm/IR/DerivedTypes.h"
#include    "llvm/IR/Function.h"
#include    "llvm/IR/IRBuilder.h"
#include    "llvm/IR/LLVMContext.h"
#include    "llvm/IR/LegacyPassManager.h"
#include    "llvm/IR/Module.h"
#include    "llvm/IR/Type.h"
#include    "llvm/IR/Verifier.h"
#include    "llvm/Support/TargetSelect.h"
#include    "llvm/Target/TargetMachine.h"
#include    "llvm/Transforms/InstCombine/InstCombine.h"
#include    "llvm/Transforms/Scalar.h"
#include    "llvm/Transforms/Scalar/GVN.h"
#include    <algorithm>
#include    <cassert>
#include    <cctype>
#include    <cstdint>
#include    <cstdio>
#include    <cstdlib>
#include    <map>
#include    <memory>
#include    <string>
#include    <vector>

using namespace llvm;
using namespace llvm::orc;

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
    int c;

    if( !in )
    {
        c = getchar();
    }
    else
    {
        c = fgetc(in);
        LineNumber += (c=='\n')?1:0;
    }

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

///  ifexpr ::= 'if' expression 'then' expression 'else' expression
static std::unique_ptr<ExprAST> ParseIfExpr()
{
    getNextToken();

    auto Cond = ParseExpression();
    if(!Cond) return nullptr;

    if(CurTok!=TOK_THEN) return LogError("expected then");
    getNextToken();

    auto Then = ParseExpression();
    if(!Then) return nullptr;

    if(CurTok!=TOK_ELSE) return LogError("expected else");
    getNextToken();

    auto Else = ParseExpression();
    if(!Else) return nullptr;

    return  std::make_unique<IfExprAST>(std::move(Cond),
                                std::move(Then),std::move(Else));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
///   ::= ifexpr
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
        case    TOK_IF:
            return  ParseIfExpr();
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
/// Code Generation
///=================================================================
static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module>      TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string,Value*> NamedValues;

static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::map<std::string,std::unique_ptr<PrototypeAST>> FunctionProtos;

static ExitOnError  ExitOnErr;

Value*  LogErrorV(const char* str)
{
    LogError(str);
    return  nullptr;
}

Function*   getFunction(std::string Name)
{
    if( auto F = TheModule->getFunction(Name) )
    {
        return  F;
    }

    auto FI = FunctionProtos.find(Name);

    if( FI != FunctionProtos.end() )
    {
        return  FI->second->codegen();
    }

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

Value*  IfExprAST::codegen()
{
    // will be implemented
    return  nullptr;
}

Value*  BinaryExprAST::codegen()
{
    Value*  L = LHS->codegen();
    Value*  R = RHS->codegen();

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
    Function*   CalleeF = getFunction(Callee);

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
    auto &P = *Proto;
    FunctionProtos[Proto->getName()] = std::move(Proto);

    Function*   TheFunction = getFunction(P.getName());
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
        TheFPM->run(*TheFunction);

        return  TheFunction;
    }

    TheFunction->eraseFromParent();
    return  nullptr;
}

///=================================================================
/// Top_Level parsing and JIT Driver
///=================================================================
static void InitializeModuleAndPassManager()
{
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("Kaleidoscope JIT",*TheContext);
    TheModule->setDataLayout(TheJIT->getDataLayout());

    Builder = std::make_unique<IRBuilder<>>(*TheContext);
    TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());

    TheFPM->add(createInstructionCombiningPass());
    TheFPM->add(createReassociatePass());
    TheFPM->add(createGVNPass());
    TheFPM->add(createCFGSimplificationPass());

    TheFPM->doInitialization();
}

static void HandleDefinition()
{
    if( auto FuncAST = ParseDefinition() )
    {
        if( auto* FuncIR = FuncAST->codegen() )
        {
            fprintf(stderr,"Read function definition:\n");
            FuncIR->print(errs());
            fprintf(stderr,"\n");
            ExitOnErr(TheJIT->addModule(ThreadSafeModule
                    (std::move(TheModule),std::move(TheContext))));
            InitializeModuleAndPassManager();
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
            FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
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
            auto RT = TheJIT->getMainJITDylib().createResourceTracker();
            auto TSM = ThreadSafeModule(std::move(TheModule),std::move(TheContext));

            ExitOnErr(TheJIT->addModule(std::move(TSM),RT));
            InitializeModuleAndPassManager();

            auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));
            double  (*FP)() = (double (*)())(intptr_t)ExprSymbol.getAddress();

            fprintf(stderr,"Evaluated to %f\n",FP());
            ExitOnErr(RT->remove());
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

        fprintf(stderr,"ready> ");
    }
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
  fputc((char)X, stderr);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

///=================================================================
/// Main Driver Code
///=================================================================
int main(int argc,char** argv)
{
    if( argc > 1 )
    {
        in = fopen(argv[1],"r");

        if( !in )
        {
            fprintf(stderr,"Can't open %s\n",argv[1]);
            return  0;
        }
    }

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    keyword["def"] = TOK_DEF;
    keyword["extern"] = TOK_EXTERN;
    keyword["if"] = TOK_IF;
    keyword["then"] = TOK_THEN;
    keyword["else"] = TOK_ELSE;

    BinOpPrecedence['<'] = 10;
    BinOpPrecedence['+'] = 20;
    BinOpPrecedence['-'] = 20;
    BinOpPrecedence['*'] = 40;

    fprintf(stderr,"ready> ");
    getNextToken();

    TheJIT = ExitOnErr(KaleidoscopeJIT::Create());
    InitializeModuleAndPassManager();

    MainLoop();

    return  0;
}