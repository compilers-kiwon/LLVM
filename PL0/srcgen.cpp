#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

//===----------------------------------------------------------------------===//
// Private Defines
//===----------------------------------------------------------------------===//
#define IN_RANGE(MIN,n,MAX)   ((MIN)<=(n)&&(n)<=(MAX))

//===----------------------------------------------------------------------===//
// Private Variables
//===----------------------------------------------------------------------===//
static std::map<std::string,int> keyword_tok;
static std::map<int,std::string> cond_operator;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
  TOK_EOF = -1,

  TOK_COLOEQ = -2,
  TOK_EQ = -3,
  TOK_NOTEQ = -4,
  TOK_LT = -5,
  TOK_GT = -6,
  TOK_LE = -7,
  TOK_GE = -8,

  TOK_CONST = -9,
  TOK_VAR = -10,
  TOK_FUNCTION = -11,
  TOK_BEGIN = -12,
  TOK_END = -13,
  TOK_IF = -14,
  TOK_THEN = -15,
  TOK_WHILE = -16,

  TOK_DO = -17,
  TOK_RETURN = -18,
  TOK_WRITE = -19,
  TOK_WRITELN = -20,
  TOK_ODD = -21,
  
  TOK_IDENT = -22,
  TOK_NUMBER = -23
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number
static int LineNumber = 1;

static int read_char(FILE* in) {
  int ret = fgetc(in);

  if( ret == '\n' )
  {
    LineNumber++;
  }

  return  ret;
}

/// gettok - Return the next token from standard input.
static int gettok(FILE* in) {
  static int LastChar = ' ';
  
  // Skip any whitespace.
  while (isspace(LastChar))
  {
    LastChar = read_char(in);
  }
  
  if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    IdentifierStr = LastChar;

    while (isalnum((LastChar = read_char(in))))
      IdentifierStr += LastChar;
    
    return (keyword_tok.find(IdentifierStr)!=keyword_tok.end())?
            keyword_tok[IdentifierStr]:TOK_IDENT;
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = read_char(in);
    } while (isdigit(LastChar) || LastChar == '.');

    if( NumStr == "." )
    {
      return '.';
    }

    NumVal = strtod(NumStr.c_str(), nullptr);
    return TOK_NUMBER;
  }

  if (LastChar == '#') {
    // Comment until end of line.
    do {
      LastChar = read_char(in);
    } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF)
      return gettok(in);
  }
  
  if (LastChar == ':') {
    if((LastChar=read_char(in)) == '='){
        LastChar = read_char(in);
        return  TOK_COLOEQ;
    }
    return ':';
  }

  if (LastChar == '=') {
    if((LastChar=read_char(in)) == '='){
        LastChar = read_char(in);
        return  TOK_EQ;
    }
    return '=';
  }

  if (LastChar == '!') {
    if((LastChar=read_char(in)) == '='){
        LastChar = read_char(in);
        return  TOK_EQ;
    }
    return '!';
  }

  if (LastChar == '<') {
    if((LastChar=read_char(in)) == '='){
        LastChar = read_char(in);
        return  TOK_LE;
    }
    return TOK_LT;
  }

  if (LastChar == '>') {
    if((LastChar=read_char(in)) == '='){
        LastChar = read_char(in);
        return  TOK_GE;
    }
    return TOK_GT;
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return TOK_EOF;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = read_char(in);
  return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

/// AST - Base class for all expression nodes.
class AST {
public:
  virtual ~AST() = default;
  virtual void srcgen(int indent) = 0;
  virtual Value* codegen(void) = 0;
  virtual Value* codegen(stc::vector<Value*>& v) = 0;
};

/// ProgramAST
/// program ::= block '.'
class ProgramAST : public AST {
  std::unique_ptr<AST>  block;

public:
  ProgramAST(std::unique_ptr<AST> block) : block(std::move(block)) {}
  void srcgen(int indent) override;
  Value* codegen(void) override;
};

/// BlockAST
/// block ::= declList statement
class BlockAST : public AST {
  std::unique_ptr<AST>  declList,statement;

public:
  BlockAST(std::unique_ptr<AST> declList,std::unique_ptr<AST> statement)
    : declList(std::move(declList)), statement(std::move(statement)) {}
  void srcgen(int indent) override;
  Value* codegen(void) override;
};

/// DeclListAST
/// declList 
///    ::= <empty>
///    ::= declList decl
class DeclListAST : public AST {
  std::unique_ptr<AST> declList,decl;

public:
  DeclListAST(std::unique_ptr<AST> declList,std::unique_ptr<AST> decl)
    : declList(std::move(declList)), decl(std::move(decl)) {}
  void srcgen(int indent) override;
  Value* codegen(stc::vector<Value*>& v) override;
};

/// DeclAST
/// decl
///    ::= constDecl
///    ::= varDecl
///    ::= funcDecl
class DeclAST : public AST {
  std::unique_ptr<AST>  decl;

public:
  DeclAST(std::unique_ptr<AST> decl)
    : decl(std::move(decl)) {}
  void srcgen(int indent) override;
  Value* codegen(void) override;
};

/// ConstDeclAST
/// constDecl ::= CONST numberList ';'
class ConstDeclAST : public AST {
  std::unique_ptr<AST> numberList;

public:
  ConstDeclAST(std::unique_ptr<AST> numberList)
    : numberList(std::move(numberList)) {}
  void srcgen(int indent) override;
  Value* codegen(void) override;
};

/// NumberListAST
/// numberList
///    ::= IDENT EQ NUMBER
///    ::= numberList COMMA IDENT EQ NUMBER
class NumberListAST : public AST {
  std::string Name;
  float Val;
  std::unique_ptr<AST> numberList;

public:
  NumberListAST(std::string Name,float Val,std::unique_ptr<AST> numberList)
    : Name(Name),Val(Val),numberList(std::move(numberList)) {}
  void srcgen(int indent) override;
  Value* codegen(stc::vector<Value*>& v) override;
};

/// VarDeclAST
/// varDecl ::= VAR identList ';'
class VarDeclAST : public AST {
  std::unique_ptr<AST> identList;

public:
  VarDeclAST(std::unique_ptr<AST> identList)
    : identList(std::move(identList)) {}
  void srcgen(int indent) override;
  Value* codegen(void) override;
};

/// IdentListAST
/// identList
///    ::= IDENT
///    ::= identList COMMA IDENT
class IdentListAST : public AST {
  std::string Name;
  std::unique_ptr<AST> identList;

public:
  IdentListAST(std::string Name,std::unique_ptr<AST> identList)
    : Name(Name),identList(std::move(identList)) {}
  void srcgen(int indent) override;
  Value* codegen(stc::vector<Value*>& v) override;
};

/// OptParListAST
/// optParList
///    ::= <empty>
///    ::= parList
class OptParListAST : public AST {
  std::unique_ptr<AST>  parList;

public:
  OptParListAST(std::unique_ptr<AST> parList)
    : parList(std::move(parList)) {}
  void srcgen(int indent) override;
  Value* codegen(void) override;
};

/// ParListAST
/// parList
///    ::= IDENT
///    ::= parList COMMA IDENT
class ParListAST : public AST {
  std::string Name;
  std::unique_ptr<AST> parList;

public:
  ParListAST(std::string Name, std::unique_ptr<AST> parList)
    : Name(Name),parList(std::move(parList)) {}
  void srcgen(int indent) override;
  Value* codegen(stc::vector<Value*>& v) override;
};

/// FuncDeclAST
/// funcDecl ::= FUNCTION IDENT '('  optParList ')' block ';'
class FuncDeclAST : public AST {
  std::string Name;
  std::unique_ptr<AST>  optParList,block;

public:
  FuncDeclAST(std::string Name,std::unique_ptr<AST> optParList,
    std::unique_ptr<AST> block) : Name(Name),
    optParList(std::move(optParList)), block(std::move(block)) {}
  void srcgen(int indent) override;
  Value* codegen(void) override;
};

/// StatementAST
/// statement
///    ::= <empty>
///    ::= IDENT COLOEQ expression 
///    ::= BEGIN statement stateList END
///    ::= IF condition THEN statement 
///    ::= WHILE condition DO statement 
///    ::= RETURN expression 
///    ::= WRITE expression
///    ::= WRITELN
class StatementAST : public AST {
  int head_tok;
  std::string Name;
  std::unique_ptr<AST> expression;
  std::unique_ptr<AST> condition;
  std::unique_ptr<AST> statement;
  std::unique_ptr<AST> stateList;

public:
  StatementAST(int head_tok,std::string Name,std::unique_ptr<AST> expression,
    std::unique_ptr<AST> condition,std::unique_ptr<AST> statement,
    std::unique_ptr<AST> stateList) : head_tok(head_tok),Name(Name),
    expression(std::move(expression)),condition(std::move(condition)),
    statement(std::move(statement)),stateList(std::move(stateList)) {}
  void srcgen(int indent) override;
  Value* codegen(void) override;
};

/// StateListAST
/// stateList
///    ::= <empty>
///    ::= stateList ';' statement
class StateListAST : public AST {
  std::unique_ptr<AST> stateList,statement;

public:
  StateListAST(std::unique_ptr<AST> stateList,
    std::unique_ptr<AST> statement) : stateList(std::move(stateList)),
    statement(std::move(statement)) {}
  void srcgen(int indent) override;
  Value* codegen(stc::vector<Value*>& v) override;
};

/// ConditionAST
/// condition
///    ::= ODD expression
///    ::= expression EQ expression
///    ::= expression NOTEQ expression
///    ::= expression LT expression
///    ::= expression GT expression
///    ::= expression LE expression
///    ::= expression GE expression
class ConditionAST : public AST {
  int op_tok;
  std::unique_ptr<AST> LHS,RHS;

public:
  ConditionAST(int op_tok,std::unique_ptr<AST> LHS,
    std::unique_ptr<AST> RHS) : op_tok(op_tok),
    LHS(std::move(LHS)),RHS(std::move(RHS)) {}
  void srcgen(int indent) override;
  Value* codegen(void) override;
};

/// ExpressionAST
/// expression
///    ::= '-'  term termList
///    ::= term  termList
class ExpressionAST : public AST {
  int head_tok;
  std::unique_ptr<AST> term,termList;

public:
  ExpressionAST(int head_tok,std::unique_ptr<AST> term,
    std::unique_ptr<AST> termList) : head_tok(head_tok),
    term(std::move(term)),termList(std::move(termList)) {}
  void srcgen(int indent) override;
  Value* codegen(void) override;
};

/// TermListAST
/// termList
///    ::= <empty>
///    ::= termList '+' term
///    ::= termList '-' term
class TermListAST : public AST {
  int op_tok;
  std::unique_ptr<AST> term,termList;

public:
  TermListAST(int op_tok,std::unique_ptr<AST> term,
    std::unique_ptr<AST> termList) : op_tok(op_tok),
    term(std::move(term)),termList(std::move(termList)) {}
  void srcgen(int indent) override;
  Value* codegen(stc::vector<Value*>& v) override;
};

/// TermAST
/// term ::= factor factList
class TermAST : public AST {
  std::unique_ptr<AST> factor,factList;

public:
  TermAST(std::unique_ptr<AST> factor,std::unique_ptr<AST> factList)
    : factor(std::move(factor)),factList(std::move(factList)) {}
  void srcgen(int indent) override;
  Value* codegen(void) override;
};

/// FactListAST
/// factList
///    ::= <empty>
///    ::= factList '*' factor
///    ::= factList '/' factor
class FactListAST : public AST {
  int op_tok;
  std::unique_ptr<AST> factor,factList;

public:
  FactListAST(int op_tok,std::unique_ptr<AST> factor,
    std::unique_ptr<AST> factList) : op_tok(op_tok),
    factor(std::move(factor)),factList(std::move(factList)) {}
  void srcgen(int indent) override;
  Value* codegen(stc::vector<Value*>& v) override;
};

/// FactorAST
/// factor
///    ::= IDENT
///    ::= NUMBER
///    ::= IDENT '(' expList ')'
///    ::= '(' expression ')'
class FactorAST : public AST {
  std::string Name;
  float Val;
  std::unique_ptr<AST> expList,expression;

public:
  FactorAST(std::string Name,float Val,std::unique_ptr<AST> expList,
    std::unique_ptr<AST> expression) : Name(Name),Val(Val),
    expList(std::move(expList)),expression(std::move(expression)) {}
  void srcgen(int indent) override;
  Value* codegen(void) override;
};

/// ExpListAST
/// expList
///    ::= expression
///    ::= expList ',' expression
class ExpListAST : public AST {
  std::unique_ptr<AST> expList,expression;

public:
  ExpListAST(std::unique_ptr<AST> expList,std::unique_ptr<AST> expression) :
    expList(std::move(expList)),expression(std::move(expression)) {}
  void srcgen(int indent) override;
  Value* codegen(stc::vector<Value*>& v) override;
};

}  

//===----------------------------------------------------------------------===//
// Source Code Generator from Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//
static void print_indent(int indent)
{
  for(int i=0;i<indent;i++)
  {
    for(int j=0;j<4;j++)
    {
      fputc(' ',stderr);
    }
  }
}

void ProgramAST::srcgen(int indent)
{
  if(block) block->srcgen(indent);
  fprintf(stderr,".\n");
}

void BlockAST::srcgen(int indent)
{
  if(declList) declList->srcgen(indent);
  if(statement) statement->srcgen(indent);
}

void DeclAST::srcgen(int indent)
{
  if(decl) decl->srcgen(indent);
}

void DeclListAST::srcgen(int indent)
{
  if(decl) decl->srcgen(indent);
  if(declList) declList->srcgen(indent);
}

void ConstDeclAST::srcgen(int indent)
{
  print_indent(indent);
  fprintf(stderr,"const ");

  if(numberList) numberList->srcgen(indent);
  fprintf(stderr,";\n");
}

void ExpListAST::srcgen(int indent)
{
  if(expression) expression->srcgen(indent);
  if(expList) {
    fputc(',',stderr);
    expList->srcgen(indent);
  }
}

void FactorAST::srcgen(int indent)
{
  if(Name.empty()) {
    if(expression) {
      fputc('(',stderr);
      expression->srcgen(indent);
      fputc(')',stderr);
    }
    else {
      fprintf(stderr,"%f",Val);
    }
  }
  else {
    fprintf(stderr,"%s",Name.c_str());
    if(expList) {
      fputc('(',stderr);
      expList->srcgen(indent);
      fputc(')',stderr);
    }
  }
}

void FactListAST::srcgen(int indent)
{
  if(op_tok) fputc(op_tok,stderr);
  if(factor) factor->srcgen(indent);
  if(factList) factList->srcgen(indent);
}

void TermListAST::srcgen(int indent)
{
  if(op_tok) fputc(op_tok,stderr);
  if(term) term->srcgen(indent);
  if(termList) termList->srcgen(indent);
}

void TermAST::srcgen(int indent)
{
  if(factor) factor->srcgen(indent);
  if(factList) factList->srcgen(indent);
}

void ExpressionAST::srcgen(int indent)
{
  if(head_tok) fputc(head_tok,stderr);
  if(term) term->srcgen(indent);
  if(termList) termList->srcgen(indent);
}

void ConditionAST::srcgen(int indent)
{
  if( op_tok == TOK_ODD )
  {
    fprintf(stderr,"odd ");
    if(LHS) LHS->srcgen(indent);
  }
  else
  {
    if(LHS) LHS->srcgen(indent);
    fprintf(stderr," %s ",cond_operator[op_tok].c_str());
    if(RHS) RHS->srcgen(indent);
  }
}

void StateListAST::srcgen(int indent)
{
  if(statement) {
    statement->srcgen(indent);
    fprintf(stderr,";\n");
  }
  if(stateList) {
    stateList->srcgen(indent);
  }
}

void StatementAST::srcgen(int indent)
{
  if(head_tok) print_indent(indent);

  switch(head_tok)
  {
    case TOK_IDENT:
      fprintf(stderr,"%s := ",Name.c_str());
      if(expression) expression->srcgen(indent);
      break;
    case TOK_BEGIN:
      fprintf(stderr,"begin\n");
      if(statement) statement->srcgen(indent+1);
      if(stateList) {
        fprintf(stderr,";\n");
        stateList->srcgen(indent+1);
      }
      print_indent(indent);
      fprintf(stderr,"end");
      break;
    case TOK_IF:
      fprintf(stderr,"if ");
      if(condition) condition->srcgen(indent);
      fprintf(stderr," then\n");
      if(statement) statement->srcgen(indent+1);
      break;
    case TOK_WHILE:
      fprintf(stderr,"while ");
      if(condition) condition->srcgen(indent);
      fprintf(stderr," do\n");
      if(statement) statement->srcgen(indent);
      break;
    case TOK_RETURN:
      fprintf(stderr,"return ");
      if(expression) expression->srcgen(indent);
      break;
    case TOK_WRITE:
      fprintf(stderr,"write ");
      if(expression) expression->srcgen(indent);
      break;
    case TOK_WRITELN:
      fprintf(stderr,"writeln");
      break;
    default:
      // do nothing
      break;
  }
}

void FuncDeclAST::srcgen(int indent)
{
  fprintf(stderr,"function %s (",Name.c_str());

  if(optParList) optParList->srcgen(indent);
  fprintf(stderr,")\n");

  if(block) block->srcgen(indent+1);
  fprintf(stderr,";\n");
}

void ParListAST::srcgen(int indent)
{
  fprintf(stderr,"%s",Name.c_str());

  if(parList) {
    fputc(',',stderr);
    parList->srcgen(indent);
  }
}

void OptParListAST::srcgen(int indent)
{
  if(parList) parList->srcgen(indent);
}

void IdentListAST::srcgen(int indent)
{
  fprintf(stderr,"%s",Name.c_str());

  if(identList) {
    fputc(',',stderr);
    identList->srcgen(indent);
  }
}

void VarDeclAST::srcgen(int indent)
{
  print_indent(indent);
  fprintf(stderr,"var ");

  if(identList) identList->srcgen(indent);
  fprintf(stderr,";\n");
}

void NumberListAST::srcgen(int indent)
{
  fprintf(stderr,"%s=%f",Name.c_str(),Val);

  if(numberList) {
    fputc(',',stderr);
    numberList->srcgen(indent);
  }
}
//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static FILE* fp;
static int CurTok;
static int getNextToken(FILE* in){ return CurTok = gettok(in); }

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<AST> LogError(const char *Str) {
  static bool found = false;

  if(!found) fprintf(stderr, "%d, Error: %s\n", LineNumber, Str),found=true;
  return nullptr;
}
std::unique_ptr<AST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

static std::unique_ptr<AST> ParseBlock(void);
static std::unique_ptr<AST> ParseExpression(void);
static std::unique_ptr<AST> ParseFactor(void);
static std::unique_ptr<AST> ParseStatement(void);

/// parList
///    ::= IDENT
///    ::= parList COMMA IDENT
static std::unique_ptr<AST> ParseParList(void)
{
  std::string name;

  if( CurTok != TOK_IDENT )
  {
    return LogError("Expected Identifier");
  }

  name = IdentifierStr;
  getNextToken(fp);

  if( CurTok == ',' )
  {
    getNextToken(fp);

    auto P = ParseParList();

    if( !P )
    {
      return nullptr;
    }

    return std::make_unique<ParListAST>(name,std::move(P));
  }

  return std::make_unique<ParListAST>(name,nullptr);
}

/// optParList
///    ::= <empty>
///    ::= parList
static std::unique_ptr<AST> ParseOptParList(void)
{
  auto P = ParseParList();

  if( !P )
  {
    return nullptr;
  }

  return std::make_unique<OptParListAST>(std::move(P));
}

/// numberList
///    ::= IDENT EQ NUMBER
///    ::= numberList COMMA IDENT EQ NUMBER
static std::unique_ptr<AST> ParseNumberList(void)
{
  std::string name;
  float val;

  if( CurTok != TOK_IDENT )
  {
    LogError("Expected Identifier");
    return nullptr;
  }

  name = IdentifierStr;
  getNextToken(fp);
  
  if( CurTok != '=' )
  {
    return LogError("Expected '='");
  }

  getNextToken(fp);

  if( CurTok != TOK_NUMBER )
  {
    return LogError("Expected Number");;
  }

  val = NumVal;
  getNextToken(fp);

  if( CurTok == ',' )
  {
    getNextToken(fp);

    auto N = ParseNumberList();

    if( !N )
    {
      return nullptr;
    }
    
    return std::make_unique<NumberListAST>(name,val,std::move(N));
  }

  return std::make_unique<NumberListAST>(name,val,nullptr);
}

/// identList
///    ::= IDENT
///    ::= identList COMMA IDENT
static std::unique_ptr<AST> ParseIdentList(void)
{
  std::string name;

  if( CurTok != TOK_IDENT )
  {
    return LogError("Expected Identifier");
  }

  name = IdentifierStr;
  getNextToken(fp);

  if( CurTok == ',' )
  {
    getNextToken(fp);

    auto I = ParseIdentList();

    if( !I )
    {
      return nullptr;
    }

    return std::make_unique<IdentListAST>(name,std::move(I));
  }

  return std::make_unique<IdentListAST>(name,nullptr);
}

/// constDecl ::= CONST numberList ';'
static std::unique_ptr<AST> ParseConstDecl(void)
{
  if( CurTok != TOK_CONST )
  {
    return nullptr;
  }
  
  getNextToken(fp);

  auto NL = ParseNumberList();

  if( !NL )
  {
    return nullptr;
  }

  if( CurTok != ';' )
  {
    return LogError("Expected ';'");
  }

  getNextToken(fp);

  return std::make_unique<ConstDeclAST>(std::move(NL));
}

/// varDecl ::= VAR identList ';'
static std::unique_ptr<AST> ParseVarDecl(void)
{
  if( CurTok != TOK_VAR )
  {
    return nullptr;
  }

  getNextToken(fp);

  auto IL = ParseIdentList();

  if( !IL )
  {
    return nullptr;
  }

  if( CurTok != ';' )
  {
    return LogError("Expected ';'");
  }

  getNextToken(fp);

  return std::make_unique<VarDeclAST>(std::move(IL));
}

/// funcDecl ::= FUNCTION IDENT '('  optParList ')' block ';'
static std::unique_ptr<AST> ParseFuncDecl(void)
{
  std::string name;

  if( CurTok != TOK_FUNCTION )
  {
    return nullptr;
  }
  
  getNextToken(fp);
  
  if( CurTok != TOK_IDENT )
  {
    return LogError("Expected Identifier");
  }

  name = IdentifierStr;
  getNextToken(fp);

  if( CurTok != '(' )
  {
    return LogError("Expected '('");;
  }

  getNextToken(fp);

  auto O = ParseOptParList();

  if( CurTok != ')' )
  {
    return LogError("Expected ')'");
  }

  getNextToken(fp);
  auto B = ParseBlock();
  
  if( !B )
  {
    return nullptr;
  }

  if( CurTok != ';' )
  {
    return LogError("Expected ';'");
  }

  getNextToken(fp);

  return std::make_unique<FuncDeclAST>(name,std::move(O),std::move(B));
}

/// decl
///    ::= constDecl
///    ::= varDecl
///    ::= funcDecl
static std::unique_ptr<AST> ParseDec(void)
{
  auto D = ParseConstDecl();

  if( !D )
  {
    D = ParseVarDecl();
  }

  if( !D )
  {
    D = ParseFuncDecl();
  }

  if( !D )
  {
    return nullptr;
  }

  return std::make_unique<DeclAST>(std::move(D));
}

/// declList 
///    ::= <empty>
///    ::= declList dec
static std::unique_ptr<AST> ParseDeclList(void)
{
  auto D = ParseDec();

  if( !D )
  {
    return nullptr;
  }

  return std::make_unique<DeclListAST>(ParseDeclList(),std::move(D));
}

/// expList
///    ::= expression
///    ::= expList ',' expression
static std::unique_ptr<AST> ParseExpList(void)
{
  auto E = ParseExpression();

  if( !E )
  {
    return nullptr;
  }

  if( CurTok != ',' )
  {
    return std::make_unique<ExpListAST>(nullptr,std::move(E));
  }

  getNextToken(fp);
  return std::make_unique<ExpListAST>(ParseExpList(),std::move(E));
}
/// factList
///    ::= <empty>
///    ::= factList '*' factor
///    ::= factList '/' factor
static std::unique_ptr<AST> ParseFactList(void)
{
  int op_tok = CurTok;

  if( op_tok!='*' && op_tok!='/' )
  {
    return nullptr;
  }

  getNextToken(fp);
  auto F = ParseFactor();

  if( !F )
  {
    return nullptr;
  }

  return std::make_unique<FactListAST>(op_tok,std::move(F),ParseFactList());
}

/// factor
///    ::= IDENT
///    ::= NUMBER
///    ::= IDENT '(' expList ')'
///    ::= '(' expression ')'
static std::unique_ptr<AST> ParseFactor(void)
{
  if( CurTok == TOK_IDENT )
  {
    std::string name = IdentifierStr;

    getNextToken(fp);

    if( CurTok != '(' )
    {
      return std::make_unique<FactorAST>(name,0.0,nullptr,nullptr);
    }

    getNextToken(fp);
    auto EL = ParseExpList();

    if( !EL || CurTok!=')' )
    {
      if( !EL )
      {
        return nullptr;
      }

      return LogError("Expected ')'");
    }

    getNextToken(fp);
    return std::make_unique<FactorAST>(name,0.0f,std::move(EL),nullptr);
  }

  if( CurTok == TOK_NUMBER )
  {
    getNextToken(fp);
    return std::make_unique<FactorAST>("",NumVal,nullptr,nullptr);
  }

  if( CurTok == '(' )
  {
    getNextToken(fp);
    auto E = ParseExpression();

    if( !E || CurTok!=')' )
    {
      if( !E )
      {
        return nullptr;
      }

      return LogError("Expected ')'");
    }

    getNextToken(fp);
    return std::make_unique<FactorAST>("",0.0,nullptr,std::move(E));
  }

  return nullptr;
}

/// term ::= factor factList
static std::unique_ptr<AST> ParseTerm(void)
{
  auto F = ParseFactor();

  if( !F )
  {
    return nullptr;
  }

  return std::make_unique<TermAST>(std::move(F),ParseFactList());
}

/// termList
///    ::= <empty>
///    ::= termList '+' term
///    ::= termList '-' term
static std::unique_ptr<AST> ParseTermList(void)
{
  int op_tok = CurTok;

  if( op_tok!='+' && op_tok!='-' )
  {
    return nullptr;
  }

  getNextToken(fp);
  auto T = ParseTerm();

  if( !T )
  {
    return nullptr;
  }

  return std::make_unique<TermListAST>(op_tok,std::move(T),ParseTermList());
}

/// expression
///     ::= '-'  term termList
///     ::= term  termList
static std::unique_ptr<AST> ParseExpression(void)
{
  if( CurTok == '-' )
  {
    getNextToken(fp);
    auto T = ParseTerm();
    
    if( !T )
    {
      return nullptr;
    }

    auto TL = ParseTermList();

    return std::make_unique<ExpressionAST>('-',std::move(T),std::move(TL));
  }

  auto T = ParseTerm();

  if( !T )
  {
    return nullptr;
  }

  auto TL = ParseTermList();

  return std::make_unique<ExpressionAST>(0,std::move(T),std::move(TL));
}

/// condition
///    ::= ODD expression
///    ::= expression EQ expression
///    ::= expression NOTEQ expression
///    ::= expression LT expression
///    ::= expression GT expression
///    ::= expression LE expression
///    ::= expression GE expression
static std::unique_ptr<AST> ParseCondition(void)
{
  if( CurTok == TOK_ODD )
  {
    getNextToken(fp);
    auto LHS = ParseExpression();

    if( !LHS )
    {
      return nullptr;
    }

    return std::make_unique<ConditionAST>(TOK_ODD,std::move(LHS),nullptr);
  }
  
  auto LHS = ParseExpression();

  if( !LHS )
  {
    return nullptr;
  }

  int op_tok = CurTok;

  if( !IN_RANGE(TOK_GE,op_tok,TOK_EQ) )
  {
    return LogError("Expected conditional operator");
  }

  getNextToken(fp);
  auto RHS = ParseExpression();

  if( !RHS )
  {
    return  nullptr;
  }
  
  return std::make_unique<ConditionAST>(op_tok,std::move(LHS),std::move(RHS));
}

/// stateList
///    ::= <empty>
///    ::= stateList ';' statement
static std::unique_ptr<AST> ParseStateList(void)
{
  if( CurTok != ';' )
  {
    return LogError("Expected ';'");
  }

  getNextToken(fp);
  auto S = ParseStatement();

  return std::make_unique<StateListAST>(ParseStateList(),std::move(S));
}

/// statement
///    ::= <empty>
///    ::= IDENT COLOEQ expression
///    ::= BEGIN statement stateList END
///    ::= IF condition THEN statement
///    ::= WHILE condition DO statement
///    ::= RETURN expression
///    ::= WRITE expression
///    ::= WRITELN
static std::unique_ptr<AST> ParseStatement(void)
{
  std::string name;
  
  switch (CurTok)
  {
    case  TOK_IDENT:
      {
        name = IdentifierStr;
        getNextToken(fp);
        if(CurTok!=TOK_COLOEQ) return LogError("Expected ':='");;
        getNextToken(fp);
        auto E = ParseExpression();
        if(!E) return nullptr;
        return std::make_unique<StatementAST>(TOK_IDENT,name,std::move(E),nullptr,nullptr,nullptr);
      }
      break;
    case  TOK_BEGIN:
      {
        getNextToken(fp);
        auto S = ParseStatement();
        auto SL = ParseStateList();
        if(CurTok!=TOK_END) return LogError("Expected 'end'");
        getNextToken(fp);
        return std::make_unique<StatementAST>(TOK_BEGIN,"",nullptr,nullptr,std::move(S),std::move(SL));
      }
      break;
    case  TOK_IF:
      {
        getNextToken(fp);
        auto C = ParseCondition();
        if(!C) return nullptr;
        if(CurTok!=TOK_THEN) return LogError("Expected 'then'");
        getNextToken(fp);
        auto S = ParseStatement();
        return std::make_unique<StatementAST>(TOK_IF,"",nullptr,std::move(C),std::move(S),nullptr);
      }
      break;
    case  TOK_WHILE:
      {
        getNextToken(fp);
        auto C = ParseCondition();
        if(!C) return LogError("Expected a condition");
        if(CurTok!=TOK_DO) return LogError("Expected 'do'");
        getNextToken(fp);
        auto S = ParseStatement();
        return std::make_unique<StatementAST>(TOK_WHILE,"",nullptr,std::move(C),std::move(S),nullptr);
      }
      break;
    case  TOK_RETURN:
      {
        getNextToken(fp);
        auto E = ParseExpression();
        if(!E) return LogError("Expected an expression");
        return std::make_unique<StatementAST>(TOK_RETURN,"",std::move(E),nullptr,nullptr,nullptr);
      }
      break;
    case  TOK_WRITE:
      {
        getNextToken(fp);
        auto E = ParseExpression();
        if(!E) return nullptr;
        return std::make_unique<StatementAST>(TOK_WRITE,"",std::move(E),nullptr,nullptr,nullptr);
      }
      break;
    case  TOK_WRITELN:
      {
        getNextToken(fp);
        return std::make_unique<StatementAST>(TOK_WRITELN,"",nullptr,nullptr,nullptr,nullptr);
      }
      break;
    default:
      break;
  }

  return nullptr;
}

/// block ::= declList statement
static std::unique_ptr<AST> ParseBlock(void)
{
  auto D = ParseDeclList();
  auto S = ParseStatement();

  return  std::make_unique<BlockAST>(std::move(D),std::move(S));
}

/// program ::= block '.'
static std::unique_ptr<AST> ParseProgram(void)
{
  auto B = ParseBlock();

  if( !B )
  {
    return  nullptr;
  }
  
  if( CurTok != '.' )
  {
    return LogError("Expected '.' at end of program");
  }
  
  getNextToken(fp);

  return  std::make_unique<ProgramAST>(std::move(B));
}

//===----------------------------------------------------------------------===//
// LLVM IR Generator
//===----------------------------------------------------------------------===//
static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value *> NamedValues;

Value* NumberListAST::codegen(void){

}

void init_map(void)
{
  keyword_tok["const"] = TOK_CONST;
  keyword_tok["var"] = TOK_VAR;
  keyword_tok["function"] = TOK_FUNCTION;
  keyword_tok["begin"] = TOK_BEGIN;
  keyword_tok["end"] = TOK_END;
  keyword_tok["if"] = TOK_IF;
  keyword_tok["then"] = TOK_THEN;
  keyword_tok["while"] = TOK_WHILE;
  keyword_tok["do"] = TOK_DO;
  keyword_tok["return"] = TOK_RETURN;
  keyword_tok["write"] = TOK_WRITE;
  keyword_tok["writeln"] = TOK_WRITELN;
  keyword_tok["odd"] = TOK_ODD;

  cond_operator[TOK_EQ] = "==";
  cond_operator[TOK_NOTEQ] = "!=";
  cond_operator[TOK_LT] = "<";
  cond_operator[TOK_GT] = ">";
  cond_operator[TOK_LE] = "<=";
  cond_operator[TOK_GE] = ">=";
}

int  main(int argc,char** argv)
{
    init_map();
    fp = fopen(argv[1],"r");

    if( !fp )
    {
        fprintf(stderr,"Can't open \'%s\'\n",argv[1]);
        return  0;
    }

    getNextToken(fp);
    auto P = ParseProgram();

    if( !P )
    {
      LogError("Fail");
    }
    else
    {
      LogError("Success");
      P->srcgen(0);
    }

    fclose(fp);

    return  0;
}