#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

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

/// gettok - Return the next token from standard input.
static int gettok(FILE* in) {
  static int LastChar = ' ';
  
  LineNumber += ((LastChar=='\n')?1:0);

  // Skip any whitespace.
  while (isspace(LastChar))
  {
    LastChar = fgetc(in);
    LineNumber += ((LastChar=='\n')?1:0);
  }
  
  if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    IdentifierStr = LastChar;
    while (isalnum((LastChar = fgetc(in))))
      IdentifierStr += LastChar;
    
    if (IdentifierStr == "const")
      return TOK_CONST;
    if (IdentifierStr == "var")
      return TOK_VAR;
    if (IdentifierStr == "function")
      return TOK_FUNCTION;
    if (IdentifierStr == "begin")
      return TOK_BEGIN;
    if (IdentifierStr == "end")
      return TOK_END;
    if (IdentifierStr == "if")
      return TOK_IF;
    if (IdentifierStr == "then")
      return TOK_THEN;
    if (IdentifierStr == "while")
      return TOK_WHILE;
    if (IdentifierStr == "do")
      return TOK_DO;
    if (IdentifierStr == "return")
      return TOK_RETURN;
    if (IdentifierStr == "write")
      return TOK_WRITE;
    if (IdentifierStr == "writeln")
      return TOK_WRITELN;
    if (IdentifierStr == "odd")
      return TOK_ODD;
    
    return TOK_IDENT;
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = fgetc(in);
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
      LastChar = fgetc(in);
      LineNumber += ((LastChar=='\n')?1:0);
    } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF)
      return gettok(in);
  }
  
  if (LastChar == ':') {
    if((LastChar=fgetc(in)) == '='){
        LastChar = fgetc(in);
        return  TOK_COLOEQ;
    }
    return ':';
  }

  if (LastChar == '=') {
    if((LastChar=fgetc(in)) == '='){
        LastChar = fgetc(in);
        return  TOK_EQ;
    }
    return '=';
  }

  if (LastChar == '!') {
    if((LastChar=fgetc(in)) == '='){
        LastChar = fgetc(in);
        return  TOK_EQ;
    }
    return '!';
  }

  if (LastChar == '<') {
    if((LastChar=fgetc(in)) == '='){
        LastChar = fgetc(in);
        return  TOK_LE;
    }
    return TOK_LT;
  }

  if (LastChar == '>') {
    if((LastChar=fgetc(in)) == '='){
        LastChar = fgetc(in);
        return  TOK_GE;
    }
    return TOK_GT;
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return TOK_EOF;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = fgetc(in);
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
};

/// ProgramAST
/// program ::= block '.'
class ProgramAST : public AST {
  std::unique_ptr<AST>  block;

public:
  ProgramAST(std::unique_ptr<AST> block) : block(std::move(block)) {}
};

/// BlockAST
/// block ::= declList statement
class BlockAST : public AST {
  std::unique_ptr<AST>  declList,statement;

public:
  BlockAST(std::unique_ptr<AST> declList,std::unique_ptr<AST> statement)
    : declList(std::move(declList)), statement(std::move(statement)) {}
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
};

/// ConstDeclAST
/// constDecl ::= CONST numberList ';'
class ConstDeclAST : public AST {
  std::unique_ptr<AST> numberList;

public:
  ConstDeclAST(std::unique_ptr<AST> numberList)
    : numberList(std::move(numberList)) {}
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
};

/// VarDeclAST
/// varDecl ::= VAR identList ';'
class VarDeclAST : public AST {
  std::unique_ptr<AST> identList;

public:
  VarDeclAST(std::unique_ptr<AST> identList)
    : identList(std::move(identList)) {}
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
};

/// StatementAST
/// statement
///    ::= <empty>
///    ::= IDENT COLOEQ expression
///    ::= BEGINN statement stateList END
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
};

/// TermAST
/// term ::= factor factList
class TermAST : public AST {
  std::unique_ptr<AST> factor,factList;

public:
  TermAST(std::unique_ptr<AST> factor,std::unique_ptr<AST> factList)
    : factor(std::move(factor)),factList(std::move(factList)) {}
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
};

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
  fprintf(stderr, "%d, Error: %s\n", LineNumber, Str);
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
    return nullptr;
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
    return nullptr;
  }

  name = IdentifierStr;
  getNextToken(fp);
  
  if( CurTok != '=' )
  {
    return nullptr;
  }

  getNextToken(fp);

  if( CurTok != TOK_NUMBER )
  {
    return nullptr;
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
    return nullptr;
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
    return nullptr;
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
    return nullptr;
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
    return LogError("Expected ID");
  }

  name = IdentifierStr;
  getNextToken(fp);

  if( CurTok != '(' )
  {
    return LogError("Expected (");;
  }

  getNextToken(fp);

  auto O = ParseOptParList();

  if( CurTok != ')' )
  {
    return LogError("Expected )");
  }

  getNextToken(fp);
  auto B = ParseBlock();
  
  if( !B )
  {
    return nullptr;
  }

  if( CurTok != ';' )
  {
    return LogError("Expected ;");
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
      return nullptr;
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
      return nullptr;
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

  if( op_tok!=TOK_EQ && op_tok!=TOK_NOTEQ && op_tok!=TOK_LT
        && op_tok!=TOK_GT && op_tok!=TOK_LE && op_tok!=TOK_GT )
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
    return nullptr;
  }

  getNextToken(fp);
  auto S = ParseStatement();

  return std::make_unique<StateListAST>(ParseStateList(),std::move(S));
}

/// statement
///    ::= <empty>
///    ::= IDENT COLOEQ expression
///    ::= BEGINN statement stateList END
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
        if(CurTok!=TOK_COLOEQ) return nullptr;
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
        if(CurTok!=TOK_END) return nullptr;
        getNextToken(fp);
        return std::make_unique<StatementAST>(TOK_BEGIN,"",nullptr,nullptr,std::move(S),std::move(SL));
      }
      break;
    case  TOK_IF:
      {
        getNextToken(fp);
        auto C = ParseCondition();
        if(!C) return nullptr;
        if(CurTok!=TOK_THEN) return nullptr;
        getNextToken(fp);
        auto S = ParseStatement();
        return std::make_unique<StatementAST>(TOK_IF,"",nullptr,std::move(C),std::move(S),nullptr);
      }
      break;
    case  TOK_WHILE:
      {
        getNextToken(fp);
        auto C = ParseCondition();
        if(!C) return nullptr;
        if(CurTok!=TOK_DO) return nullptr;
        getNextToken(fp);
        auto S = ParseStatement();
        return std::make_unique<StatementAST>(TOK_WHILE,"",nullptr,std::move(C),std::move(S),nullptr);
      }
      break;
    case  TOK_RETURN:
      {
        getNextToken(fp);
        auto E = ParseExpression();
        if(!E) return nullptr;
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
    return  LogError("Cannot find block!!");
  }
  
  if( CurTok != '.' )
  {
    return LogError("Expected '.' at end of program");
  }
  
  getNextToken(fp);

  return  std::make_unique<ProgramAST>(std::move(B));
}

int  main(int argc,char** argv)
{
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
    }

    fclose(fp);

    return  0;
}