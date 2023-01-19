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

/// gettok - Return the next token from standard input.
static int gettok(FILE* fp) {
  static int LastChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = fgetc(fp);

  if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    IdentifierStr = LastChar;
    while (isalnum((LastChar = fgetc(fp))))
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
      LastChar = fgetc(fp);
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), nullptr);
    return TOK_NUMBER;
  }

  if (LastChar == '#') {
    // Comment until end of line.
    do
      LastChar = fgetc(fp);
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF)
      return gettok(fp);
  }
  
  if (LastChar == ':') {
    if((LastChar=fgetc(fp)) == '='){
        LastChar = fgetc(fp);
        return  TOK_COLOEQ;
    }
    return ':';
  }

  if (LastChar == '=') {
    if((LastChar=fgetc(fp)) == '='){
        LastChar = fgetc(fp);
        return  TOK_EQ;
    }
    return '=';
  }

  if (LastChar == '!') {
    if((LastChar=fgetc(fp)) == '='){
        LastChar = fgetc(fp);
        return  TOK_EQ;
    }
    return '!';
  }

  if (LastChar == '<') {
    if((LastChar=fgetc(fp)) == '='){
        LastChar = fgetc(fp);
        return  TOK_LE;
    }
    return TOK_LT;
  }

  if (LastChar == '>') {
    if((LastChar=fgetc(fp)) == '='){
        LastChar = fgetc(fp);
        return  TOK_GE;
    }
    return TOK_GT;
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return TOK_EOF;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = fgetc(fp);
  return ThisChar;
}

static int CurTok;
static int getNextToken(FILE* fp) { return CurTok = gettok(fp); }

int  main(int argc,char** argv)
{
    FILE* fp = fopen(argv[1],"r");

    if( !fp )
    {
        fprintf(stderr,"Can't open \'%s\'\n",argv[1]);
        return  0;
    }

    while((CurTok=getNextToken(fp))!=TOK_EOF)
    {
        if( CurTok >= 0 )
        {
            fprintf(stderr,"Unexpected token : %c , %d\n",(char)CurTok,CurTok);
        }
    }

    fprintf(stderr,"Well tokenized!!\n");
    fclose(fp);

    return  0;
}