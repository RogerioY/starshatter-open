/*  Starshatter OpenSource Distribution
     Copyright (c) 1997-2004, Destroyer Studios LLC.
     All Rights Reserved.

     Redistribution and use in source and binary forms, with or without
     modification, are permitted provided that the following conditions are met:

     * Redistributions of source code must retain the above copyright notice,
        this list of conditions and the following disclaimer.
     * Redistributions in binary form must reproduce the above copyright notice,
        this list of conditions and the following disclaimer in the documentation
        and/or other materials provided with the distribution.
     * Neither the name "Destroyer Studios" nor the names of its contributors
        may be used to endorse or promote products derived from this software
        without specific prior written permission.

     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
     AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
     IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
     ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
     LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
     CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
     SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
     INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
     CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
     ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
     POSSIBILITY OF SUCH DAMAGE.

     SUBSYSTEM:    Parser
     FILE:         Token.cpp
     AUTHOR:       John DiCamillo


     OVERVIEW
     ========
     Scanner class implementation
*/

#include "MemDebug.h"
#include "Token.h"
#include "Reader.h"
#include "Text.h"

#include <ctype.h>

// +-------------------------------------------------------------------+

bool        Token::hidecom   = true;
char        Token::combeg[3] = "//";
char        Token::comend[3] = "\n";
char        Token::altbeg[3] = "/*";
char        Token::altend[3] = "*/";
Dictionary<int>   Token::keymap;

// +-------------------------------------------------------------------+

Token::Token()
    : mType(Undefined), mKey(0), mLine(0), mColumn(0)
{
    mLength    = 0;
    mSymbol[0] = '\0';
}

Token::Token(const Token& rhs)
    : mType(rhs.mType), mKey(rhs.mKey), mLine(rhs.mLine), mColumn(rhs.mColumn)
{
    mLength = rhs.mLength;
    if (mLength < 8) {
        strcpy_s(mSymbol, rhs.mSymbol);
    }
    else {
        mFullSymbol = new(__FILE__, __LINE__) char[mLength + 1];
        strcpy(mFullSymbol, rhs.mFullSymbol);
    }
}

Token::Token(int t)
    : mType(t), mKey(0), mLine(0), mColumn(0)
{
    mLength    = 0;
    mSymbol[0] = '\0';
}

Token::Token(const char* s, int t, int k, int l, int c)
    : mType(t), mKey(k), mLine(l), mColumn(c)
{
    mLength = strlen(s);
    if (mLength < 8) {
        strcpy_s(mSymbol, s);
    }
    else {
        mFullSymbol = new(__FILE__, __LINE__) char[mLength + 1];
        strcpy(mFullSymbol, s);
    }
}

Token::Token(const Text& s, int t, int k, int l, int c)
    : mType(t), mKey(k), mLine(l), mColumn(c)
{
    mLength = s.length();
    if (mLength < 8) {
        strcpy_s(mSymbol, s.data());
    }
    else {
        mFullSymbol = new(__FILE__, __LINE__) char[mLength + 1];
        strcpy(mFullSymbol, s.data());
    }
}

Token::~Token()
{
    if (mLength >= 8)
        delete [] mFullSymbol;
}

// +-------------------------------------------------------------------+

void
Token::close()
{
    keymap.clear();
}

// +-------------------------------------------------------------------+

Token&
Token::operator = (const Token& rhs)
{
    if (mLength >= 8)
        delete [] mFullSymbol;

    mLength = rhs.mLength;
    if (mLength < 8) {
        strcpy_s(mSymbol, rhs.mSymbol);
    }
    else {
        mFullSymbol = new(__FILE__, __LINE__) char[mLength + 1];
        strcpy(mFullSymbol, rhs.mFullSymbol);
    }
    
    mType   = rhs.mType;
    mKey    = rhs.mKey;
    mLine   = rhs.mLine;
    mColumn = rhs.mColumn;
    
    return *this;
}

// +-------------------------------------------------------------------+

bool
Token::match(const Token& ref) const
{
    if (mType == ref.mType) {                    // if types match 
        if (ref.mLength == 0)                     // if no symbol to match
            return true;                           // match!

        else if (mLength == ref.mLength) {        // else if symbols match
            if (mLength < 8) {
                if (!strcmp(mSymbol, ref.mSymbol))
                    return true;                     // match!
            }
            else {
                if (!strcmp(mFullSymbol, ref.mFullSymbol))
                    return true;                     // match!
            }
        }
    }
    
    return false;
}

// +-------------------------------------------------------------------+

Text
Token::symbol() const
{
    if (mLength < 8)
        return Text(mSymbol);
    else
        return Text(mFullSymbol);
}

// +-------------------------------------------------------------------+

void
Token::addKey(const Text& k, int v)
{
    keymap.insert(k, v);
}

// +-------------------------------------------------------------------+

void
Token::addKeys(Dictionary<int>& keys)
{
    DictionaryIter<int> iter = keys;
    while (++iter)
        keymap.insert(iter.key(), iter.value());
}

// +-------------------------------------------------------------------+

bool
Token::findKey(const Text& k, int& v)
{
    if (keymap.contains(k)) {
        v = keymap.find(k, 0);
        return true;
    }
    else
        return false;
}

// +-------------------------------------------------------------------+

void
Token::comments(const Text& begin, const Text& end)
{
    combeg[0] = begin(0);
    if (begin.length() > 1) combeg[1] = begin(1);
    else                    combeg[1] = '\0';   
        
    comend[0] = end(0);
    if (end.length() > 1)   comend[1] = end(1);
    else                    comend[1] = '\0';   
}     

// +-------------------------------------------------------------------+

void
Token::altComments(const Text& begin, const Text& end)
{
    altbeg[0] = begin(0);
    if (begin.length() > 1) altbeg[1] = begin(1);
    else                    altbeg[1] = '\0';   
        
    altend[0] = end(0);
    if (end.length() > 1)   altend[1] = end(1);
    else                    altend[1] = '\0';   
}     

// +-------------------------------------------------------------------+

Text
Token::typestr() const
{
    Text t = "Unknown";
    switch (type()) {
    case Undefined:      t = "Undefined"; break;
    case Keyword:        t = "Keyword"; break;
    case AlphaIdent:     t = "AlphaIdent"; break;
    case SymbolicIdent:  t = "SymbolicIdent"; break;
    case Comment:        t = "Comment"; break;
    case IntLiteral:     t = "IntLiteral"; break;
    case FloatLiteral:   t = "FloatLiteral"; break;
    case StringLiteral:  t = "StringLiteral"; break;
    case CharLiteral:    t = "CharLiteral"; break;
    case Dot:            t = "Dot"; break;
    case Comma:          t = "Comma"; break;
    case Colon:          t = "Colon"; break;
    case Semicolon:      t = "Semicolon"; break;
    case LParen:         t = "LParen"; break;
    case RParen:         t = "RParen"; break;
    case LBracket:       t = "LBracket"; break;
    case RBracket:       t = "RBracket"; break;
    case LBrace:         t = "LBrace"; break;
    case RBrace:         t = "RBrace"; break;
    case EOT:            t = "EOT"; break;
    case LastTokenType:  t = "LastTokenType"; break;
    }
    
    return t;
}

// +-------------------------------------------------------------------+

Text
Token::describe(const Text& tok)
{
    Text d;

    switch (tok(0)) {
    case '.' : d = "Token::Dot"; break;
    case ',' : d = "Token::Comma"; break;
    case ';' : d = "Token::Semicolon"; break;
    case '(' : d = "Token::LParen"; break;
    case ')' : d = "Token::RParen"; break;
    case '[' : d = "Token::LBracket"; break;
    case ']' : d = "Token::RBracket"; break;
    case '{' : d = "Token::LBrace"; break;
    case '}' : d = "Token::RBrace"; break;
    default  :               break;
    }
    
    if (d.length() == 0) {
        if (isalpha(tok(0)))
            d = "\"" + tok + "\", Token::AlphaIdent";
        else if (isdigit(tok(0))) {
            if (tok.contains("."))
                d = "\"" + tok + "\", Token::FloatLiteral";
            else
                d = "\"" + tok + "\", Token::IntLiteral";
        }
        else
            d = "\"" + tok + "\", Token::SymbolicIdent";
    }
    
    return d;
}

// +-------------------------------------------------------------------+

Scanner::Scanner(Reader* r)
	: reader(r), str('\0'), index(0), old_index(0),
      length(0), line(0), old_line(0), lineStart(0)
{ }

Scanner::Scanner(const Scanner& rhs)
    : index(rhs.index), old_index(rhs.old_index), length(rhs.length),
      reader(rhs.reader), str(rhs.str),
      line(rhs.line), old_line(0), lineStart(rhs.lineStart)
{
}

Scanner::Scanner(const Text& s)
    : reader(0), str(s), index(0), old_index(0), length(s.length()), line(0),
      old_line(0), lineStart(0)
{
}

Scanner::~Scanner()
{
}

// +-------------------------------------------------------------------+

Scanner&
Scanner::operator = (const Scanner& rhs)
{
    str = rhs.str;
    
    index     = rhs.index;
    old_index = rhs.old_index;
    length    = rhs.length;
    line      = rhs.line;
    old_line  = rhs.old_line;
    lineStart = rhs.lineStart;
    
    return *this;
}

// +-------------------------------------------------------------------+

void
Scanner::Load(const Text& s)
{
    str = s;
    index       = 0;
    old_index   = 0;
    best        = Token();
    length      = s.length();
    line        = 0;
    old_line    = 0;
    lineStart   = 0;
}

// +-------------------------------------------------------------------+

Token
Scanner::Get(Need need)
{
    int   type = Token::EOT;
    old_index  = index;
    old_line   = line;

    eos = str.length();
    p   = index;

    if (p >= eos) {
        if (need == Demand && reader) {
            Load(reader->more());
            if (length > 0)
                return Get(need);
        }
        return Token("", type, 0, line, 0);
    }

    while ( p < eos && isspace(str[p]) ) { // skip initial white space
        if (str[p] == '\n') {
            line++;
            lineStart = p;
        }
        p++;
    }
    
    if (p >= eos) {
        if (need == Demand && reader) {
            Load(reader->more());
            if (length > 0)
                return Get(need);
        }
        return Token("", type, 0, line, 0);
    }

    Token  result;
    size_t start = p;

    if (str[p] == '"' || str[p] == '\'') {   // special case for quoted tokens

        if (str[p] == '"') type = Token::StringLiteral;
        else           type = Token::CharLiteral;

        char match = str[p];
        while (++p < eos) {
            if (str[p] == match) {         // find matching quote
                if (str[p-1] != '\\') {   // if not escaped
                    p++;                 // token includes matching quote
                    break;
                }
            }
        }
    }
    
    // generic delimited comments
    else if (str[p] == Token::comBeg(0) &&
                     (!Token::comBeg(1) || str[p+1] == Token::comBeg(1))) {
        type = Token::Comment;
        while (++p < eos) {
            if (str[p] == Token::comEnd(0) &&
                      (!Token::comEnd(1) || str[p+1] == Token::comEnd(1))) {
                p++; if (Token::comEnd(1)) p++;
                break;
            }
        }
    }

    // alternate form delimited comments
    else if (str[p] == Token::altBeg(0) &&
                     (!Token::altBeg(1) || str[p+1] == Token::altBeg(1))) {
        type = Token::Comment;
        while (++p < eos) {
            if (str[p] == Token::altEnd(0) &&
                      (!Token::altEnd(1) || str[p+1] == Token::altEnd(1))) {
                p++; if (Token::altEnd(1)) p++;
                break;
            }
        }
    }

    else if (str[p] == '.')  type = Token::Dot;
    else if (str[p] == ',')  type = Token::Comma;
    else if (str[p] == ';')  type = Token::Semicolon;
    else if (str[p] == '(')  type = Token::LParen;
    else if (str[p] == ')')  type = Token::RParen;
    else if (str[p] == '[')  type = Token::LBracket;
    else if (str[p] == ']')  type = Token::RBracket;
    else if (str[p] == '{')  type = Token::LBrace;
    else if (str[p] == '}')  type = Token::RBrace;

    // use lexical sub-parser for ints and floats
    else if (isdigit(str[p]))
        type = GetNumeric();
    
    else if (IsSymbolic(str[p])) {
        type = Token::SymbolicIdent;
        while (IsSymbolic(str[p])) p++;
    }
    
    else {
        type = Token::AlphaIdent;
        while (IsAlpha(str[p])) p++;
    }

    size_t extent = p - start;

    if (extent < 1) extent = 1;      // always get at least one character

    index  = start + extent;         // advance the cursor
    int col = start - lineStart;
    if (line == 0) col++;
    
    Text buf = str.substring(start, extent);

    if (type == Token::Comment && Token::hidecom) {
        if (Token::comEnd(0) == '\n') {
            line++;
            lineStart = p;
        }
        return Get(need);
    }

    if (type == Token::AlphaIdent || // check for keyword
         type == Token::SymbolicIdent) {
         int val;
         if (Token::findKey(buf, val))
            result = Token(buf, Token::Keyword, val, line+1, col);
    }

    if (result.mType != Token::Keyword)
        result = Token(buf, type, 0, line+1, col);
    
    if (line+1 >  (size_t) best.mLine ||
        (line+1 == (size_t) best.mLine && col > best.mColumn))
        best = result;

    return result;
}

// +-------------------------------------------------------------------+

int
Scanner::GetNumeric()
{
    int type = Token::IntLiteral;             // assume int

    if (str[p] == '0' && str[p+1] == 'x') {         // check for hex:
        p += 2;
        while (isxdigit(str[p])) p++;
        return type;
    }

    while (isdigit(str[p]) || str[p] == '_') p++;     // whole number part
    
    if (str[p] == '.') { p++;                     // optional fract part
        type = Token::FloatLiteral;            // implies float

        while (isdigit(str[p]) || str[p] == '_') p++;  // fractional part
    }

    if (str[p] == 'E' || str[p] == 'e') {  p++;       // optional exponent
        if (str[p] == '+' || str[p] == '-') p++;       // which may be signed
        while (isdigit(str[p])) p++;

        type = Token::FloatLiteral;            // implies float
    }

    return type;
}

// +-------------------------------------------------------------------+

bool
Scanner::IsAlpha(char c)
{
    return (isalpha(str[p]) || isdigit(str[p]) || (str[p] == '_'))?true:false;
}

// +-------------------------------------------------------------------+

bool
Scanner::IsSymbolic(char c)
{
    const char* s = "+-*/\\<=>~!@#$%^&|:";
    return strchr(s, c)?true:false;
}
