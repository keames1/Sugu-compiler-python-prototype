import re
# Regular expression constants used to recognize certain tokens.
RE_C_VALID_IDENTIFIER_PTRN = re.compile(r"^[a-zA-Z_]+[a-zA-Z_0-9]*$")
#RE_C_VALID_STRUCT_ENUM_REF_PATTERN = re.compile(r"^([a-zA-Z_]+[a-zA-Z_0-9]*+\.)+[a-zA-Z_]+[a-zA-Z_0-9]*$")

# A function for generating unique constants with values that will be consistent throughout execution
# in cases where the specific values are irrelevant.
next_const = 0
def mkConst ():
    global next_const
    new_const = next_const
    next_const += 1
    return new_const

# Token type identifier constants.
TT_INTEGER                   = mkConst()
TT_FLOAT                     = mkConst()
TT_STRING                    = mkConst()
TT_CHAR                      = mkConst()
TT_KWD                       = mkConst()
TT_LCURLY                    = mkConst()
TT_RCURLY                    = mkConst()
TT_PLUS_EQU                  = mkConst()
TT_MINUS_EQU                 = mkConst()
TT_POW_EQU                   = mkConst()
TT_TIMES_EQU                 = mkConst()
TT_DIV_EQU                   = mkConst()
TT_MOD_EQU                   = mkConst()
TT_XOR_EQU                   = mkConst()
TT_FLOOR_DIV_EQU             = mkConst()

# A range of expression related tokens marked by the start and end constants EXPR_TKN_RANGE_START and END.
EXPR_TKN_RANGE_START         = mkConst()

TT_LPAREN                    = mkConst()
TT_RPAREN                    = mkConst()
TT_PLUS                      = mkConst()
TT_MINUS                     = mkConst()
TT_ASTERISK                  = mkConst()
TT_DBL_ASTERISK              = mkConst()
TT_FWD_SLASH                 = mkConst()
TT_DOUBLE_EQUAL              = mkConst()
TT_PERCENT                   = mkConst()
TT_ANDPERSAND                = mkConst()
TT_PIPE_SYM                  = mkConst()
TT_BACKSLASH                 = mkConst()
TT_TILDE                     = mkConst()
TT_CARROT                    = mkConst()
TT_NOT_EQU                   = mkConst()
TT_LESS_THAN                 = mkConst()
TT_GREATER_THAN              = mkConst()
TT_LESS_THAN_EQUAL           = mkConst()
TT_GREATER_THAN_EQUAL        = mkConst()
TT_LEFT_SHIFT                = mkConst()
TT_RIGHT_SHIFT               = mkConst()
TT_IN_PLACE_INCR             = mkConst()
TT_IN_PLACE_DECR             = mkConst()

EXPR_TKN_RANGE_END           = mkConst()

TT_DBL_FWD_SLASH             = mkConst()
TT_EQUAL                     = mkConst()
TT_ILLEGAL_TOKEN             = mkConst()
TT_END_OF_FILE               = mkConst()
TT_IDENTIFIER                = mkConst()
TT_MEMBER_FIELD_REF          = mkConst()
TT_SEMICOLON                 = mkConst()
TT_COLON                     = mkConst()
TT_OCTOTHORPE                = mkConst()
TT_NEWLINE                   = mkConst()
TT_DOLLAR_SIGN               = mkConst()
TT_COMMA                     = mkConst()

# token type any makes Token.compare ignore the type of the token.
TT_ANY                       = mkConst()
# Token type None guarantees the Token.compare method will return False.
TT_NONE                      = mkConst()

# A token type map used for creating string representations of tokens.
TOKEN_TYPE_MAP = {
    TT_INTEGER                   : "INTEGER",
    TT_FLOAT                     : "FLOAT",
    TT_KWD                       : "KEYWORD",
    TT_LPAREN                    : "LPAREN",
    TT_RPAREN                    : "RPAREN",
    TT_PLUS                      : "PLUS",
    TT_MINUS                     : "MINUS",
    TT_ASTERISK                  : "ASTERISK",
    TT_DBL_ASTERISK              : "DBL_ASTERISK",
    TT_FWD_SLASH                 : "FWD_SLASH",
    TT_DBL_FWD_SLASH             : "DBL_FWD_SLASH",
    TT_EQUAL                     : "EQUAL",
    TT_DOUBLE_EQUAL              : "DBL_EQUAL",
    TT_PERCENT                   : "PERCENT",
    TT_TILDE                     : "TILDE",
    TT_ANDPERSAND                : "ANDPERSAND",
    TT_PIPE_SYM                  : "PIPE_SYM",
    TT_CARROT                    : "CARROT",
    TT_NOT_EQU                   : "NOT_EQUAL",
    TT_LESS_THAN                 : "LESS_THAN",
    TT_GREATER_THAN              : "GREATER_THAN",
    TT_LESS_THAN_EQUAL           : "LESS_THAN_EQUAL",
    TT_GREATER_THAN_EQUAL        : "GREATER_THAN_EQUAL",
    TT_ILLEGAL_TOKEN             : "ILLEGAL_TOKEN",
    TT_END_OF_FILE               : "END_OF_FILE",
    TT_IDENTIFIER                : "IDENTIFIER",
    TT_MEMBER_FIELD_REF          : "MEMBER_FIELD_REF",
    TT_LEFT_SHIFT                : "LEFT_SHIFT",
    TT_RIGHT_SHIFT               : "RIGHT_SHIFT",
    TT_STRING                    : "STRING",
    TT_CHAR                      : "CHAR",
    TT_BACKSLASH                 : "BACKSLASH",
    TT_SEMICOLON                 : "SEMICOLON",
    TT_COLON                     : "COLON",
    TT_OCTOTHORPE                : "OCTOTHORPE",
    TT_NEWLINE                   : "NEWLINE",
    TT_ANY                       : "ANY",
    TT_DOLLAR_SIGN               : "DOLLAR_SIGN",
    TT_COMMA                     : "COMMA",
    TT_NONE                      : "NONE",
    TT_LCURLY                    : "LCURLY",
    TT_RCURLY                    : "RCURLY",
    TT_IN_PLACE_INCR             : "IN_PLACE_INCREMENT",
    TT_IN_PLACE_DECR             : "IN_PLACE_DECREMENT",
    TT_PLUS_EQU                  : "PLUS_EQU",
    TT_MINUS_EQU                 : "MINUS_EQU",
    TT_POW_EQU                   : "POW_EQU",
    TT_TIMES_EQU                 : "TIMES_EQU",
    TT_DIV_EQU                   : "DIV_EQU",
    TT_MOD_EQU                   : "MOD_EQU",
    TT_XOR_EQU                   : "XOR_EQU",
    TT_FLOOR_DIV_EQU             : "FLOOR_DIVIDE_EQU"
}

# Character set constants:
CS_WHITESPACE = " \t"
CS_NEWLINE    = "\n\r"
CS_NUMERALS   = "0123456789"
CS_HEX_DIGITS = "ABCDEFabcdef" + CS_NUMERALS
CS_OPERATORS  = "+-*/%&|^=!<>"
CS_WORD_CHRS  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz" + CS_NUMERALS

# Keyword constants
KWD_AND         = "and"   # Keyword operators.
KWD_OR          = "or"
KWD_NOT         = "not"
KWD_IF          = "if"    # Greyzone. They're both the keywords in
KWD_ELIF        = "elif"  # conditional statements and keyword operators
KWD_ELSE        = "else"  # in conditional expressions.

KWD_I8          = "i8"    # Data type keywords
KWD_I16         = "i16"
KWD_I32         = "i32"
KWD_I64         = "i64"
KWD_IEXP        = "Iexp"
KWD_U8          = "u8"
KWD_U16         = "u16"
KWD_U32         = "u32"
KWD_U64         = "u64"
KWD_F32         = "f32"
KWD_F64         = "f64"
KWD_FEXP        = "Fexp"
KWD_STR         = "str"
KWD_STRING      = "String"

KWD_CHAR        = "char"
KWD_BOOL        = "bool"
KWD_REF         = "ref"

KWD_NONRUNTIME  = ".nonruntime"  # Directive keywords signifying code to be interpreted during compile time.
KWD_END         = ".end"
KWD_AVAILABLE   = ".available"   # Declares constants to be allowed for manipulation by nonruntime code.
KWD_UNAVAILABLE = ".unavailable" # Declares constants to be unavailable for manipulation by nonruntime code.

# A keyword tuple used for determining wheather a certain sequence is a keyword.
KS_ALL_KWDS   = (
  KWD_AND, KWD_OR,   KWD_NOT,  KWD_IF,   KWD_ELIF, KWD_ELSE,
  
  KWD_I8,  KWD_I16,  KWD_I32,  KWD_I64,  KWD_IEXP, KWD_F32,
  KWD_F64, KWD_FEXP, KWD_CHAR, KWD_BOOL, KWD_REF, KWD_STR,
  KWD_STRING, KWD_U8, KWD_U16, KWD_U32, KWD_U64, 
  
  KWD_NONRUNTIME, KWD_END, KWD_AVAILABLE, KWD_UNAVAILABLE,
)
# Tuples of keywords used for identifying specific types of keywords, such as keyword operators.
KS_LOGICAL_OPERATORS = (KWD_AND, KWD_OR, KWD_NOT, )
KS_CONDITIONAL_KWDS = (KWD_IF, KWD_ELIF, KWD_ELSE, )
KS_KWD_OPERATORS = (KWD_AND, KWD_OR, KWD_NOT, KWD_IF, KWD_ELIF, KWD_ELSE, )

KS_TYPE_KWDS = (
  KWD_I8,  KWD_I16,  KWD_I32,  KWD_I64,  KWD_IEXP, KWD_F32,
  KWD_F64, KWD_FEXP, KWD_CHAR, KWD_BOOL, KWD_REF,
  KWD_STR, KWD_STRING, KWD_U8, KWD_U16, KWD_U32, KWD_U64, 
)
KS_NONRUNTIME_RELATED = (KWD_NONRUNTIME, KWD_END, KWD_AVAILABLE, KWD_UNAVAILABLE, )

def isIdentifier (aStr):
    return RE_C_VALID_IDENTIFIER_PTRN.search(aStr) and aStr not in KS_ALL_KWDS

# def isStructEnumRef (aStr):
    # return RE_C_VALID_STRUCT_ENUM_REF_PATTERN.search(aStr) and aStr not in KS_ALL_KWDS

class TV_ANY:  # An instance of TV_ANY in the value field of a Token instance makes the compare method
               # ignore the value field in the comparison.

    def __repr__ (self):
        return "***ANY***"

class TV_NONE: # An instance of TV_NONE in the value field of a Token instance makes the compare method
               # always return False.

    def __repr__ (self):
        return "***NONE***"

# Comparison modes for the the compare method of the Token class
CM_TYPE_AND_VALUE            = mkConst()
CM_TYPE_ONLY                 = mkConst()
CM_VALUE_ONLY                = mkConst()
CM_COMPARE_NOTHING           = mkConst()

class Token:

    def __init__ (self, tokType, tokVal, startIdx = None, endIdx = None, fileName = None):
        self.tokType = tokType
        self.tokVal = tokVal
        self.startIdx = startIdx
        self.endIdx = endIdx
        self.fileName = fileName

    def compare (self, other):

        if self.tokType == TT_NONE or type(self.tokVal) is TV_NONE: return False
        if other.tokType == TT_NONE or type(other.tokVal) is TV_NONE: return False

        ignoreType = False
        ignoreValue = False
        if other.tokType == TT_ANY or self.tokType == TT_ANY: ignoreType = True
        if type(self.tokVal) is TV_ANY or type(other.tokVal) is TV_ANY: ignoreValue = True
        mode = {
        (False, False) : CM_TYPE_AND_VALUE,
        (False, True ) : CM_TYPE_ONLY,
        (True , False) : CM_VALUE_ONLY,
        (True , True ) : CM_COMPARE_NOTHING,
        }[(ignoreType, ignoreValue)]

        if mode == CM_COMPARE_NOTHING:

            return True

        elif mode == CM_TYPE_AND_VALUE:

            if self.tokType != other.tokType:
                return False

            if type(self.tokVal) == type(other.tokVal):
                return self.tokVal == other.tokVal

            else:
                return False

        elif mode == CM_TYPE_ONLY:

            return self.tokType == other.tokType

        elif mode == CM_VALUE_ONLY:

            if type(self.tokVal) == type(other.tokVal):
                return self.tokVal == other.tokVal

            else:
                return False

        def copy (self):
            return Token(self.tokType, self.tokVal, self.startIdx, self.endIdx, self.fileName)

    def __repr__ (self):

        return f"[{TOKEN_TYPE_MAP[self.tokType]}, {self.tokVal}]"

DEFAULT_ERR_TEMPLATE = """Unspecified error in '{}' from line {}, col {} to line {}, col {}.

    {}"""

class GeneralError: # A superclass from which all my error classes inherit.

    def __init__ (self, startPos, endPos, srcCode, fileName, message):
        self.startPos = startPos
        self.endPos = endPos
        self.srcCode = srcCode
        self.fileName = fileName
        self.message = message
        self.template = DEFAULT_ERR_TEMPLATE

    def getLineColStartEnd (self):
        linePos = 1
        colPos = 0
        startLine = 0
        startCol = 0
        for i, c in enumerate(self.srcCode[:self.endPos + 1]):

            if c in CS_NEWLINE:
                linePos += 1
                colPos = 0
            else:
                colPos += 1

            if i == self.startPos:
                startLine = linePos
                startCol = colPos

        return (startLine,
                startCol,
                linePos,   # Since the loop ended on the ending position, these are the correct values
                colPos,    # for the end of the error.
               )

    def __repr__ (self):

        startLine, startCol, endLine, endCol = self.getLineColStartEnd()
        return self.template.format(self.fileName, startLine, startCol, endLine, endCol, self.message)

LEX_ERR_TEMPLATE = """A lexing error occurred in '{}' beginning on line {}, col {} and ending on line {}, col {}.

    {}"""

class LexError (GeneralError):
    def __init__ (self, startPos, endPos, srcCode, fileName, message):
        super().__init__(startPos, endPos, srcCode, fileName, message)
        self.template = LEX_ERR_TEMPLATE

class Lexer:

    # A negative number is never used for indexing in this class. Loops that
    # iterate over the code can just check whether the idx instance variable is
    # greater than or equal to 0.
    IDX_SENTINAL_VALUE = -2048

    def __init__(self, srcCode, fileName):

        self.srcCode = srcCode
        self.idx = -1
        self.char = None
        self.toNext()
        self.fileName = fileName

    def toNext (self): # This is exactly as it needs to be. Don't mess with it.
        self.idx += 1
        if self.idx < len(self.srcCode) and self.idx >= 0:
            self.char = self.srcCode[self.idx]
        else:
            self.idx = Lexer.IDX_SENTINAL_VALUE
            self.char = ""

    def shellReset (self, newSrc): # A method used by the shell to feed a new line of code into the lexer
        self.srcCode = newSrc
        self.idx = -1
        self.char = None
        self.toNext()

    def genTokens (self):
        tokens = [Token(TT_NEWLINE, TV_ANY(), 0, 1, self.fileName)]

        while self.idx >= 0:

            # Whitespace gets ignored here, becoming nothing more than a delimiter.
            if self.char in CS_WHITESPACE:
                pass

            elif self.char == '+':
                tokens.append(Token(TT_PLUS, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '-':
                tokens.append(Token(TT_MINUS, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '*':
                aToken = None
                if len(self.srcCode) > self.idx + 1:
                    if self.srcCode[self.idx + 1] == '*':
                        aToken = Token(TT_DBL_ASTERISK, '**', self.idx, self.idx + 2, self.fileName)
                        self.toNext() # Skip over the second asterisk. It's already part of this token.
                    else:
                        aToken = Token(TT_ASTERISK, self.char, self.idx, self.idx + 2, self.fileName)
                else:
                    aToken = Token(TT_ASTERISK, self.char, self.idx, self.idx + 2, self.fileName)

                tokens.append(aToken)

            elif self.char == '/':
                if len(self.srcCode) > self.idx + 1:
                    if self.srcCode[self.idx + 1] == '/':
                        tokens.append(Token(TT_DBL_FWD_SLASH, '//', self.idx, self.idx + 1, self.fileName))
                    else:
                        tokens.append(Token(TT_FWD_SLASH, self.char, self.idx, self.idx + 1, self.fileName))
                else:
                    tokens.append(Token(TT_FWD_SLASH, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '~':
                tokens.append(Token(TT_TILDE, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '&':
                tokens.append(Token(TT_ANDPERSAND, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '%':
                tokens.append(Token(TT_PERCENT, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '(':
                tokens.append(Token(TT_LPAREN, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == ')':
                tokens.append(Token(TT_RPAREN, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '^':
                tokens.append(Token(TT_CARROT, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '|':
                tokens.append(Token(TT_PIPE_SYM, self.char, self.idx, self.idx + 1, self.fileName))
            
            elif self.char == '#':
                self.ignoreComment(multiline = False)
            
            elif self.char == '{':
                if len(self.srcCode) > self.idx + 1:
                    if self.srcCode[self.idx + 1] == '*':
                        self.ignoreComment(multiline = True)
                    else:
                        tokens.append(Token(TT_LCURLY, self.char, self.idx, self.idx +1, self.fileName))
                else:
                    tokens.append(Token(TT_LCURLY, self.char, self.idx, self.idx +1, self.fileName))
            
            elif self.char == '}':
                if self.srcCode[self.idx - 1] == '*':
                    return None, LexError(
                      self.idx - 1,
                      self.idx + 1,
                      self.srcCode,
                      self.fileName,
                      "Found an end multiline comment tag '*}' but no accompanying '{*'."
                    ),
                tokens.append(Token(TT_RCURLY, self.char, self.idx, self.idx +1, self.fileName))

            elif self.char == '\\':
                tokens.append(Token(TT_BACKSLASH, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == ',':
                tokens.append(Token(TT_COMMA, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == ':':
                tokens.append(Token(TT_COLON, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '#':
                tokens.append(Token(TT_OCTOTHORPE, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '$':
                tokens.append(Token(TT_DOLLAR_SIGN, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char in CS_NEWLINE + ';':
                tokens.append(Token(TT_NEWLINE, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '=':
                aToken = None
            
                # If there is an operator in the previous index, we want to reassign the last token
                # to be an in place assignment operator.
                if self.idx > 0 and self.srcCode[self.idx - 1] == '+':
                    tokens[-1] = Token(TT_PLUS_EQU, '+=', self.idx -2, self.idx + 1, self.fileName)
                
                elif self.idx > 0 and self.srcCode[self.idx - 1] == '-':
                    tokens[-1] = Token(TT_MINUS_EQU, '-=', self.idx -2, self.idx + 1, self.fileName)
                
                elif self.idx > 0 and self.srcCode[self.idx - 2: self.idx + 1] == '**=':
                    tokens[-1] = Token(TT_POW_EQU, '**=', self.idx -3, self.idx + 1, self.fileName)
                
                elif self.idx > 0 and self.srcCode[self.idx - 1] == '*':
                    tokens[-1] = Token(TT_TIMES_EQU, '*=', self.idx -2, self.idx + 1, self.fileName)
                
                elif self.idx > 0 and self.srcCode[self.idx - 2: self.idx + 1] == '//=':
                    tokens[-1] = Token(TT_FLOOR_DIV_EQU, '//=', self.idx -3, self.idx + 1, self.fileName)
                    del tokens[-2]
                
                elif self.idx > 0 and self.srcCode[self.idx - 1] == '/':
                    tokens[-1] = Token(TT_DIV_EQU, '/=', self.idx -2, self.idx + 1, self.fileName)
                
                elif self.idx > 0 and self.srcCode[self.idx - 1] == '%':
                    tokens[-1] = Token(TT_MOD_EQU, '%=', self.idx -2, self.idx + 1, self.fileName)
                
                elif self.idx > 0 and self.srcCode[self.idx - 1] == '^':
                    tokens[-1] = Token(TT_XOR_EQU, '^=', self.idx -2, self.idx + 1, self.fileName)
                
                elif self.idx > 0 and self.srcCode[self.idx - 1] == '=':
                    tokens[-1] = Token(TT_DOUBLE_EQUAL, '==', self.idx -2, self.idx + 1, self.fileName)
                
                else:
                    aToken = Token(TT_EQUAL, self.char, self.idx, self.idx + 1, self.fileName)
                
                if aToken: tokens.append(aToken)

            elif self.char == '!':
                if len(self.srcCode) > self.idx + 1:
                    if self.srcCode[self.idx + 1] == '=':
                        tokens.append(Token(TT_NOT_EQU, self.char + '=', self.idx, self.idx + 1, self.fileName))
                        self.toNext() # Skip over the equal sign. It's already part of this token.
                    else:
                        return None, LexError(
                          self.idx,
                          self.idx + 2,
                          self.srcCode,
                          self.fileName,
                          f"Illegal token '{self.srcCode[self.idx: self.idx + 2]}'. This character sequence is unrecognized."
                        ),
                else:
                    return None, LexError(
                      self.idx,
                      self.idx + 1,
                      self.srcCode,
                      self.fileName,
                      f"An exclamation mark unaccompanied by an = is not a recognized token."
                    ),

            elif self.char == '<':
                aToken = None
                if self.idx + 1 < len(self.srcCode):
                    if self.srcCode[self.idx + 1] == '=':
                        aToken = Token(TT_LESS_THAN_EQUAL, '<=', self.idx, self.idx + 2, self.fileName)
                        self.toNext() # Skip over the equals sign. It's already been processed.
                    elif self.srcCode[self.idx + 1] == '<':
                        aToken = Token(TT_LEFT_SHIFT, '<<', self.idx, self.idx + 2, self.fileName)
                        self.toNext() # Skip over the other <. It's already been processed.
                    elif self.srcCode[self.idx + 1] == '+':
                        aToken = Token(TT_IN_PLACE_INCR, '<+', self.idx, self.idx + 2, self.fileName)
                        self.toNext()
                    elif self.srcCode[self.idx + 1] == '-':
                        aToken = Token(TT_IN_PLACE_DECR, '<-', self.idx, self.idx + 2, self.fileName)
                        self.toNext()
                    else:
                        aToken = Token(TT_LESS_THAN, self.char, self.idx, self.idx + 1, self.fileName)
                else:
                    aToken = Token(TT_LESS_THAN, self.char, self.idx, self.idx + 1, self.fileName)
                tokens.append(aToken)

            elif self.char == '>':
                aToken = None
                if self.idx + 1 < len(self.srcCode):
                    if self.srcCode[self.idx + 1] == '=':
                        aToken = Token(TT_GREATER_THAN_EQUAL, '>=', self.idx, self.idx + 2, self.fileName)
                        self.toNext() # Skip over the equals sign. It's already been processed.
                    elif self.srcCode[self.idx + 1] == '>':
                        aToken = Token(TT_RIGHT_SHIFT, '>>', self.idx, self.idx + 2, self.fileName)
                        self.toNext()
                    else:
                        aToken = Token(TT_GREATER_THAN, self.char, self.idx, self.idx + 1, self.fileName)
                else:
                    aToken = Token(TT_GREATER_THAN, self.char, self.idx, self.idx + 1, self.fileName)
                tokens.append(aToken)

            elif self.char == '^':
                tokens.append(Token(TT_CARROT, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '"':

                aToken, err = self.getString()

                if err:
                    return None, err,

                tokens.append(aToken)

            elif self.char == "'":

                aToken, err = self.getChar()

                if err:
                    return None, err,

                tokens.append(aToken)

            elif self.char in CS_NUMERALS: # Generate tokens for integers and floating point.

                aToken, err = self.intConvert()

                if err:
                    return None, err,

                tokens.append(aToken)

            elif self.char in CS_WORD_CHRS + '.': # Generate tokens for identifiers and keywords.

                aToken, err = self.getIdentifierOrKeyword()

                if err:
                    return None, err,

                tokens.append(aToken)

            else:
                return None, LexError(
                  self.idx,
                  self.idx + 1,
                  self.srcCode,
                  self.fileName,
                  f"Illegal character '{self.char}'. This character is not allowed in the source code."
                ),

            self.toNext()

        tokens.append(Token(TT_END_OF_FILE, "End of file", self.idx, self.idx + 1, self.fileName))
        return tokens, None,

    def ignoreComment(self, multiline):

        # Advance past the comment tag to avoid registering it again. At the initial call,
        # self.idx is pointing to the backslash but the slicing logic inside the ignore loop
        # recognizes end comment tags when self.idx points to the asterisk.
        # {*...
        # ^ We are here.
        self.toNext(); self.toNext(); self.toNext()
        # {*...
        #    ^ Now we are here.
        nestingDepth = 0
        while self.idx >= 0:

            if multiline:
                if self.srcCode[self.idx - 1: self.idx + 1] == '{*':
                    nestingDepth += 1
                    self.toNext()
                elif self.srcCode[self.idx - 1: self.idx + 1] == '*}' and nestingDepth > 0:
                    nestingDepth -= 1
                    self.toNext()
                elif self.srcCode[self.idx - 1: self.idx + 1] == '*}' and nestingDepth <= 0:
                    return

            else:
                if self.char == '\n': return

            self.toNext()

    def getString (self):

        strStr = self.char
        isBlockStr = False
        startPos = self.idx

        # Determine whether the string is a block string, an inline string, or an empty string.
        self.toNext()
        strStr += self.char
        if strStr == '""':                             # Could be empty string or block string.
            if len(self.srcCode) > self.idx + 1:
                if self.srcCode[self.idx + 1] == '"':  # Is most definitely a block string.
                    self.toNext()
                    strStr += self.char
                    isBlockStr = True
                else:
                    return Token(TT_STRING, "", startPos, self.idx + 1, self.fileName), None,

        while self.idx >= 0:

            # Determine whether to advance or break out of the loop in a manner
            # according to whether the string is a block string or inline.
            if isBlockStr:
                if len(strStr) >= 6: # We wouldn't want to register the first 3 " as the end of the string.
                    if strStr[-3:] == '"""' and strStr[-4:] != '\\"""':
                        break
                    else:
                        self.toNext()
                else:
                    self.toNext()
            else:
                if len(strStr) >= 2: # Let's not register the first quotation mark as the end of the string.
                    if strStr[-1] == '"' and strStr[-2:] != '\\"':
                        break
                    else:
                        self.toNext()
                else:
                    self.toNext()

            strStr += self.char

            # Yell and scream at the programmer if they typed an unescaped newline.
            if strStr[-1] == '\n' and strStr[-2:] != '\\\n':
                return None, LexError(
                  startPos,
                  self.idx + 1,
                  self.srcCode,
                  self.fileName,
                  "Unescaped newline found within string."
                ),

        # Try to evaluate the string and yell and scream at the programmer if the eval failed.
        try:
            strStr = eval(strStr)
        except SyntaxError as err:
            return None, LexError(
              startPos,
              self.idx + 1,
              self.srcCode,
              self.fileName,
              f"Invalid string literal. {err}"
            ),

        return Token(TT_STRING, strStr, startPos, self.idx + 1, self.fileName), None,

    def getChar (self):

        charStr = self.char
        startPos = self.idx
        self.toNext()

        while self.idx >= 0:

            charStr += self.char

            if len(self.srcCode) > self.idx + 1:
                if self.char != "'" or len(charStr) < 2: self.toNext()
                else: break
            else:
                break

            if len(charStr) >= 2:
                if charStr[-1] == "'" and charStr[-2:] != "\\'":
                    break

            # Yell and scream if the character contains an unescaped newline.
            if charStr[-1] == '\n' and charStr[-2:] != '\\\n':
                return None, LexError(
                  startPos,
                  self.idx + 1,
                  self.srcCode,
                  self.fileName,
                  "Unescaped newline in character literal."
                ),

        # Try to evaluate the character literal and yell and scream if the eval fails.
        try:
            charStr = eval(charStr)
        except SyntaxError as err:
            return None, LexError(
              startPos,
              self.idx + 1,
              self.srcCode,
              self.fileName,
              f"Invalid character literal. {err}"
            ),

        # Yell and scream if the programmer wrote a character literal which evaluates to
        # multiple characters.
        if len(charStr) > 1:
            return None, LexError(
              startPos,
              self.idx + 1,
              self.srcCode,
              self.fileName,
              f"Invalid character literal '{charStr}' evaluates to length greater than one."
            ),

        return Token(TT_CHAR, charStr, startPos, self.idx + 1, self.fileName), None,

    def getIdentifierOrKeyword (self):

        # It's unknown whether it's a keyword or identifier at this time, so wordStr it is.
        wordStr = self.char
        dotCount = 0
        previousCharWasDot = False
        startPos = self.idx

        # We'll also want to handle references to struct fields, enum members, and module
        # contents, all of which use the dotted notation, so the dot is included in the
        # characters to be recognized as part of the sequence being converted in this token.
        while self.char in CS_WORD_CHRS + '.' and self.idx >= 0:

            if self.idx + 1 < len(self.srcCode):
                if self.srcCode[self.idx + 1] in CS_WORD_CHRS + '.': self.toNext()
                else: break
            else:
                break

            wordStr += self.char

            # Yell and scream if the user types a member/field reference with 2
            # consecutive dots: For example: 'foo..bar'.
            if previousCharWasDot and self.char == '.':
                return None, LexError(
                  startPos,
                  self.idx + 1,
                  self.srcCode,
                  self.fileName,
                  f"Two consecutive dots in the member/field reference '{wordStr}'. The space between these dots must contain a valid identifier."
                ),

            if self.char == '.':
                previousCharWasDot = True
                dotCount += 1
            else:
                previousCharWasDot = False

        # Yell and scream if the user typed a dot at the beginning of the string. Some compiler directive keywords
        # in the future like '.nonruntime' may be written like this so I want to leave those alone.
        if wordStr[0] == '.' and wordStr not in KS_ALL_KWDS:
            return None, LexError(
              startPos,
              self.idx + 1,
              self.srcCode,
              self.fileName,
              f"A dot in a member/field reference must be preceeded by a valid identifier and '{wordStr}' is not a keyword."
            ),

        # Yell and scream at the programmer if they typed a dot at the end. For example, 'invalid.'.
        if wordStr[-1] == '.':
            return None, LexError(
              startPos,
              self.idx + 1,
              self.srcCode,
              self.fileName,
              f"A dot in a member field reference must be proceeded by a valid identifier. '{wordStr}' is invalid."
            ),

        # Yell and scream if the programmer typed a numeral at the beginning of their identifier.
        if wordStr[0] in CS_NUMERALS:
            return None, LexError(
              startPos,
              self.idx + 1,
              self.srcCode,
              self.fileName,
              f"Identifiers cannot begin in numerals! The identifier {wordStr} is invalid."
            ),

        if dotCount == 0 and wordStr not in KS_ALL_KWDS:
            return Token(TT_IDENTIFIER, wordStr, startPos, self.idx + 1, self.fileName), None,
        elif wordStr in KS_ALL_KWDS:
            return Token(TT_KWD, wordStr, startPos, self.idx + 1, self.fileName), None,
        elif dotCount != 0 and wordStr not in KS_ALL_KWDS:
            return Token(TT_MEMBER_FIELD_REF, wordStr, startPos, self.idx + 1, self.fileName), None,

    def intConvert (self):

        numStr = self.char
        number = 0
        sawDecimal = False
        startPos = self.idx

        while self.char in CS_HEX_DIGITS + "_xXbBdD." and self.idx >= 0:

            if self.idx + 1 < len(self.srcCode):
                if self.srcCode[self.idx + 1] in CS_HEX_DIGITS + "_xXbBdD.": self.toNext()
                else: break
            else:
                break

            numStr += self.char

            if self.char == '.' and not sawDecimal: sawDecimal = True
            elif sawDecimal and self.char == '.':
                return None, LexError(
                  startPos,
                  self.idx + 1,
                  self.srcCode,
                  self.fileName,
                  f"Multiple decimal points in floating point value '{numStr}'. There can only be one decimal point."
                ),

        # Support for explicit decimal numbers:
        if numStr[0:2].upper() == "0D":
            try:
                if sawDecimal: number = float(numStr[2:])
                else: number = int(numStr[2:])

            except ValueError:
                return None, LexError(
                  startPos,
                  self.idx + 1,
                  self.srcCode,
                  self.fileName,
                  f"Invalid identifier or integer/float literal '{numStr}'."
                ),
        # Yell and scream if the programmer tries to code hexadecimal or binary floating point literals.
        elif numStr[0:2].upper() in ["0X", "0B"] and sawDecimal:
            return None, LexError(
              startPos,
              self.idx + 1,
              self.srcCode,
              self.fileName,
              f"Invalid floating point literal '{numStr}'. Floating points can only be represented in decimal."
            ),
        # Handle the case of hexadecimal integers.
        elif numStr[0:2].upper() == "0X":
            try:
                number = int(numStr[2:], 16)
            except ValueError:
                return None, LexError(
                  startPos,
                  self.idx + 1,
                  self.srcCode,
                  self.fileName,
                  f"Invalid identifier or integer literal '{numStr}'."
                ),
        # Handle the case of binary integers.
        elif numStr[0:2].upper() == "0B":
            try:
                number = int(numStr[2:], 2)
            except ValueError:
                return None, LexError(
                  startPos,
                  self.idx + 1,
                  self.srcCode,
                  self.fileName,
                  f"Invalid identifier or integer literal '{numStr}'."
                ),
        # If none of the other cases apply, we can just assume it's a standard decimal.
        else:
            try:
                if sawDecimal: number = float(numStr)
                else:          number = int(numStr)
            except ValueError:
                return None, LexError(
                  startPos,
                  self.idx + 1,
                  self.srcCode,
                  self.fileName,
                  f"Invalid identifier or integer literal '{numStr}'."
                ),

        # Generate the token and return it.
        if sawDecimal:
            return Token(TT_FLOAT, number, startPos, self.idx + 1, self.fileName), None,
        else:
            return Token(TT_INTEGER, number, startPos, self.idx + 1, self.fileName), None,

# Tree type constants used for designating the type of tree being parsed. Different subtrees will call
# different parse methods based on their assigned type.
TR_ROOT                  =               mkConst() # The root AST.

TR_ASSIGN_EXPR           =               mkConst() # The subtrees that can appear in a root tree.
TR_VAR_DECLARATION       =               mkConst()
TR_EXPR                  =               mkConst()

# Token constants used by the parser.
TC_NONE_TOKEN            =               Token(TT_NONE, TV_NONE())
TC_NEWLINE               =               Token(TT_NEWLINE, TV_ANY())
TC_SEMICOLON             =               Token(TT_NEWLINE, ';')
TC_IDENTIFIER            =               Token(TT_IDENTIFIER, TV_ANY())
TC_EQUAL                 =               Token(TT_EQUAL, TV_ANY())
TC_DOLLAR_SIGN           =               Token(TT_DOLLAR_SIGN, TV_ANY())
TC_END_OF_FILE           =               Token(TT_END_OF_FILE, TV_ANY())
TC_KWD                   =               Token(TT_KWD, TV_ANY())
TC_LINEFILL              =               Token(TT_PIPE_SYM, TV_ANY())
TC_PIPE_SYM              =               TC_LINEFILL
TC_LPAREN                =               Token(TT_LPAREN, TV_ANY())
TC_MEMBER_FIELD_REF      =               Token(TT_MEMBER_FIELD_REF, TV_ANY())

SYNTAX_ERR_TEMPLATE = """A syntax error occured in {} begininning on line {}, col {} and ending on line {}, col {}.

    {}"""

class SyntxError (GeneralError):

    def __init__ (self, startPos, endPos, srcCode, fileName, message):
        super().__init__ (startPos, endPos, srcCode, fileName, message)
        self.template = SYNTAX_ERR_TEMPLATE

START_OF_LINE = 0

# A parse tree base class. Instances of this class represent the program parse tree and subtree classes
# inherit from this class to satisfy the DRY gods.
class ParseTree:

    IDX_SENTINAL_VALUE = -2048

    def __init__ (self, tokens, startIdx, srcCode = None, fileName = None, treeType = TR_ROOT):

        self.tokens = tokens
        self.idx = startIdx
        self.currTok = self.tokens[self.idx]
        self.treeType = treeType
        self.children = []

        # Yell and scream if srcCode and fileName weren't passed and aren't currently assigned.
        if not hasattr(self, "srcCode") and srcCode is None:
            raise TypeError("As the srcCode attribute is not yet assigned, expected srcCode to be passed to \
this constructor.")

        if not hasattr(self, "fileName") and fileName is None:
            raise TypeError("As the fileName attribute is not yet assigned, expected fileName to be passed \
to this constructor.")

        if not hasattr(self, "srcCode") or srcCode is not None:
            self.srcCode = srcCode

        if not hasattr(self, "fileName") or fileName is not None:
            self.fileName = fileName

    def toNext():
        self.idx += 1
        if self.idx < len(self.tokens) and self.idx >= 0:
            self.currTok = self.tokens[self.idx]
        else:
            self.idx = ParseTree.IDX_SENTINAL_VALUE
            self.currTok = TC_NONE_TOKEN

    def compareTknSequence (self, aSeq, bSeq):

        if len(aSeq) != len(bSeq): return False

        for idx, tok in enumerate(aSeq):

            if not tok.compare(bSeq[idx]): return False

        return True

    def integrate (self, aSubtree):
        self.idx = aSubTree.idx
        self.currTok = self.tokens[self.idx]
        self.children.append(aSubtree)

    def parse (self):

        # Call the appropriate parse method based on the tree type:
        appropriateMethod = {

          TR_ROOT              : self.parse_root,

          TR_VAR_DECLARATION   : self.parse_var_declaration,
          TR_ASSIGN_EXPR       : self.parse_assign_expr,
          TR_EXPR              : self.parse_expr,

        }[self.treeType]

        parseResult = appropriateMethod()
        # Return the tuple (ParseTree instance or None, error object or None).
        return parseResult

##########################################################################################
#                                      PARSE_ROOT

    def parse_root (self):

        # The sequence of tokens currently being analyzed.
        currentSequence = []
        # The start index of the sequence of tokens being analyzed.
        startIdx = self.idx
        # While there are tokens to be parsed, parse on! The sentinal
        # value is always positive.
        while self.idx <= 0:

            if not self.currTok.compare(TC_NEWLINE):
                currentSequence.append(self.currTok)
            else:
                currentSequence = []
                startIdx = self.idx + 1
                # Yell and scream if there are multiple consecutive semicolons.
                if self.currTok.compare(TC_SEMICOLON
                  ) and self.tokens[self.idx - 1
                  ].compare(TC_SEMICOLON):

                    return None, SyntxError(
                      self.tokens[startIdx].startIdx,
                      self.currTok.endIdx,
                      self.srcCode,
                      self.fileName,
                      "Multiple consecutive semicolons on a single line."
                    ),

            if self.currTok.compare(TC_END_OF_FILE):
                # Yell and scream if the current sequence of tokens
                # hasn't yet been recognized and discarded.
                if len(currentSequence) != 0:
                    return None, SyntxError(
                      self.tokens[startIdx].startIdx,
                      self.tokens[self.idx - 1].endIdx,
                      self.srcCode,
                      self.fileName,
                      f"Unrecognizable statement '{self.srcCode[self.tokens[startIdx].startIdx:]}' Reached EOF before the sequence became recognizable."
                    ),
                else:
                    return self, None,

            # Different features will require different numbers of tokens to recognize. Each of
            # these elif clauses will perform tests to identify those sequences. The else clause
            # represents the case in which none of the tests passed meaning the token sequence is
            # either unrecognizable and constitutes a syntax error or it's an expression.
            elif len(currentSequence) == 0:

                pass # Nothing recognizable with 0 information

            elif len(currentSequence) == 1:

                # Since this is the root, we don't want to allow backslashes. They are used to mark the end
                # of a block without adding a newline. Linefills (pipe symbols) may be used for
                # programming style reasons, but we don't want any other tokens on the same line as a
                # linefill.
                if self.currTok.compare(TC_LINEFILL):

                    if len(self.tokens) > self.idx + 1:
                        if not (self.tokens[self.idx + 1].compare(TC_NEWLINE) or
                          self.tokens[self.idx + 1].compare(TC_END_OF_FILE)):

                            return None, SyntxError(
                              self.tokens[startIdx].startIdx,
                              self.tokens[self.idx + 1].endIdx,
                              self.srcCode,
                              self.fileName,
                              "Only newlines, whitespace and comments are allowed after a newline.\n    Moreover, if there is a multiline comment after the linefill, please be\n    sure to add a newline after the comment.\n\n    (Newlines inside the comment are ignored as the comment is not subject to lexical analysis.)"
                            ),

                # Currently the only subtree that could have a lenght of 1 are expressions. We don't
                # want to jump the gun since there could be more tokens before the next statement so
                # we check whether the next token marks the end of the statement.
                elif len(self.tokens) > self.idx + 1:
                    if self.tokens[self.idx + 1].compare(TC_NEWLINE
                      ) or self.tokens[self.idx + 1].compare(TC_END_OF_FILE):

                        aSubTree, err = ParseTree(
                          self.tokens,
                          startIdx,
                          self.srcCode,
                          self.fileName,
                          treeType = TR_EXPR
                        ).parse()

                        if err:
                            return None, err,

                        self.integrate(aSubtree)
                        currentSequence = []
                        startIdx = self.idx
                        continue

            elif len(currentSequence) == 2:

                # Yell and scream if the programmer types a | (linefill) at the end of a line.
                if currentSequence[1].compare(TC_LINEFILL):
                    if self.tokens[self.idx + 1].compare(TC_END_OF_FILE
                      ) or self.tokens[self.idx + 1].compare(TC_NEWLINE):
                        return None, SyntxError(
                          self.tokens[startIdx].startIdx,
                          self.currTok.endIdx,
                          self.srcCode,
                          self.fileName,
                          "Linefills (|) can only appear on blank lines."
                        ),

                if self.compareTknSequence(currentSequence, [
                  TC_IDENTIFIER,
                  TC_EQUAL
                  ]) or self.compareTknSequence(currentSequence, [
                  TC_MEMBER_FIELD_REF,
                  TC_EQUAL
                  ]):

                    aSubTree, err = ParseTree(
                      self.tokens,
                      startIdx,
                      self.srcCode,
                      self.fileName,
                      treeType = TR_ASSIGN_EXPR
                    ).parse()

                    if err:
                        return None, err,

                    self.integrate(aSubtree)
                    currentSequence = []
                    startIdx = self.idx
                    continue

                elif self.compareTknSequence(currentSequence, [
                  TC_IDENTIFIER,
                  TC_DOLLAR_SIGN
                  ]) or self.compareTknSequence(currentSequence, [
                  TC_MEMBER_FIELD_REF,
                  TC_DOLLAR_SIGN
                  ]):

                    aSubTree, err = ParseTree(
                      self.tokens,
                      startIdx,
                      self.srcCode,
                      self.fileName,
                      treeType = TR_VAR_DECLARATION
                    ).parse()

                    if err:
                        return None, err,

                    self.integrate(aSubtree)
                    currentSequence = []
                    startIdx = self.idx
                    continue

                # Variable declarations can also begin with IDENTIFIER, KWD_DATA_TYPE. This check needs to
                # be performed as well.
                elif (
                  currentSequence[0].compare(TC_IDENTIFIER
                  ) and currentSequence[1].compare(TC_KWD
                  ) and currentSequence[1].tokVal in KS_TYPE_KWDS
                  ) or (
                  currentSequence[0].compare(TC_IDENTIFIER
                  ) and currentSequence[1].compare(TC_KWD
                  ) and currentSequence[1].tokVal in KS_TYPE_KWDS
                  ):

                    aSubTree, err = ParseTree(
                      self.tokens,
                      startIdx,
                      self.srcCode,
                      self.fileName,
                      treeType = TR_VAR_DECLARATION
                    ).parse()

                    if err:
                        return None, err,

                    self.integrate(aSubtree)
                    currentSequence = []
                    startIdx = self.idx
                    continue

                # If nothing else has been recognized and the end of the statement is on its way,
                # try to parse the current sequence as an expression.
                elif len(self.tokens) > self.idx + 1:
                    if self.tokens[self.idx + 1].compare(TC_NEWLINE
                      ) or self.tokens[self.idx + 1].compare(TC_END_OF_FILE):

                        aSubTree, err = ParseTree(
                          self.tokens,
                          startIdx,
                          self.srcCode,
                          self.fileName,
                          treeType = TR_EXPR
                        ).parse()

                        if err:
                            return None, err,

                        self.integrate(aSubtree)
                        currentSequence = []
                        startIdx = self.idx
                        continue

            else:
                if len(self.tokens) > self.idx + 1:
                    if self.tokens[self.idx + 1].compare(TC_NEWLINE
                      ) or self.tokens[self.idx + 1].compare(TC_END_OF_FILE):

                        aSubTree, err = ParseTree(
                          self.tokens,
                          startIdx,
                          self.srcCode,
                          self.fileName,
                          treeType = TR_EXPR
                        ).parse()

                        if err:
                            return None, err,

                        self.integrate(aSubtree)
                        currentSequence = []
                        startIdx = self.idx
                        continue

            self.toNext()

##########################################################################################

##########################################################################################
#                             ALL POSSIBLE CHILDREN OF ROOT

    def parse_var_declaration (self):
        pass

    def parse_assign_expr (self):
        pass

    def parse_expr (self):

        # The caller places us at the beginning of the tokens in a single line of code.
        # The end of the line marks the end of the expression so all the tokens need to
        # be collected before begining to parse the expression.
        exprSequence = []
        # We want to ignore newlines when inside pairs of parentheses so we need a nesting
        # depth variable.
        nestingDepth = 0
        # A boolean flag used to handle case of '(' but no ')'.
        sawFirstLParen = False
        firstLParenIdx = 0
        self.startIdx = self.idx

        while self.idx >= 0:

            if self.currTok.compare(TC_NEWLINE
              ) or self.currTok.compare(TC_END_OF_FILE
              ) and nestingDepth == 0:
                break

            elif self.currTok.compare(TC_NEWLINE
              ) or self.currTok.compare(TC_END_OF_FILE
              ) and nestingDepth != 0:
                return None, SyntxError(
                  self.tokens[startIdx].startIdx,
                  self.tokens[firstParenIdx].endIdx,
                  self.srcCode,
                  self.fileName,
                  "Found '(' but no ')'."
                ),

            elif nestingDepth < 0:

                # We don't want to mark a span from the beginning of the expression to the end of the file.
                endIdx = self.idx if not sawFirstLParen else firstLParenIdx

                return None, SyntxError(
                  self.tokens[startIdx].startIdx,
                  self.tokens[endIdx].endIdx,
                  self.srcCode,
                  self.fileName,
                  "Found ')' but no '(' before it."
                ),

            elif self.currTok.compare(TC_LPAREN):
                nestingDepth += 1
                # We copy the Token instance here rather than simply referencing it because the copy will
                # be deleted when it's consumed.
                exprSequence.append(self.currTok.copy())
                if not sawFirstLParen:
                    sawFirstLParen = True
                    firstLParenIdx = self.idx

            elif self.currTok.compare(TC_RPAREN):
                nestingDepth -= 1
                exprSequence.append(self.currTok)

            else:

                exprSequence.append(self.currTok.copy())

            self.toNext()

        # Recursively parse the expression.
        prescidenceMap = {
          0: (TT_DOUBLE_EQUAL, TT_LESS_THAN, TT_GREATER_THAN, TT_LESS_THAN_EQUAL, TT_GREATER_THAN_EQUAL, TT_NOT_EQU, ),
          1: (TT_ANDPERSAND, TT_PIPE_SYM, TT_CARROT, ),
          2: (TT_PLUS, TT_MINUS, ),
          3: (TT_ASTERISK, TT_FWD_SLASH, TT_PERCENT, TT_LEFT_SHIFT, TT_RIGHT_SHIFT, ),
          4: (TT_DBL_ASTERISK, ),
          5: (TT_TILDE, TT_MINUS, TT_PLUS),
          6: (TT_IDENTIFIER, TT_MEMBER_FIELD_REF, TT_INTEGER, TT_FLOAT, TT_CHAR, TT_STRING, ),
        }
        # Minus and plus are both unary operators and binary operators depending on the context in
        # which they appear. A check for the prescidence level being evaluated and the presence of
        # a token that can be a left hand operand on the left will need to be performed while
        # parsing into RPN. This tuple contains all tokens which signify a valid left hand operand.
        validLeftOperands = (TT_IDENTIFIER, TT_MEMBER_FIELD_REF, TT_INTEGER, TT_FLOAT, TT_CHAR, TT_STRING, )
        # For syntax checking purposes, valid right hand operands must also be included, and must include
        # the operators of unary expressions.
        validRightOperands = (TT_IDENTIFIER, TT_MEMBER_FIELD_REF, TT_INTEGER, TT_FLOAT, TT_CHAR, TT_STRING, TT_TILDE, TT_PLUS, TT_MINUS, )
        # The current token prescidence being searched for. Larger numbers indicate higher prescidence. 6
        # is the prescidence level of the individual atoms (Numbers, variables, etc.)
        prescidenceLvl = 6
        while len(exprSequence != 0):
            
            nestingDepth = 0
            sawSubExpr = False
            collectedSubExpr = False
            subExprStartEndIdcs = []
            
            numCurrPrescLvlTkns = 0
            currPrescLvlTknIdces = []
            subExpr = []

            for idx, tok in enumerate(exprSequence):

                if (tok.tokType == TT_LPAREN or sawSubExpr) and not collectedSubExpr:
                    
                    # Grab a sub expression and parse it by instantiating parseTree type TR_EXPR recursively
                    # after the inner for loop.
                    if tok.tokType == TT_LPAREN and not sawSubExpr:
                        sawSubExpr = True
                        nestingDepth += 1
                        subExprStartEndIdcs.append(idx)
                    
                    
                    
                    elif tok.tokType == TT_LPAREN and sawSubExpr:
                        subExpr.append(tok)
                        nestingDepth += 1
                    
                    elif tok.tokType == TT_RPAREN and sawSubExpr:
                        nestingDepth -= 1
                        if nestingDepth != 0:
                            subExpr.append(tok)
                        else:
                            collectedSubExpr = True
                            subExprStartEndIdcs.append(idx + 1)
                            break
                    
                    else:
                        subExpr.append(tok)
                
                elif not collectedSubExpr:
                    
                    # Handle unary operators and possible unary operators separately.
                    if tok.tokType in (TT_TILDE, TT_PLUS, TT_MINUS):
                        
                        pass

##########################################################################################

##########################################################################################
#                        ALL POSSIBLE CHILDREN OF VAR_DECLARATION

    # PARSE_EXPR ALREADY DEFINED

##########################################################################################

##########################################################################################
#                          ALL POSSIBLE CHILDREN OF ASSIGN_EXPR

##########################################################################################

##########################################################################################
#                              ALL POSSIBLE CHILDREN OF EXPR

def parse (tokens, srcCode, fileName):

    aParseTree = ParseTree(tokens, 0, srcCode = srcCode, fileName = fileName)
    parseResult = aParseTree.parse()
    # Return the tuple (ParseTree, syntxError or None) for the error to be handled by the caller.
    return parseResult
