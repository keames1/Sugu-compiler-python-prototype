
# This main function will eventually perform all the operations necessary to executing to code.
# For now, it just prints the token stream for testing purposes.
def main (argv):

    # Open the file to be executed.
    if len(argv) < 2:
        print("No input file. Compilation/interpretation terminated.")
        exit()

    fileName = argv[1]; srcCode = ""
    try:
        with open(fileName, 'r') as f:
            srcCode = f.read()
    except FileNotFoundError:
        print(f"No such file or directory '{fileName}'.")

    lex = Lexer(srcCode, fileName)

    tokens, err = lex.genTokens()

    if err:
        print(err)
        exit()

    print(tokens)

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
TT_LSQUARE                   = mkConst()
TT_RSQUARE                   = mkConst()
TT_PLUS_EQU                  = mkConst()
TT_MINUS_EQU                 = mkConst()
TT_POW_EQU                   = mkConst()
TT_TIMES_EQU                 = mkConst()
TT_DIV_EQU                   = mkConst()
TT_MOD_EQU                   = mkConst()
TT_XOR_EQU                   = mkConst()
TT_FLOOR_DIV_EQU             = mkConst()
TT_LEFT_SHIFT_EQU            = mkConst()
TT_RIGHT_SHIFT_EQU           = mkConst()
TT_OR_EQU                    = mkConst()
TT_AND_EQU                   = mkConst()

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
TT_LINE_FILL                 = mkConst() # Used to allow comments to be on their own line within blocks.
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
TT_DBL_COLON                 = mkConst()
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
    TT_FLOOR_DIV_EQU             : "FLOOR_DIVIDE_EQU",
    TT_LEFT_SHIFT_EQU            : "LEFT_SHIFT_EQU",
    TT_RIGHT_SHIFT_EQU           : "RIGHT_SHIFT_EQU",
    TT_OR_EQU                    : "OR_EQU",
    TT_AND_EQU                   : "AND_EQU",
    TT_LSQUARE                   : "LSQUARE",
    TT_RSQUARE                   : "RSQUARE",
    TT_DBL_COLON                 : "DBL_COLON",
    TT_LINE_FILL                 : "LINE_FILL",
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
KWD_LIST        = "List"
KWD_SIG         = "sig"
KWD_SIGNATURE   = "signature"

KWD_CHAR        = "char"
KWD_BOOL        = "bool"
KWD_REF         = "ref"

KWD_NAMESPACE   = "namespace"    # Import related keywords
KWD_IMPORT      = "import"

KWD_FUN         = "fun"          # Function related keywords
KWD_D_ERROR     = ".error"       # A decorator used to mark a function as an error handler.
KWD_D_OVERRIDE  = ".override"
KWD_RETURN      = "return"
KWD_IMPL        = "impl"
KWD_NORETURN    = "noreturn"     # equivalent to void in c-based languages.
KWD_MAIN        = "main"         # Used to identify explicit main functions.
KWD_ARGV        = "argv"         # Implicit main doesn't have a function header. The argv keyword is
                                 # how command line args are referenced when a program has an implicit main.

KWD_ENUM        = "enum"
KWD_D_BITFIELD  = ".bitfield"

KWD_STRUCT      = "struct"
KWD_FIELD       = "field"

KWD_CONST       = "const"
KWD_GLOBAL      = "global"

KWD_SWITCH      = "switch"
KWD_CASE        = "case"
KWD_DEFAULT     = "default"

KWD_WHILE       = "while"        # All flow control keywords.
KWD_POST        = "post"
KWD_FOR         = "for"
KWD_IN          = "in"
KWD_TO          = "to"

KWD_ISOLATE     = "isolate"
KWD_USING       = "using"
KWD_ISOLATE     = "isolate"
KWD_USING       = "using"
KWD_WITH        = "with"
KWD_AS          = "as"

KWD_BREAK       = "break"
KWD_CONTINUE    = "continue"
KWD_RETURN      = "return"
KWD_RAISE       = "raise"
KWD_LEAVE       = "leave"

KWD_S_TRUE      = "True"         # Singleton value keywords.
KWD_S_FALSE     = "False"
KWD_S_NONE      = "None"

KWD_NONRUNTIME  = ".nonruntime"  # Directive keywords signifying code to be interpreted during compile time.
KWD_END         = ".end"
KWD_AVAILABLE   = ".available"   # Declares constants to be allowed for manipulation by nonruntime code.
KWD_UNAVAILABLE = ".unavailable" # Declares constants to be unavailable for manipulation by nonruntime code.
KWD_ANY         = "any"          # Keyword used to declare all constants available to a nonruntime
KWD_ALL         = "all"

# A keyword tuple used for determining wheather a certain sequence is a keyword.
KS_ALL_KWDS   = (
  # The keyword operators. These include the if, elif, and else also used in if statements.
  KWD_AND, KWD_OR, KWD_NOT, KWD_IF, KWD_ELIF, KWD_ELSE,

  KWD_FUN, KWD_D_ERROR, KWD_D_OVERRIDE, KWD_RETURN,
  KWD_NORETURN, KWD_MAIN, KWD_ARGV, KWD_IMPL,

  KWD_ENUM, KWD_D_BITFIELD,

  KWD_STRUCT, KWD_FIELD,

  KWD_CONST, KWD_GLOBAL,

  # All flow control statement keywords.
  KWD_SWITCH, KWD_CASE, KWD_DEFAULT, KWD_WHILE,
  KWD_POST, KWD_FOR, KWD_IN, KWD_TO,

  KWD_ISOLATE, KWD_USING, KWD_WITH, KWD_AS,

  KWD_BREAK, KWD_CONTINUE, KWD_RETURN, KWD_RAISE, KWD_LEAVE,

  KWD_I8,  KWD_I16,  KWD_I32,  KWD_I64, KWD_IEXP, KWD_F32,
  KWD_F64, KWD_FEXP, KWD_CHAR, KWD_BOOL, KWD_REF, KWD_STR,
  KWD_STRING, KWD_LIST, KWD_U8, KWD_U16, KWD_U32, KWD_U64,
  KWD_SIGNATURE, KWD_SIG,
  
  KWD_S_TRUE, KWD_S_FALSE, KWD_S_NONE,

  KWD_IMPORT, KWD_NAMESPACE,

  KWD_NONRUNTIME, KWD_END, KWD_AVAILABLE, KWD_UNAVAILABLE,
  KWD_ANY, KWD_ALL,

  KWD_D_ERROR,
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
                        aToken = Token(TT_ASTERISK, self.char, self.idx, self.idx + 1, self.fileName)
                else:
                    aToken = Token(TT_ASTERISK, self.char, self.idx, self.idx + 1, self.fileName)

                tokens.append(aToken)

            elif self.char == '/':
                if len(self.srcCode) > self.idx + 1:
                    if self.srcCode[self.idx + 1] == '/':
                        tokens.append(Token(TT_DBL_FWD_SLASH, '//', self.idx, self.idx + 2, self.fileName))
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
                commentStartIdx = self.idx # Comments are treated as newlines but they are longer than 1.
                self.ignoreComment(multiline = False)
                # Adding a comment line fill. This will allow comments to be on their own line without
                # ending to block and will be ignored by the compiler in all other contexts.
                tokens.append(Token(TT_LINE_FILL, '||', commentStartIdx, self.idx + 1, self.fileName))
                # Adding a newline so the parser doesn't see multiple statements on a single line as
                # this would cause a syntax error.
                tokens.append(Token(TT_NEWLINE, '\n', commentStartIdx, self.idx + 1, self.fileName))

            elif self.char == '{':
                if len(self.srcCode) > self.idx + 1:
                    if self.srcCode[self.idx + 1] == '*':
                        commentStartIdx = self.idx
                        self.ignoreComment(multiline = True)
                        # Adding a comment line fill. This will allow comments to be on their own line without
                        # ending to block and will be ignored by the compiler in all other contexts.
                        tokens.append(Token(TT_LINE_FILL, '||', commentStartIdx, self.idx + 1, self.fileName))

                    else:
                        tokens.append(Token(TT_LCURLY, self.char, self.idx, self.idx + 1, self.fileName))
                else:
                    tokens.append(Token(TT_LCURLY, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '}':
                if self.srcCode[self.idx - 1] == '*' and self.idx > 0:
                    return None, LexError(
                      self.idx - 1,
                      self.idx + 1,
                      self.srcCode,
                      self.fileName,
                      "Found an end multiline comment tag '*}' but no accompanying '{*'."
                    ),
                tokens.append(Token(TT_RCURLY, self.char, self.idx, self.idx + 1, self.fileName))
            
            elif self.char == '[':
                tokens.append(Token(TT_LSQUARE, self.char, self.idx, self.idx + 1, self.fileName))
            
            elif self.char == ']':
                tokens.append(Token(TT_RSQUARE, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '\\':
                tokens.append(Token(TT_BACKSLASH, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == ',':
                tokens.append(Token(TT_COMMA, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == ':':
                if self.idx > 0 and self.srcCode[self.idx - 1] == ':' and tokens[-1].tokType != TT_DBL_COLON:
                    tokens[-1] = Token(TT_DBL_COLON, '::', self.idx, self.idx + 2, self.fileName)
                
                else:
                    tokens.append(Token(TT_COLON, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '$':
                tokens.append(Token(TT_DOLLAR_SIGN, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char in CS_NEWLINE + ';':
                tokens.append(Token(TT_NEWLINE, self.char, self.idx, self.idx + 1, self.fileName))

            elif self.char == '=':
                aToken = None

                # If there is an operator in the previous index, we want to reassign the last token
                # to be an in place assignment operator.
                if self.idx > 0 and self.srcCode[self.idx - 1] == '+':
                    tokens[-1] = Token(TT_PLUS_EQU, '+=', self.idx - 1, self.idx + 1, self.fileName)

                elif self.idx > 0 and self.srcCode[self.idx - 1] == '-':
                    tokens[-1] = Token(TT_MINUS_EQU, '-=', self.idx - 1, self.idx + 1, self.fileName)

                elif self.idx > 0 and self.srcCode[self.idx - 2: self.idx + 1] == '**=':
                    tokens[-1] = Token(TT_POW_EQU, '**=', self.idx - 1, self.idx + 1, self.fileName)

                elif self.idx > 0 and self.srcCode[self.idx - 1] == '*':
                    tokens[-1] = Token(TT_TIMES_EQU, '*=', self.idx - 1, self.idx + 1, self.fileName)

                elif self.idx > 0 and self.srcCode[self.idx - 2: self.idx + 1] == '//=':
                    tokens[-1] = Token(TT_FLOOR_DIV_EQU, '//=', self.idx - 2, self.idx + 1, self.fileName)
                    del tokens[-2]

                elif self.idx > 0 and self.srcCode[self.idx - 1] == '/':
                    tokens[-1] = Token(TT_DIV_EQU, '/=', self.idx - 1, self.idx + 1, self.fileName)

                elif self.idx > 0 and self.srcCode[self.idx - 1] == '%':
                    tokens[-1] = Token(TT_MOD_EQU, '%=', self.idx - 1, self.idx + 1, self.fileName)

                elif self.idx > 0 and self.srcCode[self.idx - 1] == '^':
                    tokens[-1] = Token(TT_XOR_EQU, '^=', self.idx - 1, self.idx + 1, self.fileName)

                elif self.idx > 0 and self.srcCode[self.idx - 1] == '=':
                    tokens[-1] = Token(TT_DOUBLE_EQUAL, '==', self.idx - 1, self.idx + 1, self.fileName)

                elif self.idx > 0 and self.srcCode[self.idx - 2: self.idx + 1] == '<<=':
                    tokens[-1] = Token(TT_LEFT_SHIFT_EQU, '<<=', self.idx - 2, self.idx + 1)

                elif self.idx > 0 and self.srcCode[self.idx - 2: self.idx + 1] == '>>=':
                    tokens[-1] = Token(TT_RIGHT_SHIFT_EQU, '>>=', self.idx - 2, self.idx + 1)

                elif self.idx > 0 and self.srcCode[self.idx - 1] == '|':
                    tokens[-1] = Token(TT_OR_EQU, '|=', self.idx - 1, self.idx + 1, self.fileName)

                elif self.idx > 0 and self.srcCode[self.idx - 1] == '&':
                    tokens[-1] = Token(TT_AND_EQU, '&=', self.idx - 1, self.idx + 1, self.fileName)

                else:
                    aToken = Token(TT_EQUAL, self.char, self.idx, self.idx + 1, self.fileName)

                if aToken: tokens.append(aToken)

            elif self.char == '!':
                if len(self.srcCode) > self.idx + 1:
                    if self.srcCode[self.idx + 1] == '=':
                        tokens.append(Token(TT_NOT_EQU, self.char + '=', self.idx, self.idx + 2, self.fileName))
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
            if strStr[-1] == '\n' and strStr[-2:] != '\\\n' and not isBlockStr:
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
        dotCount = 0 if wordStr != '.' else 1
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

        # Yell and scream at the programmer if they typed a dot at the end. For example, 'invalid.'.
        if wordStr[-1] == '.':
            return None, LexError(
              startPos,
              self.idx + 1,
              self.srcCode,
              self.fileName,
              (
              f"A dot in a member field reference must be proceeded by a valid identifier. '{wordStr}' is invalid.\n" +
              "    Additionally, the dots in member field references may not be proceeded or preceeded by spaces."
              )
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
            
            wordList = wordStr.split('.')
            wordListSansEmptyStrings = []
            for i in wordList:
                if i != "":
                    wordListSansEmptyStrings.append(i)
            
            wordTuple = tuple(wordListSansEmptyStrings)
            return Token(TT_MEMBER_FIELD_REF, wordTuple, startPos, self.idx + 1, self.fileName), None,

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

SYNTAX_ERR_TEMPLATE = """A syntax error occured in {} begininning on line {}, col {} and ending on line {}, col {}.

    {}"""

class SyntxError (GeneralError):

    def __init__ (self, startPos, endPos, srcCode, fileName, message):
        super().__init__ (startPos, endPos, srcCode, fileName, message)
        self.template = SYNTAX_ERR_TEMPLATE


if __name__ == "__main__":
    import sys
    main(sys.argv)
