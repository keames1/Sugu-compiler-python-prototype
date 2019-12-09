import sugu_interpreter as sui

VERSION_NUMBER = "0.0.01"
DEV_STATUS = "Experimental"
RELEASE_DATE = "non-applicable"

def main ():
    lex = sui.Lexer("", "<Shell>")
    print(f"Sugu Interpreter Shell version {VERSION_NUMBER} ({DEV_STATUS})")
    print(f"Release date: {RELEASE_DATE}\n\n")
    
    while True:
        lex.shellReset(input(":::: "))
        tokens, err = lex.genTokens()
        if err:
            print(str(err))
        else:
            print(tokens)

if __name__ == "__main__": main()
