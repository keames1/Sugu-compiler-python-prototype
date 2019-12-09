# Sugu-compiler-python-prototype
I've decided to be adventurous and design my own systems language. Maybe I'll make something useful...

The aim is to create a language that makes system level programming accessible. I'll begin translating the code to c as I finalize 
the algorithm. For the time being, I have only the lexical analysis working in the Lexer class and though it works, there are some
changes and additions I'd like to make. For example, currently I'm using /* c-style comments 
/* with support for nested comments in multiline. */*/ I've decided to go with # for single line comments and {*comment*} for 
multiline comments. There is also a great deal to get sorted for language features and syntax. Currently, I'm struggling with
defining expression syntax in a way that obeys operator precidence.
