Implement a scanner-parser in SML which takes in a sentence in P0 and converts it to an Abstract Syntax Tree (AST)


examples:
• IF a THEN IF b THEN c ELSE d is parsed to IFTHEN(a,IFTHENELSE(b,c,d))
• IF a THEN IF b THEN IF c THEN d ELSE e is parsed to
IFTHEN(a,IFTHEN(b, IFTHENELSE(c,d,e))
• IF a THEN IF b THEN IF c THEN d ELSE e ELSE f is parsed to
IFTHEN(a,IFTHENELSE(b, IFTHENELSE(c,d,e), f)