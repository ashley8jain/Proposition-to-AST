datatype Prop = TOP                   | BOTTOM
                     | ATOM of string     | NOT of Prop
                     | AND of Prop * Prop | OR of Prop * Prop 
                     | IMP of Prop * Prop | IFF of Prop * Prop
                     | ITE of Prop * Prop * Prop

signature AST = 

sig
    val toPrefix  : Prop -> string
    val toPostfix : Prop -> string
    val isEqual   : Prop -> Prop -> bool
end;

structure AS:AST = 
struct
    fun toPrefix (TOP)  = "TRUE" |
        toPrefix (BOTTOM) = "FALSE" |
        toPrefix(NOT(P)) = "NOT(" ^ toPrefix(P)^")" |
        toPrefix(ATOM(b)) = b |
        toPrefix(AND(P1,P2)) = "AND(" ^ toPrefix(P1) ^ "," ^ toPrefix(P2)^")" |
        toPrefix(IMP(P1,P2)) = "IFTHEN(" ^ toPrefix(P1) ^ "," ^ toPrefix(P2)^")" |
        toPrefix(OR(P1,P2)) = "OR(" ^ toPrefix(P1) ^ "," ^ toPrefix(P2)^")" |
        toPrefix(IFF(P1,P2)) = "IFF(" ^ toPrefix(P1) ^ "," ^ toPrefix(P2)^")" |
        toPrefix(ITE(P1,P2,P3)) = "IFTHENELSE(" ^ toPrefix(P1) ^ "," ^ toPrefix(P2) ^ "," ^ toPrefix(P3)^")";


    fun toPostfix (TOP)  = "TRUE" |
        toPostfix (BOTTOM) = "FALSE" |
        toPostfix(NOT(P)) = "("^toPostfix(P) ^ ")NOT"  |
        toPostfix(ATOM(b)) = b |
        toPostfix(AND(P1,P2)) =  "("^toPostfix(P1) ^ " " ^ toPostfix(P2) ^  ")AND"|
        toPostfix(IMP(P1,P2)) =  "("^toPostfix(P1) ^ " " ^ toPostfix(P2) ^ ")IFTHEN" |
        toPostfix(OR(P1,P2)) =  "("^toPostfix(P1) ^ " " ^ toPostfix(P2) ^ ")OR"|
        toPostfix(IFF(P1,P2)) = "("^toPostfix(P1) ^ " " ^ toPostfix(P2) ^ ")IFF" |
        toPostfix(ITE(P1,P2,P3)) =  "("^toPostfix(P1) ^ " " ^ toPostfix(P2) ^ " " ^ toPostfix(P3) ^ ")IFTHENELSE";

    fun isEqual P1 P2 = false

end;

structure Prop : sig
	           val parse : string*string -> unit
                 end = 
struct

  structure PropLrVals =
    PropLrValsFun(structure Token = LrParser.Token)

  structure PropLex =
    PropLexFunc(structure Tokens = PropLrVals.Tokens)

  structure PropParser =
    Join(structure LrParser = LrParser
	 structure ParserData = PropLrVals.ParserData
	 structure Lex = PropLex)

  fun invoke lexstream =
      let fun print_error (s,i:int,_) =
	      TextIO.output(TextIO.stdOut,
			    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
       in PropParser.parse(0,lexstream,print_error,())
      end


  fun parse(infile:string,outfile:string) = 
      let val inpStream = TextIO.openIn infile
      	val outStream =  TextIO.openOut outfile
      	val lexer = PropParser.makeLexer (fn _ =>
                                               (case TextIO.inputLine(inpStream)
                                                of SOME s => s
                                                 | _ => ""))
	  val dummyEOF = PropLrVals.Tokens.EOF(0,0)
	  val dummySEMI = PropLrVals.Tokens.SEMI(0,0)
	  fun loop lexer =
	      let val (result,lexer) = invoke lexer
		  val (nextToken,lexer) = PropParser.Stream.get lexer
		  val _ = case result
			    of SOME r =>
				(TextIO.output(outStream,(r) ^ "\n");TextIO.output(outStream,(r) ^ "\n"))
			     | NONE => (TextIO.closeOut outStream)
	       in if PropParser.sameToken(nextToken,dummyEOF) then ()
		  else loop lexer
	      end
       in loop lexer before TextIO.closeIn inpStream
      end

end
