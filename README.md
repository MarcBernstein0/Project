# Parser, Interpreter and Compiler Project

Made by Marc Bernstein, Benjamin Borden, Pat Duffill 
with help from the Professor and TF of CS320

To run the parser, just type:
parse parser "function to parse goes here, can either be a string, or a list of strings concatanted"
	Note, running the parser will give you the Just(Program,rest)

To run the Interpreter there are two methods:
	1) eval [Whatever program you want to eval is but make sure it is of type Program and not a list]
		This will give you a list of the print statements as a return type
	2) run [Whatever program you want to eval is but make sure it is of type Program and not a list]
		This returns an IO(), but will print out the prints from the code in a pretty way

To run the Compiler, again, there are two methods:
	1) compile [Whatever program you want to eval is but make sure it is of type Program and not a list]
		This will return to you a type IC_Program, but will not execute it
	2) executeProgram [Whatever program you want to eval is but make sure it is of type Program and not a list]
		This will create the IC_Program from whatever program you give it, then it will run that program through execute in IC_Interpreter and give you the result.


Note: ICInterpreter.hs, IcInterpreterExamples.hs, ParserMonad.hs and StatefulUnsafeMonad.hs was written by the Professor and TF respectively. Also, all the files that I made are in the src directory.
