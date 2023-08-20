TITLE Project 6     (Proj6_prulhiec.asm)



INCLUDE Irvine32.inc


mGetString MACRO inputPrompt, inputString, inputLengthLimit, inputStringLength, outputStringOffset, inputNumber

	push			EAX
	push			ECX
	push			EDX
	mDisplayString	inputPrompt
	push			outputStringOffset
	push			inputNumber
	call			WriteVal
	mov				AL, ':'
	call			WriteChar
	mov				AL, ' '
	call			WriteChar
	mov				EDX, inputString
	mov				ECX, inputLengthLimit
	call			ReadString					
	mov				inputStringLength, EAX
	pop				EDX
	pop				ECX
	pop				EAX

ENDM

mPrintTotal MACRO outputStringOffset, inputTotalString, inputTotal

; --------------------------------------------------------------------------------- 
; Displays total 
; ---------------------------------------------------------------------------------

	call			CrLf
	mDisplayString	inputTotalString

	push			outputStringOffset
	push			inputTotal
	call			WriteVal
	call			CrLf

ENDM


mDisplayString MACRO outputStringOffset
; --------------------------------------------------------------------------------- 
; Displays in the console
; ---------------------------------------------------------------------------------
	
	push			EDX
	mov				EDX, outputStringOffset
	call			WriteString
	pop				EDX

ENDM


INTEGER_COUNT = 10
MAX_INPUT_LENGTH = 15

.data

programTitle		BYTE	"PROGRAMMING ASSIGNMENT 6: String Primitives and Macros ", 13, 10, 0
programByline		BYTE	"Written by: Calder Prulhiere ", 13, 10, 13, 10, 0
extraCredit1		BYTE	"**EC Number each line of user input and display a running subtotal of the user’s valid numbers.",13,10,13,10,0
instructions		BYTE	"Please provide 10 signed decimal integers.  ", 13, 10
					BYTE	"Each number needs to be small enough to fit inside a 32 bit register. After you have finished inputting the raw numbers I will display ", 13, 10
					BYTE	"a list of the integers, their sum, and their average value. ",13, 10, 13, 10, 0
inputTotal			BYTE	"running subtotal: ", 0
inputRequest		BYTE	"Please enter a signed number:", 0
inputErrorMsg		BYTE	"ERROR: You did not enter an signed number or your number was too big.", 13, 10, 0
userInput			BYTE	MAX_INPUT_LENGTH DUP(?)
inputLength			DWORD	0
inputErrorFlag		DWORD	0
inputSign			SDWORD	1
inputArray			SDWORD	INTEGER_COUNT DUP(?)
outputNumbers		BYTE	"You entered the following numbers: ", 13, 10, 0
outputSum			BYTE	"The sum of these numbers is: ", 0
outputAverage		BYTE	"The truncated average iss: ", 0
outputString		BYTE	MAX_INPUT_LENGTH DUP(?)

; Goodbye Identifiers
goodbye				BYTE	"Goodbye", 13, 10, 0

.code
main PROC
; ---------------------------------------------------- 
;	Prints instructions 
; ---------------------------------------------------- 
	mDisplayString	offset programTitle
	mDisplayString	offset programByline
	mDisplayString	offset extraCredit1
	mDisplayString	offset instructions

; ----------------------------------------------------  
;  User input
; ----------------------------------------------------
	mov				ECX, INTEGER_COUNT
	mov				EDI, offset inputArray

_getUserInput:
	
	push			offset inputTotal
	push			offset inputArray
	push			ECX
	push			offset outputString
	push			offset inputErrorMsg
	push			EDI
	push			offset inputSign
	push			offset inputErrorFlag
	push			offset inputLength
	push			offset userInput
	push			offset inputRequest
	call			ReadVal
	add				EDI, type SDWORD				
	loop			_getUserInput

; ---------------------------------------------------- 
; Display output
; ----------------------------------------------------
	push			offset outputString
	push			offset inputArray
	push			offset outputAverage
	push			offset outputSum
	push			offset outputNumbers
	call			printOutput

; ----------------------------------------------------
;; ----------------------------------------------------
	call			CrLf
	call			CrLf
	mDisplayString	offset goodbye

	Invoke ExitProcess,0	; exit to operating system
main ENDP


ReadVal PROC uses EAX EBX ECX EDX ESI EDI
	push			EBP
	mov				EBP, ESP

_getInput:

	;current number
	mov				EAX, INTEGER_COUNT
	sub				EAX, [EBP + 64]
	inc				EAX
	push			EAX
	; sum
	mov				ECX, EAX
	mov				ESI, [EBP + 68]
	mov				EBX, 0								; Store Sum

_sumNextInteger:

	mov				EAX, 0
	cld
	LODSD
	add				EBX, EAX
	loop			_sumNextInteger
	pop				EAX
	; current total + prompt
	mPrintTotal		[EBP + 60], [EBP + 72], EBX
	mGetString		[EBP + 32], [EBP + 36], MAX_INPUT_LENGTH, [EBP + 40], [EBP + 60], EAX
	push			[EBP + 48]							
	push			[EBP + 44]							
	push			[EBP + 40]							
	push			[EBP + 36]							
	call			validateString
	; error
	mov				EAX, [EBP + 44]
	mov				EAX, [EAX]
	cmp				EAX, 0
	jne				_errorMessage

; ----------------------------------------------------  
; Convert from string.
; ----------------------------------------------------

	push			[EBP + 52]							
	push			[EBP + 48]							
	push			[EBP + 44]							
	push			[EBP + 40]							
	push			[EBP + 36]							
	call			stringToSDWORD
	; error check
	mov				EAX, [EBP + 44]
	mov				EAX, [EAX]
	cmp				EAX, 0
	je				_errorMessageEnd

_errorMessage:

	; Display error
	mDisplayString	[EBP + 56]
	mov				EAX, [EBP + 44]
	mov				DWORD ptr [EAX], 0
	jmp				_getInput

_errorMessageEnd:

	pop				EBP
	ret				44
ReadVal ENDP

; --------------------------------------------------------------------------------- 
;  checks each character if valid, flag
; --------------------------------------------------------------------------------- 
validateString PROC uses EAX ECX EDX ESI
	
	push			EBP
	mov				EBP, ESP
	mov				ESI, [EBP + 24]						
	mov				ECX, [EBP + 28]						
	cmp				ECX, 0
	jle				_inputLengthError
	cmp				ECX, 12
	jge				_inputLengthError
	mov				EAX, 0
	cld
	lodsb
	push			[EBP + 36]							
	push			[EBP + 32]							
	push			EAX
	call			validateFirstCharacter
	dec				ECX
	cmp				ECX, 0
	jle				_validateEnd

_nextCharacter:

	mov				EAX, 0
	cld
	lodsb												
	push			[EBP + 32]							
	push			EAX
	call			validateCharacter
	;check, break loop
	mov				EAX, 0
	mov				EDX, [EBP + 32]
	cmp				EAX, [EDX]
	jne				_validateEnd
	loop			_nextCharacter
	jmp				_validateEnd

_inputLengthError:

	; Set Error Flag
	mov				EAX, [EBP + 32]
	mov				DWORD ptr [EAX], 1

_validateEnd:

	pop				EBP
	ret				16
validateString ENDP

; --------------------------------------------------------------------------------- 
; Checks first charcter of string input
; ---------------------------------------------------------------------------------
validateFirstCharacter PROC uses EAX EDX
	
	push			EBP
	mov				EBP, ESP
	mov				EAX, [EBP + 16]
	cmp				EAX, 2Bh		; + sign
	je				_errorFirstCharEnd
	cmp				EAX, 2Dh		; - sign
	je				_minusSign
	cmp				EAX, 30h
	jb				_errorFirstChar
	cmp				EAX, 39h
	ja				_errorFirstChar
	jmp				_errorFirstCharEnd

_minusSign:

	mov				EAX, [EBP + 24]
	mov				EDX, -1
	mov				[EAX], EDX
	jmp				_errorFirstCharEnd

_errorFirstChar:
	
	mov				EAX, [EBP + 20]
	mov				DWORD ptr [EAX], 1

_errorFirstCharEnd:

	pop				EBP
	ret				12
validateFirstCharacter ENDP

; --------------------------------------------------------------------------------- 
; Checks if character input are integers	
; ---------------------------------------------------------------------------------
validateCharacter PROC uses EAX
	
	push			EBP
	mov				EBP, ESP
	mov				EAX, [EBP + 12]
	cmp				EAX, 30h
	jb				_error
	cmp				EAX, 39h
	ja				_error
	jmp				_errorEnd ; check in range

_error:

	mov				EAX, [EBP + 16]
	mov				DWORD ptr [EAX], 1		

_errorEnd:

	pop				EBP
	ret				8
validateCharacter ENDP

; --------------------------------------------------------------------------------- 
;  Converts string to SDWORD
; ---------------------------------------------------------------------------------
stringToSDWORD PROC uses EAX EBX ECX EDX ESI EDI
	
	push			EBP
	mov				EBP, ESP
	mov				ESI, [EBP + 32]						
	mov				EDI, [EBP + 48]						
	mov				ECX, [EBP + 36]	 ; sring length 
	mov				EBX, 0								
	mov				EDX, 1								
	mov				EAX, ECX
	dec				EAX
	add				ESI, EAX

_addInteger:
	
	mov				EAX, 0
	std
	lodsb
	cmp				EAX, 2Bh		
	je				_multiplyBySignFlag
	cmp				EAX, 2Dh		
	je				_multiplyBySignFlag
	sub				EAX, 30
	push			EDX
	imul			EDX									
	add				EBX, EAX
	jo				_overflowError
	pop				EDX
	mov				EAX, EDX
	mov				EDX, 10
	imul			EDX
	mov				EDX, EAX
	loop			_addInteger

_multiplyBySignFlag:

	mov				EAX, EBX
	mov				EBX, [EBP + 44]						
	mov				EBX, [EBX]
	imul			EBX									
	mov				EBX, [EBP + 44]	
	mov				sdword ptr [EBX], 1
	mov				[EDI], EAX
	jmp				_stringToSDWORDEnd

_overflowError:

	pop				EDX
	mov				EAX, [EBP + 40] ;error
	mov				DWORD ptr [EAX], 1

_stringToSDWORDEnd:

	pop				EBP
	ret				20

stringToSDWORD ENDP

; --------------------------------------------------------------------------------- 
; SDWORD to strings .
; ---------------------------------------------------------------------------------
WriteVal PROC uses EAX EBX ECX EDX EDI
	
	push			EBP
	mov				EBP, ESP
	mov				EDX, [EBP + 28]						
	mov				EDI, [EBP + 32]						
	mov				ECX, 10								
	mov				EBX, 1000000000						
	cmp				EDX, 0
	jl				_negativeNumber
	jmp				_getChar

_negativeNumber:
	
	neg				EDX
	mov				EAX, '-'
	STOSB			

_getChar:
	
	mov				EAX, EDX
	cdq
	div				EBX								
	push			EDX
	cmp				EAX, 0
	jne				_saveChar
	cmp				ECX, 1
	je				_saveChar
	push			EAX
	push			EBX
	mov				EAX, [EBP + 32]

_checkNextChar:

	mov				BL, BYTE PTR [EAX]
	cmp				BL, 31h
	jge				_nonLeadingZero
	inc				EAX
	cmp				EDI, EAX
	jle				_leadingZero
	jmp				_checkNextChar

	
_leadingZero:

	pop				EBX
	pop				EAX
	jmp				_saveCharEnd

_nonLeadingZero:

	pop				EBX
	pop				EAX

_saveChar:
	
	add				EAX, 30h
	STOSB

_saveCharEnd:

	mov				EAX, EBX
	cdq
	mov				EBX, 10
	div				EBX
	mov				EBX, EAX
	pop				EDX
	loop			_getChar
	mov				EAX, 0
	STOSB

	mDisplayString	[EBP + 32]

	mov				ECX, MAX_INPUT_LENGTH
	mov				EDI, [EBP + 32]
	mov				EAX, 0
	rep				STOSB
	pop				EBP
	ret				8
WriteVal ENDP

; --------------------------------------------------------------------------------- 
; Prints numbers in array
; ---------------------------------------------------------------------------------
printOutput PROC uses EAX EBX ECX EDX ESI
	
	push			EBP
	mov				EBP, ESP
	mov				ECX, INTEGER_COUNT
	mov				ESI, [EBP + 40]
	call			CrLf
	mDisplayString	[EBP + 28]

_printNumber:

	LODSD
	push			[EBP + 44]							
	push			EAX								
	call			WriteVal
	cmp				ECX, 1
	je				_calculateSum
	mov				AL, ','
	call			WriteChar
	mov				AL, ' '
	call			WriteChar
	loop			_printNumber

_calculateSum:

	mov				ECX, INTEGER_COUNT
	mov				ESI, [EBP + 40]
	mov				EBX, 0			

_sumNext:

	LODSD
	add				EBX, EAX
	loop			_sumNext
	; sum title + sum 
	call			CrLf
	call			CrLf
	mDisplayString	[EBP + 32]
	mov				EAX, EBX
	push			[EBP + 44]							
	push			EAX									
	call			WriteVal

_calculateAverage:

	mov				EBX, INTEGER_COUNT
	cdq
	idiv			EBX									
	call			CrLf
	call			CrLf
	mDisplayString	[EBP + 36]
	push			[EBP + 44]							
	push			EAX									
	call			WriteVal
	pop				EBP
	ret				16

printOutput ENDP

END main
