;-------------------------------------------------------------------------------
;                            Battleship Game 
;                       Carlos A.Rodriguez Santiago
;                           Profesor: Nicolas Cobo 
;                               INEL 4206
;-------------------------------------------------------------------------------
#include     <msp430.h>
A    EQU    0x0401
B    EQU    0x0402
c    EQU    0x0403 //Min
D    EQU    0x0404
E    EQU    0x0405
F    EQU    0x0406
G    EQU    0x0407
H    EQU    0x0408
I    EQU    0x0409
J    EQU    0x040A
K    EQU    0x040B
L    EQU    0x040C
M    EQU    0x040D
n    EQU    0x040E //Min
O    EQU    0x040F
P    EQU    0x0500
Q    EQU    0x0501
R    EQU    0x0502
S    EQU    0x0503
T    EQU    0x0504
U    EQU    0x0505
v    EQU    0x0506 //Min
W    EQU    0x0507
X    EQU    0x0508
Y    EQU    0x0509
z    EQU    0x050A //Min
ONE     EQU     0x0301
TWO     EQU     0x0302
THREE   EQU     0x0303
FOUR    EQU     0x0304

Letters   Macro X
        mov     #X,R13                  ;Move argument into R13
        swpb    R13                     ;Swpb to get the most significant first 
        mov.b   R13,R5            	;Move Letter MSB to R5 to put it into P1OUT    
        call    #Writing        	;Call Write Subroutine
        swpb    R13                     ;Swpb again to get the LSB 
        mov.b   R13,R5            	;Move the LSB to R5 to put it into P1OUT
        call    #Writing        	;Call Write subroutine
        clr.w   R5            		;Clear Register for next Write
        call    #Delay2                 ;call DELAY
        ENDM            		;end definition 
Counter   Macro	A,B
      	mov.b	#A,R12			;Move byte first value to R12
	call	#WritingShip		;Call WritingShip subroutine
	mov	#B,R12			;Move second value to R12
	call	#WritingShip2		;Call WritingShip2 subroutine
	call	#Delay2			;Delay to present the digit more time in the display
	call	#Delay2			;Wait
	call	#Delay2			;Wait
	call	#Delay2			;Wait
	call	#Buttons		;Call the subroutine for the Buttons to be ready for an Interruption
	ENDM

;			Variables Defense 
	   ORG	0200h
ReceiveL   DS8	1
	   ORG	0204h
ReceiveN   DS8	1
	   ORG	020Ah
GameOverFinal      DS8  1
;                       Barco 1
            ORG    0220h                ;Direccion en RAM   
Letra1B1    DS8    1
            ORG    0224h
Num1B1      DS8    1
            ORG    0228h
Letra2B1    DS8    1
            ORG    022Eh
Num2B1      DS8    1

;                       Barco 2
            ORG    0230h                ;Direccion en RAM   
Letra1B2    DS8    1
            ORG    0234h
Num1B2      DS8    1
            ORG    0238h
Letra2B2    DS8    1
            ORG    023Eh
Num2B2      DS8    1
;                       Barco 3
            ORG    0240h                  ;Direccion en RAM   
Letra1B3    DS8    1
            ORG    0244h
Num1B3      DS8    1
            ORG    0248h
Letra2B3    DS8    1
            ORG    024Eh
Num2B3      DS8    1       
       
;------------------------------------------------------------
    ORG    0C000h    ;Program Start
;------------------------------------------------------------
RESET      mov    #0400h,SP               ;Initialize stackpointer
StopWDT    mov    #WDTPW+WDTHOLD,&WDTCTL  ; Stop WDT
SetupP1    bis.b  #00111111b,&P1DIR       ; P1.0 output   

Start
;         Inicializar memorias en RAM 
           mov    #0,&Letra1B1
           mov    #0,&Num1B1
           mov    #0,&Letra2B1
           mov    #0,&Num2B1
      
           mov    #0,&Letra1B2
           mov    #0,&Num1B2
           mov    #0,&Letra2B2
           mov    #0,&Num2B2
      
           mov    #0,&Letra1B3
           mov    #0,&Num1B3
           mov    #0,&Letra2B3
           mov    #0,&Num2B3
           
           mov	  #0,&GameOverFinal    
	   
	   mov	  #0,&ReceiveL
	   mov	  #0,&ReceiveN
      
           call    #ClearPorts             ;Clear ports 1 & 2
           call    #Delay                  ;Delay 1
           call    #InitLCD                ;Init LCD for 4 bits
           call    #Buttons                ;Init Buttons P1.6 &P1.7
           call    #Welcome                ;Welcome Message
           call    #SelShip1              

Here       jmp     Here
;------------------------------------------------------------------------------
;                Initialize LCD to 4 bits
;------------------------------------------------------------------------------
InitLCD	mov.b   #00000000b,&P1OUT
    	call    #Delay            	;Wait 150 ms after power is applied
   
    	mov.b   #0x3,&P1OUT        	;0x30 on the output port
     	call    #Delay            	;Wait 150 ms, busy flag not available
    	call    #Nybble            	;Command 0x30 = Wake Up
    	call    #Delay            	;Wait 150 ms, busy flag not available        
    	call    #Nybble           	;Command 0x30 = Wake up #2
    	call    #Delay            	;Wait 150 ms, busy flag not available
    	call    #Nybble          	;Command 0x30 = Wake up #3
    	call    #Delay            	;Can check busy flag now instead of delay
   
    	mov.b   #0x2,&P1OUT        	;Put 0x20 on the output port
    	call    #Nybble            	;Function set: 4 bit interface
   
    	mov.w   #0x2,R4            	;Function set: 4bit/2-line
    	call    #Command        	;Command 0x28 to set the 4 bit 2 lines
    	mov.w   #0x8,R4            	;Function set: 4bit/2-line
    	call    #Command        	;Command 0x28 to set the 4 bit 2 lines
    	clr.w    R4            		;Clear Register for next Command
   
;            Set cursor
    	mov.w   #0x1,R4            	;Set cursor = 0x10
    	call    #Command        	;Call command subroutine
    	mov.w   #0x0,R4            	;Set Cursor = 0x10
    	call    #Command        	;Call command subroutine   
    	clr.w    R4            		;Clear Register for next command
   
;            Display Cursor
    	mov.w   #0x0,R4            	;Display cursor(blinking) = 0x0F
    	call    #Command        	;Call command subroutine
        mov.w   #0xF,R4            	;Display cursor(blinking) = 0x0F
    	call    #Command        	;Call command subroutine
    	clr.w    R4            		;Clear Register for next command
;            Entry Mode
   
   	mov.w   #0x0,R4            	;Entry mode set = 0x06
    	call    #Command        	;Call command subroutine
    	mov.w   #0x6,R4            	;Entry mode set = 0x06
    	call    #Command        	;Call command subroutine
    	clr.w    R4            		;Clear Register for next command        
;            Return Home
    	mov.w   #0x0,R4            	;Return Home set = 0x02
    	call    #Command        	;Call command subroutine
    	mov.w   #0x2,R4            	;Return Home set = 0x02
    	call    #Command        	;Call command subroutine
    	clr.w    R4            		;Clear Register for next command   
        ret
;-------------------------------------------------------------------------------
;            WELCOME TO BATTLESHIP MSG
;-------------------------------------------------------------------------------
Welcome:
;            Set DDRAM
        call    #ClearPorts
        call    #Delay2
        call    #Clear
        call    #Delay2
	mov.w    #0x8,R4            	;Set DDRAm position 05 = 0x83
	call    #Command        	;Call command subroutine
	mov.w    #0x3,R4            	;Set DDRAm position 05 = 0x83
	call    #Command        	;Call command subroutine
	clr.w    R4   
;       Welcome to Battleship      
        Letters  W
        Letters  E
        Letters  L
        Letters  c
        Letters  O
        Letters  M
        Letters  E
        call    #Space                       
        Letters T
        Letters O
        call    #Clear
;            Set DDRAM
        call    #ClearPorts
        call    #Delay2
        call    #Clear
        call    #Delay2
    	mov.w   #0x8,R4            	;Set DDRAm position 05 = 0x83
    	call    #Command        	;Call command subroutine
    	mov.w   #0x3,R4            	;Set DDRAm position 05 = 0x83
    	call    #Command        	;Call command subroutine
    	clr.w    R4
       
        Letters B
        Letters A
        Letters T
        Letters T
        Letters L
        Letters E
        Letters S
        Letters H
        Letters I
        Letters P       
        ret

;-------------------------------------------------------------------------------
;           Place  ship MSG
;------------------------------------------------------------------------------          
Place   Letters P
        Letters L
        Letters A
        Letters c
        Letters E
        call    #Space
        Letters S
        Letters H
        Letters I
        Letters P
        call    #Space
        ret
;-------------------------------------------------------------------------------
;                Select Ship 1
;Clears Registers to be used, call Place MSG for Ship 1 and then the Counter 
;Selection to start the counter for Letters. 
;-------------------------------------------------------------------------------
SelShip1
      call    #Clear
      call    #ClearPorts
    
       mov    #0,  R6            ;Letters coordinate Ship 1
       mov    #0,  R7            ;Numbers coordinate Ship1
       mov    #0,  R8            ;Letters coordinate Ship 3
       mov    #0,  R9            ;Numbers coordinate Ship1
       mov    #0,  R10           ;Letters coordinate Ship 3
       mov    #0,  R11           ;Numbers coordinate Ship 3
      
       call   #Place
;            Number 1
   
       Letters  ONE
            
       jmp      CounterSelection
;-------------------------------------------------------------------------------
;                 Select Ship 2
;Clears Registers to be used, call Place MSG for Ship 2 and then the Counter 
;Selection to start the counter for Letters. 
;-------------------------------------------------------------------------------
SelShip2
      call    #Clear
      call    #ClearPorts
     
       mov    #0,  R8            ;Letters coordinate Ship 3
       mov    #0,  R9            ;Numbers coordinate Ship1
       mov    #0,  R10           ;Letters coordinate Ship 3
       mov    #0,  R11           ;Numbers coordinate Ship 3
      
      call   #Place
;            Number 2
   
      Letters  TWO
            
      jmp      CounterSelection
;-------------------------------------------------------------------------------
;                 Select Ship 3
;Clears Registers to be used, call Place MSG for Ship 3 and then the Counter 
;Selection to start the counter for Letters.
;-------------------------------------------------------------------------------
SelShip3
      call    #Clear
      call    #ClearPorts    
      
      mov    #0,  R10           ;Letters coordinate Ship 3
      mov    #0,  R11           ;Numbers coordinate Ship 3  
      
      call   #Place
;            Number 3
   
      Letters  THREE           
      jmp      CounterSelection 
;-------------------------------------------------------------------------------
;           Made Ship Macro
;To made each ship compare each letter from A to D  with R12 to found the 
;user selection and store it at Register selected.
;------------------------------------------------------------------------------         
Madeship  Macro Register, Number, Destination
          mov   #Number,Register        ;Mov the Hex ASCII value to the Register
          cmp   Register,R12            ;Cmp the Register value with the one selected by the USER
          jeq   Destination             ;JEQ to the destination selected 
          ENDM  
;-------------------------------------------------------------------------------
;           Made Ship 1 
;------------------------------------------------------------------------------
MadeShip1
	Madeship  R6,41h,CounterSelection2  ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal

        Madeship  R6,42h,CounterSelection2  ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal

        Madeship  R6,43h,CounterSelection2  ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal

        Madeship  R6,44h,CounterSelection2  ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal     
;-------------------------------------------------------------------------------
;           Made Ship 1 Number
;------------------------------------------------------------------------------
MadeShip1N
       Madeship  R7,31h,SelShip2    ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	
       Madeship  R7,32h,SelShip2    ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	
       Madeship  R7,33h,SelShip2    ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	
       Madeship  R7,34h,SelShip2    ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
;-------------------------------------------------------------------------------
;           Made Ship 2
;------------------------------------------------------------------------------
MadeShip2
  	Madeship  R8,41h,CounterSelection2  ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	
	Madeship  R8,42h,CounterSelection2  ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	
	Madeship  R8,43h,CounterSelection2  ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	
	Madeship  R8,44h,CounterSelection2  ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
;-------------------------------------------------------------------------------
;           Made Ship 2 Number
;------------------------------------------------------------------------------
MadeShip2N
        Madeship  R9,31h,SelShip3           ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal      
	
	Madeship  R9,32h,SelShip3           ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	
	Madeship  R9,33h,SelShip3           ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	
	Madeship  R9,34h,SelShip3           ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
;-------------------------------------------------------------------------------
;           Made Ship 3
;------------------------------------------------------------------------------
MadeShip3
   
        Madeship  R10,41h,CounterSelection2 ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	
	Madeship  R10,42h,CounterSelection2 ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	
	Madeship  R10,43h,CounterSelection2 ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	
	Madeship  R10,44h,CounterSelection2 ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	    
;-------------------------------------------------------------------------------
;           Made Ship 3 Number
;------------------------------------------------------------------------------
MadeShip3N
        Madeship  R11,31h,MoveLocations     ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	
	Madeship  R11,32h,MoveLocations     ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	
	Madeship  R11,33h,MoveLocations     ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
	  
	Madeship  R11,34h,MoveLocations     ;Madeship MACRO Register to sotre, Number to compare, Destination  to jump if equal
;-------------------------------------------------------------------------------
;                Counter Selection
;Counter to Select Letters from A to D for the locations
;-------------------------------------------------------------------------------
CounterSelection:       
       	call    #Delay2              
LOOP ;        Letter A
    	call    #Position             ;Position Subroutine         		
	Counter	4h,41h                ;Counter Macro first Send an A to the Display
    	bit.b   #01000000b,&P1IN      ;Bit test for an interruption
    	jz      Int                   ;If zero jmp to eint
    	clr.w    R12                  ;Else clear the Register for the other Letter 		
	
;            Letter B      
    	call    #Position             ;Position Subroutine     	
	Counter	4h,42h                ;Counter Macro first Send an B to the Display
    	bit.b   #01000000b,&P1IN      ;Bit test for an interruption
    	jz      Int                   ;If zero jmp to eint
    	clr.w    R12           	      ;Else clear the Register for the other Letter
	
;            Letter C      
        call    #Position             ;Position Subroutine          	
        Counter 4h,43h                ;Counter Macro first Send an C to the Display
    	bit.b 	#01000000b,&P1IN      ;Bit test for an interruption
    	jz    	Int                   ;If zero jmp to eint
    	clr.w   R12                   ;Else clear the Register for the other Letter     	 
	
;            Letter D      
        call    #Position             ;Position Subroutine          	
        Counter	4h,44h                ;Counter Macro first Send an D to the Display
    	bit.b  #01000000b,&P1IN       ;Bit test for an interruption
    	jz     Int                    ;If zero jmp to eint
    	clr.w  R12                    ;Else clear the Register for the other Letter    	
       
    	call  #Buttons                ;Buttons Subroutine
    	bit.b #01000000b,&P1IN        ;Bit test for an interruption
    	jnz    CounterSelection       ;If no zero Keep the Loop for CounterSelection    
Int     eint                          ;Interruption
HERE2   jmp    HERE2                  ;Wait
;-------------------------------------------------------------------------------
;                Counter Selection for Numbers
;Counter to Select Numbers from 1 to 2 for the locations
;-------------------------------------------------------------------------------
CounterSelection2:       
       	call    #Delay2              
;            Number 1
       	call    #Position2           ;Position Subroutine       		
        Counter	3h,31h               ;Counter Macro Send a 1 to the Display
    	bit.b 	#01000000b,&P1IN     ;Bit test for an interruption
    	jz    	Int2                 ;If zero jmp to eint
    	clr   	 R12                 ;Else clear the Register for the other Letter       			
	
;            Number 2     
    	call    #Position2           ;Position Subroutine      	
        Counter 3h,32h               ;Counter Macro send a 2 to the Display
    	bit.b 	#01000000b,&P1IN     ;Bit test for an interruption
    	jz    Int2                   ;If zero jmp to eint
    	clr    R12            	     ;Else clear the Register for the other Letter   		      
;            Number 3      
    	call    #Position2           ;Position Subroutine    	
       	Counter	3h,33h               ;Counter Macro send a 3 to the Display
   	bit.b   #01000000b,&P1IN     ;Bit test for an interruption
    	jz      Int2                 ;If zero jmp to eint
    	clr     R12                  ;Else clear the Register for the other Letter 
	
;            Number 4       
    	call    #Position2           ;Position Subroutine    	
       	Counter	3h,34h               ;Counter Macro send a 4 to the Display
   	bit.b   #01000000b,&P1IN     ;Bit test for an interruption
    	jz      Int2                 ;If zero jmp to eint
    	clr     R12                  ;Else clear the Register for the other Letter 
      
    	call    #Buttons             ;Buttons Subroutine
   	bit.b   #01000000b,&P1IN     ;Bit test for an interruption
    	jnz     CounterSelection2    ;If no zero Keep the Loop for CounterSelection       
Int2    eint                         ;Interruption
HERE3   jmp    HERE3                 ;Wait

;-------------------------------------------------------------------------------
;                  P 1.6 Interrupt Servic Routine
;                  Made Ships and Player Selection
;------------------------------------------------------------------------------
P6
      bic.b #01000000b,&P1IFG        ;Clear the Interrupt Flag for P1.6
     
      cmp   #0,R6                    ;If R6 is zero Made first ship(Letter)
      jz   MadeShip1
      cmp   #0,R7                    ;If R7 is zero Made first ship(Number)
      jz   MadeShip1N
         
      cmp #0,R8                      ;If R8 is zero Made second ship(Letter)
      jz  MadeShip2 
      cmp #0,R9                      ;If R9 is zero Made second ship(Number)
      jz MadeShip2N
     
      cmp #0,R10                     ;If R10 is zero Made third ship(Letter)
      jz  MadeShip3
      cmp #0,R11                     ;If R11 is zero Made third ship(Number)
      jz MadeShip3N
      
      cmp  #0x31,R12                 ;If the User select Player 1 jump to Attack Mode
      jeq   Attack
   
      cmp  #0x32,R12                 ;If the User select Player 1 jump to Attack Mode
      jeq  D1                        ;If the User select Player 2 jump to Defense Mode
      jmp Attack
D1    call #Defense     
    
     
;-------------------------------------------------------------------------------
;           P 1.7 Interrupt Servic Routine
;           
;------------------------------------------------------------------------------
P7
    bic.b #10000000b,&P1IFG         ;Clear the Interrupt Flag for P1.7
    
    
    cmp  #0,R8                      ;If No zero the User already select a Letter for Attack
    jeq   SendShipL
   
    cmp  #0,R9                     ;If No zero the User already select a Number for Attack
    jeq  SendShipN
;------------------------------------------------------------------------------
;           Interrupt Servic Routine
;------------------------------------------------------------------------------  
PBISR   call    #Delay2
       
          bit.b #10000000b,&P1IN  ;Check if the interrupt was the port 1.7
          jz    P7
           
          bit.b #01000000b,&P1IN  ;Check if the interrupt was the port 1.6
          jz    P6
         
          reti  
;-------------------------------------------------------------------------------
;           Move Locations
;Move each location selected to a specfic RAM location for each letter and 
;number of the ships
;-------------------------------------------------------------------------------
MoveLocations:

       cmp    #0,Letra1B1         
       jeq    Coord               ;If Zero enter the first coordinates of each ship 
       jmp    Coord2              ;If not zero enter the second coordinates of each ship
;        Coordenada 1 Barco 1
Coord  mov    R6,&Letra1B1        ;Move the value selected to the position in RAM
       mov    R7,&Num1B1          ;Move the value selected to the position in RAM
   
;        Coordenada 1 Barco 2
       mov    R8,&Letra1B2        ;Move the value selected to the position in RAM
       mov    R9,&Num1B2          ;Move the value selected to the position in RAM

;        Coordenada 1 Barco 3
       mov    R10,&Letra1B3       ;Move the value selected to the position in RAM
       mov    R11,&Num1B3         ;Move the value selected to the position in RAM
   
       jmp    SelShip1            ;After enter the first coordinates jump to select the second coordinates of each ship
   
Coord2
;        Coordenada 2 Barco 1
        mov    R6,&Letra2B1       ;Move the value selected to the position in RAM
        mov    R7,&Num2B1         ;Move the value selected to the position in RAM
   
;        Coordenada 2 Barco 2
        mov    R8,&Letra2B2       ;Move the value selected to the position in RAM
        mov    R9,&Num2B2         ;Move the value selected to the position in RAM

;        Coordenada 2 Barco 3
        mov    R10,&Letra2B3      ;Move the value selected to the position in RAM
        mov    R11,&Num2B3        ;Move the value selected to the position in RAM
   
        jmp    Player             ;Jump to select between Player 1 or 2 
;-------------------------------------------------------------------------------
;           Select Player1(Attack) or Player 2(Defense)
;-------------------------------------------------------------------------------
Player:
    call    #Delay2
    call    #Clear
    call    #ClearPorts   
    Letters    P
    Letters    L
    Letters    A
    Letters    Y
    Letters    E
    Letters    R
   
    call #Space
LOOPP
   
;            Number 1
    call    #Position2         		
    Counter 3h,31h              ;Move number 1 to Display
    bit.b   #01000000b,&P1IN    ;Test if the user select PLayer 1
    jz      IntP                ;If the user select Player 1 jump to the interruption
    clr     R12        		;Else clean the Register for the Number 2	
    
;            Number 2     
    call    #Position2               	
    Counter 3h,32h              ;Move number 1 to Display
    bit.b   #01000000b,&P1IN    ;Test if the user select PLayer 1 
    jz      IntP                ;If the user select Player 1 jump to the interruption
    clr     R12            	;Else clean the Register for the Number 2		
   
    call    #Buttons
    bit.b   #01000000b,&P1IN
    jnz    LOOPP       
IntP    eint 
HEREP   jmp    HEREP
;-------------------------------------------------------------------------------
;           Attack
;------------------------------------------------------------------------------ 
Attack:
    call    #Clear
    call    #Delay2
    
    mov #0,R8
    mov #0,R9
    
    Letters A
    Letters T
    Letters T
    Letters A
    Letters c
    Letters K
    
    bic.b	#00000111b,&P2OUT     ;Clear the ports to send the Attak
    bis.b	#00000111b,&P2DIR     ;Set the ports to send the Attack
    
    jmp CounterSelectionA             ;Jump to counter selection A to select the Letter of the Attack
        
;-------------------------------------------------------------------------------
;           Send Ship Letter
;Select from A to D the letter of the Attack and then jump to Select the number 
;------------------------------------------------------------------------------
SendShipL:
    
        Madeship  R8,41h,CounterSelection2A
	Madeship  R8,42h,CounterSelection2A
	Madeship  R8,43h,CounterSelection2A
	Madeship  R8,44h,CounterSelection2A  
;-------------------------------------------------------------------------------
;            Send Ship Number
;Select from 1 to 4 the number of the Attack and then jump to Send the Letter of the Attack
;------------------------------------------------------------------------------
SendShipN:
        Madeship  R9,31h,SendLetter
	Madeship  R9,32h,SendLetter
	Madeship  R9,33h,SendLetter
	Madeship  R9,34h,SendLetter
;-------------------------------------------------------------------------------
;            Send Attack Letter To Rival
;Send the Letter selected to the ports of the enemy
;-------------------------------------------------------------------------------
SendLetter:

    cmp    #0x41,R8
    jeq    SendA     ;If the Letter selected is an A send it to the ports of the enemy
    
    cmp    #0x42,R8
    jeq    SendB    ;If the Letter selected is an B send it to the ports of the enemy
    
    cmp    #0x43,R8
    jeq    SendC    ;If the Letter selected is an C send it to the ports of the enemy
    
    cmp    #0x44,R8
    jeq    SendD    ;If the Letter selected is an D send it to the ports of the enemy
    
    
   
SendA    
	mov.b #00000100b,&P2OUT   ;#100 send an A to the ports of the Enemy
	jmp SendNumber            ;Jump to send the Number of the Attack     
	
SendB    
	mov.b #00000101b,&P2OUT   ;#101 send a B to the ports of the Enemy
	jmp SendNumber            ;Jump to send the Number of the Attack 
SendC    
	mov.b #00000110b,&P2OUT   ;#110 send a C to the ports of the Enemy
	jmp SendNumber            ;Jump to send the Number of the Attack 
SendD    
	mov.b #00000111b,&P2OUT   ;#111 send a D to the ports of the Enemy
	jmp SendNumber            ;Jump to send the Number of the Attack 

;-------------------------------------------------------------------------------
;            Send Attack Number To Rival
;Send the Number selected to the ports of the enemy
;-------------------------------------------------------------------------------
SendNumber:
    	call   #Delay
    	cmp    #0x31,R9         
    	jeq    Send1          ;If the Number selected is a 1 send it to the ports of the enemy
    
    	cmp    #0x32,R9
    	jeq    Send2          ;If the Number selected is a 1 send it to the ports of the enemy
    
    	cmp    #0x33,R9
    	jeq    Send3          ;If the Number selected is a 1 send it to the ports of the enemy
    
    	cmp    #0x34,R9
    	jeq    Send4          ;If the Number selected is a 1 send it to the ports of the enemy
Send1    
	mov.b #00000001b,&P2OUT   ;#001 send a 1 to the ports of the Enemy
	jmp Defense               ;Jmp to Defense Mode
	
Send2    
	mov.b #00000010b,&P2OUT   ;#010 send a 2 to the ports of the Enemy
	jmp Defense               ;Jmp to Defense Mode
Send3     
	mov.b #00000011b,&P2OUT   ;#010 send a 3 to the ports of the Enemy
	jmp Defense               ;Jmp to Defense Mode
Send4    
	mov.b #00000100b,&P2OUT   ;#100 send a 4 to the ports of the Enemy
	jmp Defense               ;Jmp to Defense Mode
;-------------------------------------------------------------------------------
;                Counter Selection Attack Mode
;-------------------------------------------------------------------------------
CounterSelectionA:       
       	call    #Delay2
              
 ;        Letter A
    	call    #Position           ;Position for Letter
        Counter	4h,41h              ;Counter Macro send an A to the Display             
    	bit.b   #10000000b,&P1IN    ;Bit Test for an interruption in port 1.7
    	jz      IntA2               ;If zero jump for interruption
    	clr.w    R12                ;Clear Register for next Write
    
              
;            Letter B      
    	call    #Position           ;Position for Letter
        Counter	4h,42h              ;Counter Macro to send an B to the Display             
    	bit.b   #10000000b,&P1IN    ;Bit Test for an interruption in port 1.7
    	jz      IntA2               ;If zero jump for interruption
    	clr.w    R12                ;Clear Register for next Write
      
       
;            Letter C      
        call    #Position           ;Position for Letter
        Counter	4h,43h              ;Counter Macro to send an C to the Display             
    	bit.b   #10000000b,&P1IN    ;Bit Test for an interruption in port 1.7
    	jz      IntA2               ;If zero jump for interruption
    	clr.w    R12                ;Clear Register for next Write
               
;            Letter D      
        call    #Position           ;Position for Letter
        Counter	4h,44h              ;Counter Macro to send an D to the Display             
    	bit.b   #10000000b,&P1IN    ;Bit Test for an interruption in port 1.7
    	jz      IntA2               ;If zero jump for interruption
    	clr.w    R12                ;Clear Register for next Write
       
    	call  #Buttons              ;Buttons Subroutine
    	bit.b #10000000b,&P1IN      ;Check for an interruption
    	jnz    CounterSelectionA    ;Else stay in the Loop      
IntA2     eint 
HERE2A   jmp    HERE2A
;-------------------------------------------------------------------------------
;                Counter Selection for Numbers for Attack Mode
;-------------------------------------------------------------------------------
CounterSelection2A:       
       call    #Delay2
      ; mov    #0,&CounterNumbers
      
             
 ;            Number 1
        call    #Position2         ;Position for Number
        Counter	3h,31h             ;Counter Macro to send a 1 to the Display 
    	bit.b 	#10000000b,&P1IN   ;Bit Test for an interruption in ports 1.7
    	jz    	IntA               ;If zero jump for interruption
    	clr    R12                 ;Clear Register for next Write
    
              
;            Number 2     
        call    #Position2         ;Position for Number
        Counter	3h,32h             ;Counter Macro to send a 2 to the Display 
    	bit.b 	#10000000b,&P1IN   ;Bit Test for an interruption in ports 1.7
    	jz    	IntA               ;If zero jump for interruption
    	clr    R12                 ;Clear Register for next Write
      
       
;            Number 3      
    	call    #Position2         ;Position for Number
        Counter	3h,33h             ;Counter Macro to send a 3 to the Display 
    	bit.b 	#10000000b,&P1IN   ;Bit Test for an interruption in ports 1.7
    	jz    	IntA               ;If zero jump for interruption
    	clr    R12                 ;Clear Register for next Write
               
;            Number 4       
    	call    #Position2         ;Position for Number
        Counter	3h,34h             ;Counter Macro to send a 4 to the Display 
    	bit.b 	#10000000b,&P1IN   ;Bit Test for an interruption in ports 1.7
    	jz    	IntA               ;If zero jump for interruption
    	clr    R12                 ;Clear Register for next Write
      
    	call  #Buttons             ;Buttons Subroutine
    	bit.b #10000000b,&P1IN     ;Check for an interruption
    	jnz    CounterSelection2A  ;Else keep the Loop to select a number      
IntA    eint                       ;Interruption
HEREA   jmp    HEREA   

;-------------------------------------------------------------------------------
;           Defense
;Receive the Letter and the Number of the Attack of the enemy and compare it 
;with the locations stored at the RAM
;------------------------------------------------------------------------------
Defense:
    	call    #Clear
    	call    #Delay2
	bic.b   #00111000b,&P2SEL       ;default
    	Letters D
    	Letters E
    	Letters F
    	Letters E
    	Letters n
    	Letters S
    	Letters E
	
	call	#Delay3
	call	#Delay3
	call	#Delay3
	call	#Delay3
	nop
	
	
	mov	#0,R7			;Letra  recibida
	mov	#0,R11			;Numero 
	
DLoop	bic.b   #00111000b,&P2SEL 		
        bic.b   #00111000b,&P2DIR       ;P2.5 & P2.4 & P2.3 as input ports
;		Letra recibida		
	mov.b	#00111000b,R7		;Value to be used with the AND.b
	AND.b	&P2IN,R7		;Receive the attack(letter) of the enemy
	cmp	#0,R7			;Received?
	jz	DLoop			;Repeat
			
SendLetterDefend:

    cmp    #00100000b,R7		;A
    jeq    SendAD
    
    cmp    #00101000b,R7		;B
    jeq    SendBD
    
    cmp    #00110000b,R7		;C
    jeq    SendCD
    
    cmp    #00111000b,R7		;D
    jeq    SendDD
    
    cmp    #00010000b,R7                ;Winner MSG
    jeq    Winner
       
SendAD    
	mov.b #0x41,ReceiveL         ;If an A send it to the Direction in RAM
	jmp SendNumberD              ;Jmp to send the Number
	
SendBD    
	mov.b #0x42,ReceiveL        ;If a B send it to the Direction in RAM
	jmp SendNumberD             ;Jmp to send the Number
SendCD    
	mov.b #0x43,ReceiveL        ;If a C send it to the Direction in RAM
	jmp SendNumberD             ;Jmp to send the Number
SendDD    
	mov.b #0x44,ReceiveL        ;If a D send it to the Direction in RAM
	jmp SendNumberD             ;Jmp to send the Number
	
SendNumberD:
;		Numero recibido

DLoop2
	call	#Delay2
	call	#ClearPorts	
	bic.b   #00111000b,&P2SEL 		
        bic.b   #00111000b,&P2DIR       ;P2.5 & P2.4 & P2.3 as input ports
	mov.b	#00111000b,R11		;Value to be used with the AND.b
	AND.b	&P2IN,R11		;Receive the attack(number) of the enemy
	cmp	#0,R11			;Received?
	jz	DLoop2			;Repeat
		
    cmp    #00001000b,R11		;1
    jeq    Send1D
    
    cmp    #00010000b,R11		;2
    jeq    Send2D
    
    cmp    #00011000b,R11		;3
    jeq    Send3D
    
    cmp    #00100000b,R11		;4
    jeq    Send4D
       
Send1D    
	mov.b #0x31,ReceiveN		;If it is a 1 send it to the Direction in RAM
	jmp Dmode                       ;Jmp to Defense Mode
	
Send2D    
	mov.b #0x32,ReceiveN            ;If it is a 2 send it to the Direction in RAM
	jmp Dmode                       ;Jmp to Defense Mode
Send3D      
	mov.b #0x33,ReceiveN            ;If it is a 3 send it to the Direction in RAM
	jmp Dmode                       ;Jmp to Defense Mode
Send4D    
	mov.b #0x34,ReceiveN            ;If it is a 4 send it to the Direction in RAM
	jmp Dmode                       ;Jmp to Defense Mode
		
	
;-------------------------------------------------------------------------------
;                 Defense Mode Compare Logic 
;Compare the Letter and the Number received if the coord is the same of the one 
;stored at the RAM, the RAM will receive a zero and the HIT msg will appear in 
;the display ELSE appears the Miss Msg
;-------------------------------------------------------------------------------    
;----------------------LETTER AREA----------------------------------------------
Dmode:
DLetter1B1:
	cmp Letra1B1,ReceiveL	;CMP First Letter of First Ship with the received Letter	
	jeq DNum1B1             ;If equal jmp to cmp the number 
DLetter2B1:
	cmp Letra2B1,ReceiveL   ;CMP Second Letter of First Ship with the received Letter	
	jeq DNum2B1             ;If equal jmp to cmp the number 
	
DLetter1B2:
	cmp Letra1B2,ReceiveL   ;CMP First Letter of Second Ship with the received Letter	
	jeq DNum1B2             ;If equal jmp to cmp the number 
DLetter2B2:
	cmp Letra2B2,ReceiveL   ;CMP Second Letter of Second Ship with the received Letter	
	jeq DNum2B2             ;If equal jmp to cmp the number 
	
DLetter1B3:
	cmp Letra1B3,ReceiveL   ;CMP First Letter of Third Ship with the received Letter	
	jeq DNum1B3             ;If equal jmp to cmp the number 
DLetter2B3:
	cmp Letra2B3,ReceiveL   ;CMP Second Letter of Third Ship with the received Letter	
	jeq DNum2B3             ;If equal jmp to cmp the number 

        jmp Miss
;-----------------------------NUMBER AREA---------------------------------------
DNum1B1:
	cmp Num1B1,ReceiveN     ;CMP First Number of First Ship with the received Number	
	jeq Explode1B1          ;If equal clear the Direction in RAM to explode the ship
	jmp DLetter2B1          ;Else check the other coord in Direction
DNum2B1:
	cmp Num2B1,ReceiveN     ;CMP Second Number of First Ship with the received Number
	jeq Explode2B1          ;If equal clear the Direction in RAM to explode the ship
	jmp DLetter1B2          ;Else check the other coord in Direction
	
DNum1B2:
	cmp Num1B2,ReceiveN     ;CMP First Number of Second Ship with the received Number	
	jeq Explode1B2          ;If equal clear the Direction in RAM to explode the ship
	jmp DLetter2B2          ;Else check the other coord in Direction
DNum2B2:
	cmp Num2B2,ReceiveN      ;CMP Second Number of Second Ship with the received Number
	jeq Explode2B2           ;If equal clear the Direction in RAM to explode the ship
	jmp DLetter1B3           ;Else check the other coord in Direction
	
DNum1B3:
	cmp Num1B3,ReceiveN     ;CMP First Number of Third Ship with the received Number	
	jeq Explode1B3          ;If equal clear the Direction in RAM to explode the ship
	jmp DLetter2B3          ;Else check the other coord in Direction
DNum2B3:
	cmp Num2B3,ReceiveN     ;CMP Second Letter of Third Ship with the received Number
	jeq Explode2B3          ;If equal clear the Direction in RAM to explode the ship
	jmp Miss                ;Else the Attack MISS, send the MSG to the Display
;-------------------ERASING SHIP WHEN HIT --------------------------------------
Explode1B1:
	   mov    #0,&Letra1B1  ;Clear the memory in RAM to explode the ship
           mov    #0,&Num1B1    ;Clear the memory in RAM to explode the ship
	   JMP    KO            ;Send HIT MSG
Explode2B1:
	   mov    #0,&Letra2B1  ;Clear the memory in RAM to explode the ship
           mov    #0,&Num2B1    ;Clear the memory in RAM to explode the ship
	   JMP    KO            ;Send HIT MSG
Explode1B2:
	   mov    #0,&Letra1B2  ;Clear the memory in RAM to explode the ship
           mov    #0,&Num1B2    ;Clear the memory in RAM to explode the ship
	   JMP    KO            ;Send HIT MSG
Explode2B2:
	   mov    #0,&Letra2B2  ;Clear the memory in RAM to explode the ship
           mov    #0,&Num2B2    ;Clear the memory in RAM to explode the ship
	   JMP    KO            ;Send HIT MSG
Explode1B3:
	   mov    #0,&Letra1B3  ;Clear the memory in RAM to explode the ship
           mov    #0,&Num1B3    ;Clear the memory in RAM to explode the ship
	   JMP    KO            ;Send HIT MSG
Explode2B3:
	   mov    #0,&Letra2B3  ;Clear the memory in RAM to explode the ship
           mov    #0,&Num2B3    ;Clear the memory in RAM to explode the ship
	   JMP    KO            ;Send HIT MSG
;------------------------------------------------------------------------------
;                              K.O Message
;------------------------------------------------------------------------------
KO:
    inc     GameOverFinal       ;Counter to know if the GAME is OVER    
    call    #Clear
    call    #Delay2
;Turn ON the RED LEd and the BUZZER using Port 2.6    
    bic.b  #01000000b,&P2SEL       ;default
    bis.b  #01000000b,&P2DIR       ; P2.6 outputs 

    bis.b   #01000000b,&P2OUT
    call    #Delay2
    call    #Delay2
    bic.b   #01000000b,&P2OUT
    call    #Delay
        
    bis.b   #01000000b,&P2OUT
    call    #Delay2
    call    #Delay2
    bic.b   #01000000b,&P2OUT
    call    #Delay
    
    bis.b   #01000000b,&P2OUT
    call    #Delay2
    bic.b   #01000000b,&P2OUT
    call    #Delay
    
    Letters H
    Letters I
    Letters T
    call    #Delay2
    call    #Delay2

    cmp   #6,GameOverFinal          ;If it is 6 the Game is OVER
    jeq  GameOver
    call  #Attack                   ;Else Jmp to Attack Mode 

;------------------------------------------------------------------------------
;                              Miss Message
;------------------------------------------------------------------------------
Miss:
    call    #Clear
    call    #Delay2
;Turn ON GREEN LED using Port 2.7    
    bic.b   #10000000b,&P2SEL       ;default
    bis.b   #10000000b,&P2DIR       ; P1 outputs 
    
    bis.b   #10000000b,&P2OUT
    call    #Delay2
    bic.b   #10000000b,&P2OUT
    call    #Delay2
    
    
    bis.b   #10000000b,&P2OUT
    call    #Delay2
    call    #Delay2
    bic.b   #10000000b,&P2OUT
    call    #Delay
    
    Letters M
    Letters I
    Letters S
    Letters S
    call    #Delay2
    call    #Delay2
    call    #Attack             ;Jmp to Attack Mode 
;------------------------------------------------------------------------------
;                              Game Over Message
;------------------------------------------------------------------------------
GameOver:
    call    #Clear
    call    #Delay2
    Letters G
    Letters A
    Letters M
    Letters E
    call    #Space
    Letters O
    Letters v
    Letters E
    Letters R
    call    #Delay2
    call    #Delay2
;After GAME OVER MSG send a #010 to the enemy to activate the Winner MSG in the enemy Display    
    bic.b   #00000111b,&P2OUT
    bis.b   #00000111b,&P2DIR
    mov.b   #00000010b,&P2OUT
    call    #Delay2
    call    #Delay2
;Turn ON the RED LED and the BUZZER   
    bis.b   #01000000b,&P2OUT
    call    #Delay3
    call    #Delay3
    bic.b   #01000000b,&P2OUT
    call    #Delay
    
    call    #Start    ;Start the Game 
;------------------------------------------------------------------------------
;                             Winner Message
;------------------------------------------------------------------------------   
Winner:    
    call  #Clear
    call  #Delay2
    Letters W
    Letters I
    Letters n
    Letters n
    Letters E
    Letters R
    call  #Delay2
    call  #Delay2
    call  #Delay2
    call  #Start
;-------------------------------------------------------------------------------
;           P1.6 & P1.7 Init Buttons POLLING
;------------------------------------------------------------------------------
Buttons bic.b   #11000000b,&P1SEL       ;default
        bic.b   #11000000b,&P1DIR       ;P1.6 & P1.7 as input ports
        bis.b   #11000000b,&P1REN       ;Internal resistor for both buttons
        bis.b   #11000000b,&P1OUT       ;make it pull up 
        bis.b   #11000000b,&P1IE        ;enable P1.6 and P1.7 as int.     
        ret        
;-------------------------------------------------------------------------------
;            Position for Second Line
;-------------------------------------------------------------------------------
Position:			  ;Position for each Letter
        mov.w   #0xC,R4           ;Command C8 start at position
        call    #Command
        mov.w   #0x8,R4
        call    #Command
        call    #Delay2
        clr.w   R4 
        ret
Position2:			  ;Position for each number
        mov.w   #0xC,R4           ;Command C9 start at position
        call    #Command
        mov.w   #0x9,R4
        call    #Command
        call    #Delay2
        clr.w   R4 
        ret
  
;-------------------------------------------------------------------------------
;                Write Ship Subroutine
;-------------------------------------------------------------------------------
WritingShip2
      
    mov.b    R12,&P1OUT
    bis.b    #00010000b,&P1OUT   ;R/S = 1 = P1.4
    call    #Nybble      
    ret      
;-------------------------------------------------------------------------------
;                Write Ship Subroutine
;-------------------------------------------------------------------------------
WritingShip
      
        mov.b    R12,&P1OUT
    bis.b    #00010000b,&P1OUT   ;R/S = 1 = P1.4
    call    #Nybble
    rla.b    R12            ;Shift the bits to the left to send the next one
    rla.b    R12            ;Shift the bits to the left to send the next one
    rla.b    R12            ;Shift the bits to the left to send the next one
    rla.b    R12            ;Shift the bits to the left to send the next one
              
    ret
;-------------------------------------------------------------------------------
;                Write Subroutine
;-------------------------------------------------------------------------------
Writing    mov.b    R5,&P1OUT
    bis.b    #00010000b,&P1OUT    ;R/S = 1 = P1.4
    call    #Nybble
    rla    R5        ;Shift the bits to the left to send the next one
    rla    R5        ;Shift the bits to the left to send the next one
    rla    R5        ;Shift the bits to the left to send the next one
    rla    R5        ;Shift the bits to the left to send the next one
    ret
;-------------------------------------------------------------------------------
;                Command Subroutine
;-------------------------------------------------------------------------------
Command    mov.b    R4,&P1OUT    ;Move the first 4 MSB to P1
    call    #Nybble        ;Nybble subroutine
    rla    R4        ;Shift the bits to the left to send the next one
    rla    R4        ;Shift the bits to the left to send the next one
    rla    R4        ;Shift the bits to the left to send the next one
    rla    R4        ;Shift the bits to the left to send the next one
    ret            ;Return from subroutine
;-------------------------------------------------------------------------------
;                Nybble Subroutine
;-------------------------------------------------------------------------------
Nybble    bis.b    #00100000b,&P1OUT    ;E=1 = P1.5
    call    #Delay            ;Delay
    bic.b   #00100000b,&P1OUT    ;E=0 = P1.5
    ret
;-------------------------------------------------------------------------------
;            Space   
;-------------------------------------------------------------------------------
Space   mov.w    #0x2,R5            	;Move E t0 P1OUT = 0x45   
    call    #Writing       		;Call Write Subroutine
    mov.w   #0x0,R5            		;Move E to P1OUT = 0x45
    call    #Writing        		;Call Write subroutine
    clr.w   R5            		;Clear Register for next Write
    call    #Delay2
    ret
;-------------------------------------------------------------------------------
;            Clear
;-------------------------------------------------------------------------------   
Clear	mov.w   #0x0,R4            ;Return Home set = 0x02
    	call    #Command        ;Call command subroutine
    	mov.w   #0x1,R4            ;Return Home set = 0x02
    	call    #Command        ;Call command subroutine
    	clr.w    R4            ;Clear Register for next command   
       
        mov.w   #0x8,R4            ;Return Home set = 0x02
    	call    #Command        ;Call command subroutine
    	mov.w   #0x0,R4            ;Return Home set = 0x02
    	call    #Command        ;Call command subroutine
    	clr.w    R4            ;Clear Register for next command   
       
        ret   
;-------------------------------------------------------------------------------
;            ClearPorts
;-------------------------------------------------------------------------------
ClearPorts  bic.b #11111111b,&P1OUT
            bic.b #11111111b,&P2OUT
	    ret
;-------------------------------------------------------------------------------
;                Delays
;-------------------------------------------------------------------------------
Delay	mov.w #10000 ,R15         	; Delay to R15 150 ms
L1      dec R15             		; Decrement R15
        jnz L1                		 ; Delay over?
        ret   
Delay2	mov.w    #65000,R15       
L2      dec R15
        jnz L2
        ret                          
Delay3:
	call #Delay2
	call #Delay2
	call #Delay2
	call #Delay2
	call #Delay2
	call #Delay2
	ret
;-------------------------------------------------------------------------------
;            Interrupt Vectors
;-------------------------------------------------------------------------------
    	ORG     0FFFEh            	;MSP430 RESET Vector
    	DW      RESET             	;
        ORG     0FFE4h                  ; interrupt vector 2
        DW      PBISR                   ; address of label PBISR
       
    	END
;R4 = Commands
;R5 = Write
;R6 = Letra  coordenada Barco 1
;R7 = Numero coordenada Barco 1
;R8 = Letra  coordenada Barco 2
;R9 = Numero coordenada Barco 2
;R10 = Letra coordenada Barco 3
;R11 = Numero coordenada Barco 3

;R12 = Counter Selection & CounterSelection2
;R13 = Macro Letras
;
;R14 = Delays
;$15 = Delays

;Jugador 1 = Ataque
;Jugador 2 = Defensa

;Letra1B1 = Letra 1 Barco 1 (Letra de la primera coordenada del barco 1)
;Num1B1      - Numero 1 Barco 1 (Numero de la primera coordenada del barco 1)