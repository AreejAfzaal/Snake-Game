[org 0x0100]      
        
jmp  start 

speedOfSnake: dw 16
ticker: dw   0 ; ;tiickcount ki jaga ticker
tickcount: dw 1456 ;seconds ki tickcount
clocktick: dw 0
seconds: dw 0
upflag: dw 0
downflag: dw 0
leftflag: dw 1
rightflag: dw 0
currentDirection: dw 4
lives: dw 4
snake: times 240 db 'O'
index: times 240 dw 0
sizeofsnake: dw 20
FruitIndex: dw 0
oldkb: dd 0
string: db '*************WELCOME TO SNAKE GAME*************'
string1: db 'Press I for Instructions and Press any key to Continue'
inst: db '****INSTRUCTIONS****'
inst1: db '1. Use the arrow keys to move the snake'
inst2: db '2. This is a single player game'
inst3: db '3. The game will end in 4 minutes or when the player loses all the lives'
inst4: db '4. Initially the player has three lives'
inst5: db '5. If the snakes head touches the border, the player loses one life'
inst6: db '6. If the snake touches itself the player loses one life'
inst7: db 'Press any key to return'
timeleft:db 'Time Remaining :'
scorest:db 'Points :'
score: dw 0
livesleft:db 'Remaining Lives :'
totallives:db 'Total Lives : 3'
GameEndMsg: db 'YOU ARE DEAD'
msglen: dw 12
time: dw 240
timeprint: dw 240
level_1: db 'Level 1'
level_2:db 'Level 2'
displayflag: dw 1
msglenforwin: dw 7
GameWinMsg: db 'YOU WIN'
finalscore: db 'Your final score is: '
fslength: dw 21
sound_index dw 0

	sound_data:
			incbin "audio.wav" ; 222,556 bytes
;===========================================================================================;
printnum:    
		push bp     
        mov  bp, sp        
		push es           
		push ax           
		push bx          
		push cx          
		push dx          
		push di 
 
        mov  ax, 0xb800         
		mov  es, ax             ; point es to video base  
		mov  ax, [bp+4]         ; load number in ax     
        mov  bx, 10             ; use base 10 for division 
		mov  cx, 0              ; initialize count of digits 
 
nextdigit: 
	mov  dx, 0              ; zero upper half of dividend       
	div  bx                 ; divide by 10           
    add  dl, 0x30           ; convert digit into ascii value          
	push dx                 ; save ascii value on stack            

	inc  cx                 ; increment count of values        
	cmp  ax, 0              ; is the quotient zero            
	jnz  nextdigit          ; if no divide it again 
 
	mov  di, [bp + 6]        ; point di to the location 
 
nextpos:     
	pop  dx                 ; remove a digit from the stack        
	mov  dh, [bp+8]           ; use normal attribute           
    mov  [es:di], dx        ; print char on screen        
	add  di, 2              ; move to next screen location      
	loop nextpos            ; repeat for all digits on stack 
 
    pop  di  
	pop  dx    
	pop  cx        
	pop  bx       
	pop  ax 
	pop  es          
	pop  bp    
	
	ret  6
;========================================================================================;
clrscr1:
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	push ds
	
	push cs
	pop ds
	
	mov ax, 0xb800								
	mov es, ax							;point es to video base
	xor di,di							;point di to top left column
	mov ax, 0x3020						;space char in normal attribute
	mov cx, 2000						;number of screen location		
	
	cld									;clear direction flag
	rep stosw							;clear the whole screen
	
	pop ds
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	
	ret
;========================================================================================;
printstr1: 
push bp
mov bp, sp
push es
push ax
push cx
push si
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov di, [bp+8] ; point di to given coordinate
mov si, [bp+6] ; point si to string
mov cx, [bp+4] ; load length of string in cx
mov ax,[bp+10]
nextchar:
 mov al, [si] ; load next char of string
mov [es:di], ax ; show this char on screen
add di, 2 ; move to next screen location
add si, 1 ; move to next char in string
loop nextchar ; repeat the operation cx times
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 8
;=========================================================================================;
print_below:

push 0x0300
push 0
push timeleft
push 16
call printstr1
;---------------;

push 0x03
push 34
push word[cs:timeprint]

call printnum
;---------------;
push 0x0500
push 50
push scorest
push 8
call printstr1
;---------------;
push 0x05
push 68
push word[cs:score]
call printnum
;---------------;
push 0x0200
push 90
push livesleft
push 17
call printstr1
;---------------;
push 0x02
push 126
push word[lives]
call printnum

ret
;=========================================================================================;
Instructions:

push 28
push 3
push inst
push 20
call printstr
;-------------;
push 5
push 8
push inst1
push 39
call printstr
;-------------;
push 5
push 9
push inst2
push 31
call printstr
;-------------;
push 5
push 10
push inst3
push 72
call printstr
;-------------;
push 5
push 11
push inst4
push 39
call printstr
;-------------;
push 5
push 12
push inst5
push 67
call printstr
;-------------;
push 5
push 13
push inst6
push 56
call printstr
;-------------;
push 25
push 19
push inst7
push 23
call printstr

 ret
;=========================================================================================;
displaystart: 
push 15
push 3
push string
push 47
call printstr

push 12
push 10
push string1
push 54
call printstr

ret
;==========================================================================================;
printstr:
push bp
mov bp , sp
pusha 
push es
mov ax , 80
mul byte[bp + 8]		; y posoiton * 80
add ax , [bp + 10]		; adding x position
shl ax , 1				; *2
mov di , ax
mov si , [bp + 6]		;pointing to data
mov ax , 0xb800
mov es , ax
mov cx , [bp + 4]
mov ah , 0x30

next:
call delay
cld
lodsb 
stosw
loop next

pop es
popa 
pop bp
ret 8
;====================================================================================;
delay:
 push cx 
push ax 
mov ax,3 
agan: 
mov cx,0xffff 
again: 
loop again
 dec ax 
cmp ax,0 
jne again
 pop ax 
pop cx
 ret
;====================================================================================;
display:
	push es
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	cmp word[cs:displayflag], 1
	jne level2
	push 0xb800
	pop es

	mov di,160
	mov cx,80
	mov ax,0x6020 ;borders

	cld
	rep stosw

	mov di,3840
	mov cx, 80
	
	cld
	rep stosw

	mov cx,25
	mov si,476
	mov di,320

	loop1:
		mov [es:di],ax
		add di,2
		mov [es:di],ax
		add di,158
		mov [es:si],ax
		add si,2
		mov [es:si],ax
		add si,158
		
		loop loop1
	jmp exitlevel
	
	
	level2:
	cmp word[cs:displayflag], 3
	je level3
	push 0xb800
	pop es

	mov di,320
	mov cx,80
	mov ax,0x1020 ;borders

	cld
	rep stosw

	mov di,3840
	mov cx, 80
	
	cld
	rep stosw

	mov cx,25
	mov si,476
	mov di,320

	loopnew1:
		mov [es:di],ax
		add di,2
		mov [es:di],ax
		add di,158
		mov [es:si],ax
		add si,2
		mov [es:si],ax
		add si,158
		
		loop loopnew1
	mov cx, 20
	mov di, 2300
	mov si, 1660
	loopnew2:	
		mov [es:di],ax
		add di, 2
		mov [es:si],ax
		add si, 2
		loop loopnew2
	
	jmp exitlevel

	level3:
	push 0xb800
	pop es

	mov di,320
	mov cx,80
	mov ax,0x4020 ;borders

	cld
	rep stosw

	mov di,3840
	mov cx, 80
	
	cld
	rep stosw

	mov cx,25
	mov si,476
	mov di,320

	loop31:
		mov [es:di],ax
		add di,2
		mov [es:di],ax
		add di,158
		mov [es:si],ax
		add si,2
		mov [es:si],ax
		add si,158
		
		loop loop31
	mov cx, 15
	mov di, 840
	mov si, 910
	loop32:	
		mov [es:di],ax
		add di, 160
		mov [es:si],ax
		add si, 160
		loop loop32
	mov cx, 10
	mov si, 1328
	mov di, 2608
	loop33:
		mov [es:si], ax
		add si, 6
		mov [es:di], ax
		add di, 6
		loop loop33
	
	exitlevel:	
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	
	ret
;==================================================================;
clrscr:
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	push ds
	
	push cs
	pop ds
	
	mov ax, 0xb800								
	mov es, ax							;point es to video base
	xor di,di							;point di to top left column
	mov ax, 0x0320						;space char in normal attribute
	mov cx, 2000						;number of screen location		
	
	cld									;clear direction flag
	rep stosw							;clear the whole screen
	call display
	call print_below
	pop ds
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	
	ret
;===========================================================;
printsnake:
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	push ds
	
	push cs
	pop ds
	mov ah,0x02
	xor si,si
	
	push 0xb800
	pop es
	mov bx,0
	mov cx,[cs:sizeofsnake]
	printing:
	mov di,[cs:index+bx]
	mov al,[cs:snake+si]
	mov word[es:di],ax
	add si,1
	add bx,2
	loop printing
	mov si,[cs:FruitIndex]
	mov ax,si
	mov bx,10
	div bx
	add dx,228
	mov ax,dx
	mov ah,0x8D
	mov word[es:si],ax
	pop ds
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	
	ret
;======================================================;
initialize:
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	push ds
	
	push cs
	pop ds
	
	mov byte[cs:snake],'X'
	mov word[cs:sizeofsnake],20
	mov cx,120
	xor si,si
	clearIndex:
	mov word[cs:index+si],5000
	add si,2
	loop clearIndex
	
	
	mov cx, 20
	xor si, si
	mov ax, 1980
	set_index:
	mov word[cs:index + si], ax
	add ax, 2
	add si, 2
	loop set_index
	mov word [cs:upflag], 0; set flag to start printing     
	mov word[cs:downflag],0
	mov word[cs:leftflag],1
	mov word[cs:rightflag],0 
	mov word[cs:currentDirection],4
	dec word[cs:lives]
	cmp word[cs:lives],0
	je GameEnd
	initializeDone:
	pop ds
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	
	ret
	
	GameEnd:
	call clrscr1
	mov si,0
	mov di,1988
	mov cx,[cs:msglen]
	push 0xb800
	pop es
	mov ah,0x34
	EndLoop:
	mov al,[cs:GameEndMsg+si]
	stosw
	add si,1
	loop EndLoop
	rep movsw
	jmp $
;=========================================================;
Eat_fruit:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	push ds
	
	push cs
	pop ds	
	
	push 0xb800
	pop es
	
	mov bx, [bp + 4]
	mov ax, [cs : index]
	add ax, bx
	cmp ax,[cs:FruitIndex]
	jne FruitDone
	;-----------------------------------------------------;
	add word[cs:sizeofsnake],4
	add word[cs:score], 4
	;-----------------------------------------------------;
	call GenerateFruit
	FruitDone:
	pop ds
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2

;===============================================================================================;
GenerateFruit:
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	push ds
	
	push cs
	pop ds
	
	FindAgain:
	inc word[cs:tickcount]
	push 0xb800
	pop es
	push 0
	push word[cs:tickcount]
	call RandomPlace
	pop si
	cmp word[es:si],0x6020				;Boundary CHECK
	je FindAgain
	cmp word[es:si],0x1020				;Boundary CHECK
	je FindAgain
	cmp word[es:si],0x4020				;Boundary CHECK
	je FindAgain
	mov ah,0x02
	mov al,'O'
	cmp word[es:si],ax			;ON SNAKE BODY CHECK
	je FindAgain
	mov ah,0x02
	mov al,'X'
	cmp word[es:si],ax			;ON HEAD CHECK
	je FindAgain
	mov ax, 3520
	mov cx, 0
	
	mov word[cs:FruitIndex],si
	
	pop ds
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	
	ret
;============================================================;	
checkCollision:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	push ds
	
	push cs
	pop ds
	;------------------;
	mov bx,[bp+4]
	mov ax,[cs:index]
	add ax,bx
	mov cx,[cs:sizeofsnake]
	xor si,si
	checkIndex:
	cmp ax,[cs:index+si]
	je collide
	add si,2
	loop checkIndex
	;------------------;
	outOfCollision:
	pop ds
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
	collide:
	mov word[bp+6],1
	jmp outOfCollision
	;=============================================================;
checkBoundary:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	push ds
	
	push cs
	pop ds	
	push 0xb800
	pop es
	mov bx, [bp + 4]
	mov ax, [cs : index]
	add ax, bx
	mov si,ax
	cmp word[es:si],0x6020
	je outOfBoundary
	cmp word[es:si],0x1020
	je outOfBoundary
	cmp word[es:si],0x4020
	je outOfBoundary

	back:
	pop ds
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
	outOfBoundary:
	mov word[bp+6],1 ;dx=1
	jmp back
;===============================================================================;	
	
RandomPlace:
push bp
mov bp,sp
push ax
push bx
push cx
push dx
push si
push di
push es
push ds

push cs
pop ds
mov dx,[bp+4]
mov ax, dx
xor dx, dx
mov cx,1680
div cx
add dl,0   
mov si,dx
shl si,1
add si,320
mov word[bp+6],si


pop ds
pop es
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop bp
ret 2
;========================================================;
moveSnake:
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	push ds
	
	push cs
	pop ds
	;-------------------------------;
	checkUp:
	cmp word[cs:upflag],1
	jne checkDown
	cmp word[cs:currentDirection],3
	je checkDone
	;-------------------;
	;Body Collisions
	;--------------------;
	push 0
	push -160
	call checkCollision
	pop dx
	cmp dx,1
	je checkDone
	;-------------------;
	;Boundary Collisions
	;--------------------;
	push 0
	push -160
	call checkBoundary
	pop dx
	cmp dx,1
	je checkDone
	;-------------------;
	;Fruit Eaten
	;--------------------;
	push -160
	call Eat_fruit
	;----------------------;
	mov cx,[cs:sizeofsnake]
	dec cx
	mov si,cx

	shl si,1
	shiftsnakeup:
	mov ax,[cs:index+si-2]
	mov word[cs:index+si],ax
	sub si,2
	loop shiftsnakeup
	add word[cs:index+si],-160
	mov word[cs:currentDirection],1
	
	jmp checkDone
	;------------------------------;
	checkDown:
	cmp word[cs:downflag],1
	jne checkLeft
	cmp word[cs:currentDirection],1
	je checkDone
	;-------------------;
	;Body Collisions
	;--------------------;
	push 0
	push 160
	call checkCollision
	pop dx
	cmp dx,1
	je checkDone
	;-------------------;
	;Boundary Collisions
	;--------------------;
	push 0
	push 160
	call checkBoundary
	pop dx
	cmp dx,1
	je checkDone
	;-------------------;
	;Fruit Eaten
	;--------------------;
	push 160
	call Eat_fruit
	;----------------------;
	mov cx,[cs:sizeofsnake]
	dec cx
	mov si,cx

	shl si,1
	shiftsnakedown:
	mov ax,[cs:index+si-2]
	mov word[cs:index+si],ax
	sub si,2
	loop shiftsnakedown
	add word[cs:index+si],160
	mov word[cs:currentDirection],3
	jmp checkDone
	;---------------------------------;
	checkLeft:
	cmp word[cs:leftflag],1
	jne checkRight
	cmp word[cs:currentDirection],2
	je checkDone
	;-------------------;
	;Body Collisions
	;--------------------;
	push 0
	push -2
	call checkCollision
	pop dx
	cmp dx,1
	je checkDone
	;-------------------;
	;Boundary Collisions
	;--------------------;
	push 0
	push -2
	call checkBoundary
	pop dx
	cmp dx,1
	je checkDone
	;-------------------;
	;Fruit Eaten
	;--------------------;
	push -2
	call Eat_fruit
	;----------------------;
	mov cx,[cs:sizeofsnake]
	dec cx
	mov si,cx

	shl si,1
	shiftsnakeleft:
	mov ax,[cs:index+si-2]
	mov word[cs:index+si],ax
	sub si,2
	loop shiftsnakeleft
	add word[cs:index+si],-2
	mov word[cs:currentDirection],4
	jmp checkDone
	;--------------------------------;
	checkRight:
	cmp word[cs:rightflag],1
	jne checkDone
	cmp word[cs:currentDirection],4
	je checkDone
	;-------------------;
	;Body Collisions
	;--------------------;
	push 0
	push 2
	call checkCollision
	pop dx
	cmp dx,1
	je checkDone
	;-------------------;
	;Boundary Collisions
	;--------------------;
	push 0
	push 2
	call checkBoundary
	pop dx
	cmp dx,1
	je checkDone
	;-------------------;
	;Fruit Eaten
	;--------------------;
	push 2
	call Eat_fruit
	;----------------------;
		
	mov cx,[cs:sizeofsnake]
	dec cx
	mov si,cx

	shl si,1
	shiftsnakeright:
	mov ax,[cs:index+si-2]
	mov word[cs:index+si],ax
	sub si,2
	loop shiftsnakeright
	add word[cs:index+si],2
	mov word[cs:currentDirection],2


	;---------------------------------;
	checkDone:
	cmp dx,1
	je Foul
	call printsnake
	pop ds
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret
	Foul:
	call clrscr
	call initialize
	mov dx,0
	jmp checkDone
	up_found:
	mov word[upflag], 1
	mov word[downflag], 0
	mov word[leftflag], 0
	mov word[rightflag], 0
	jmp checkDone
	;--------------------;
	down_found:
	mov word[upflag], 0
	mov word[downflag], 1
	mov word[leftflag], 0
	mov word[rightflag], 0
	jmp checkDone
	;---------------------;
	left_found:
	mov word[upflag], 0
	mov word[downflag], 0
	mov word[leftflag], 1
	mov word[rightflag], 0
	jmp checkDone
	;----------------------;
	right_found:
	mov word[upflag], 0
	mov word[downflag], 0
	mov word[leftflag], 0
	mov word[rightflag], 1
	jmp checkDone
 ;========================================================;
kbisr:        
call display
push ax  
cmp word[cs:displayflag], 0
je nomatch
in   al, 0x60           ; read char from keyboard port 
cmp al,0x48
jne nextcmp
cmp word[cs:downflag],1
je exit 
cmp  word [cs:upflag], 1; is the flag already set               

 je   fast               ; yes, leave the ISR 
 
	mov word [cs:upflag], 1; set flag to start printing     
	mov word[cs:downflag],0
	mov word[cs:leftflag],0
	mov word[cs:rightflag],0 
 jmp  exit 

nextcmp:
cmp al,0x50
jne nextcmp2
cmp word[cs:upflag],1
je exit 
cmp  word [cs:downflag], 1; is the flag already set               
 je   fast
	mov word [cs:upflag], 0; set flag to start printing     
	mov word[cs:downflag],1
	mov word[cs:leftflag],0
	mov word[cs:rightflag],0 
 jmp exit
 
 
 nextcmp2:
cmp al,0x4B
jne nextcmp3
cmp word[cs:rightflag],1
je exit 
cmp  word [cs:leftflag], 1; is the flag already set               
 je fast
 
	mov word [cs:upflag], 0; set flag to start printing     
	mov word[cs:downflag],0
	mov word[cs:leftflag],1
	mov word[cs:rightflag],0 
 jmp exit
 
 nextcmp3:
	cmp al,0x4D
	jne nomatch
	cmp word[cs:leftflag],1
	je exit 
	cmp  word [cs:rightflag], 1; is the flag already set   
	je   fast 
	mov word [cs:upflag], 0; set flag to start printing     
	mov word[cs:downflag],0
	mov word[cs:leftflag],0
	mov word[cs:rightflag],1 
 jmp exit
 nomatch:
 
 pop  ax               
 jmp  far [cs:oldkb]     ; call original ISR 
 
 
exit:         
mov  al, 0x20               
out  0x20, al           ; send EOI to PIC 
pop  ax             
iret
fast:
;call clrscr
;call moveSnake
jmp exit
 ;================================================================;

timer:   
push ax
push bx 
push cx   
 
call display

inc word[cs:tickcount]
inc word[cs:ticker] 
inc word[cs:clocktick]
cmp word[cs:sizeofsnake], 40 	;if size max then move to next stage
je TimeEnd
; push 0x07
; push 3070
; push word[cs:tickcount]
; call printnum
mov bx,[cs:tickcount]
sub bx,1456
cmp bx, 4320  ;4320
je lose
cmp word[cs:speedOfSnake], 1
jbe Skip
cmp word[cs:clocktick],18
jne continue

mov word[cs:clocktick],0
dec word[cs:timeprint]
inc word[cs:seconds]
cmp word[cs:seconds],20
jne continue
mov word[cs:seconds],0
mov ax,[cs:speedOfSnake]
shr ax,1
mov word[cs:speedOfSnake],ax
continue:
mov bx,[cs:speedOfSnake]
inc bx
cmp word[cs:ticker],bx
jne skipall
Skip:
cmp word[cs:speedOfSnake], 1
ja go
cmp word[cs:clocktick],18
jne go

mov word[cs:clocktick],0
dec word[cs:timeprint]

go:
call clrscr
call moveSnake


mov word[cs:ticker],0
skipall:
mov  al, 0x20
out  0x20, al           ; end of interrupt 
pop cx
pop bx
pop ax           
iret                    ; return from interrupt 
 

 
 nextstage:
 ;inc word[cs:displayflag]
 mov word[cs:speedOfSnake], 16
 mov word[cs:ticker], 0
 mov word[cs:clocktick], 0
 mov word[cs:seconds], 0
 inc word[cs:lives]
call clrscr
call print_below
call initialize
call GenerateFruit
 jmp skipall

TimeEnd:
call sound
 inc word[cs:displayflag]
 cmp word[cs:displayflag], 4
 jne  nextstage
 jmp win
;================================================================;
;================================================================; 
start:  
call clrscr1
call displaystart
call sound
;-------------------------;
mov ah,0
int 0x16

cmp al,'i'
je print_instructions

cmp al,'I'
jne continue1

print_instructions:
call clrscr1
call Instructions
;-------------------------;
mov ah,0
int 0x16

jmp start

continue1:
call clrscr
call print_below
call initialize
call GenerateFruit

;--------------------------;
 xor  ax, ax               
 mov  es, ax             ; point es to IVT base               
 mov  ax, [es:9*4]              
 mov  [oldkb], ax        ; save offset of old routine              
 mov  ax, [es:9*4+2]              
 mov  [oldkb+2], ax      ; save segment of old routine             
 cli                     ; disable interrupts              
 mov  word [es:9*4], kbisr ; store offset at n*4              
 mov  [es:9*4+2], cs     ; store segment at n*4+2              
 mov  word [es:8*4], timer ; store offset at n*4              
 mov  [es:8*4+2], cs     ; store segment at n*4+              
 sti                     ; enable interrupts 
 mov  dx, start          ; end of resident portion              
 add  dx, 15             ; round up to next para               
 mov  cl, 4               
 shr  dx, cl ; number of paras               
jmp $
UNhook:
mov ax,[cs:oldkb]
mov bx,[cs:oldkb+2]
cli
mov word[es:9*4],ax
mov word[es:9*4+2],bx
sti
 mov  ax, 0x3100   
 int 21h
  
 win:
	call clrscr1
	mov si,0
	mov di,1988
	mov cx,[cs:msglenforwin]
	push 0xb800
	pop es
	mov ah,0x30
	EndLoop1:
	mov al,[cs:GameWinMsg+si]
	stosw
	add si,1
	loop EndLoop1
	rep movsw
	mov si,0
	mov di,2298
	mov cx,[cs:fslength]
	push 0xb800
	pop es
	mov ah,0x30
	EndLoop2:
	mov al,[cs:finalscore+si]
	stosw
	add si,1
	loop EndLoop2
	rep movsw
	push 0x30
	push 2342
	push word[cs:score]
	call printnum
	call delay
	mov word[cs:displayflag], 0
	jmp UNhook
lose:
 mov word[cs:speedOfSnake], 16
 mov word[cs:ticker], 0
 mov word[cs:clocktick], 0
 mov word[cs:seconds], 0
 mov word[cs:tickcount],1456
 mov word[cs:timeprint], 240
;dec word[cs:lives]
;inc word[cs:lives]
cmp word[cs:lives],0
je GameEnd
call clrscr
call print_below
call initialize
call GenerateFruit
 jmp skipall
 sound:
 	soundloop:

			; send DSP Command 10h
			mov dx, 22ch
			mov al, 10h
			out dx, al

			; send byte audio sample
			mov si, [sound_index]
			mov al, [sound_data + si]
			out dx, al
			mov cx,50
			sound_delay:
			nop
			loop sound_delay

			inc word [sound_index]
			cmp word [sound_index], 44320 
	jb soundloop
	mov word[sound_index],0
ret	