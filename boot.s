; Title: Starting point of OS

; This project is licensed under the GPL.

; routine: boot.s -- Kernel start location. Also defines multiboot header.
;
; Based on Bran's kernel development tutorial file start.asm
;

MBOOT_PAGE_ALIGN    equ 1<<0    ; Load kernel and modules on a page boundary
MBOOT_MEM_INFO      equ 1<<1    ; Provide your kernel with memory info
MBOOT_HEADER_MAGIC  equ 0x1BADB002 ; Multiboot Magic value
; NOTE that We do not use MBOOT_AOUT_KLUDGE. It means that GRUB does not
; pass us a symbol table.
MBOOT_HEADER_FLAGS  equ MBOOT_PAGE_ALIGN | MBOOT_MEM_INFO
MBOOT_CHECKSUM      equ -(MBOOT_HEADER_MAGIC + MBOOT_HEADER_FLAGS)


[BITS 32]                       ; All instructions should be 32-bit.
global MBOOT_HEADER_FLAGS
global mboot                    ; Make 'mboot' accessible from C.
extern code                     ; Start of the '.text' section.
extern bss                      ; Start of the .bss section.
extern end                      ; End of the last loadable section.

; type: mboot
;   Structure readed for grub to initialize the kernel.
mboot:
        dd  MBOOT_HEADER_MAGIC  ; GRUB will search for this value on each
                                ; 4-byte boundary in your kernel file
        dd  MBOOT_HEADER_FLAGS  ; How GRUB should load your file / settings
        dd  MBOOT_CHECKSUM      ; To ensure the above values are correct

        dd  mboot               ; Location of this descriptor
        dd  code                ; Start of kernel '.text' (code) section.
        dd  bss                 ; End of kernel '.data' section.
        dd  end                 ; End of kernel.
        dd  start               ; Kernel entry point (initial EIP).

global start                    ; Grub entry point.
extern main                     ; This is the entry point of our C code
extern gdt_flush
global start
global module
; function: start
;   Entry point for grub.
start:
        mov dword [module],ebx             ; Load multiboot header location
        push ebx
        cli                     ; Disable interrupts.
        call gdt_flush          ; Initialize GDT
        call main               ; call our main() function.
        jmp $                   ; Enter an infinite loop, to stop the processor
                                ; executing whatever rubbish is in the memory
                                ; after our kernel! 
module: dd 0

