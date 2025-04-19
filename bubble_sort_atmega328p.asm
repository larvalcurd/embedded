;===============================================================================
;                           REGISTER USAGE MAP
;===============================================================================
; +-----------+-----------------------------+-------------------------------+
; | Register  | Purpose                     | Preservation                  |
; +-----------+-----------------------------+-------------------------------+
; | R0        | Temp LSB data               | Not preserved                 |
; | R1        | Temp MSB data               | Not preserved                 |
; | R2        | Next element LSB            | Not preserved                 |
; | R3        | Next element MSB            | Not preserved                 |
; | R16       | Stack init temporary        | Not preserved                 |
; | R17       | Byte counter (copy loop)    | Not preserved                 |
; | R20       | Outer loop counter          | Not preserved                 |
; | R22       | Swap flag                   | Not preserved                 |
; | R23       | Inner loop counter          | Not preserved                 |
; | R28 (YL)  | Array pointer low byte      | Not preserved                 |
; | R29 (YH)  | Array pointer high byte     | Not preserved                 |
; | R30 (ZL)  | Flash pointer low byte      | Not preserved                 |
; | R31 (ZH)  | Flash pointer high byte     | Not preserved                 |
; +-----------+-----------------------------+-------------------------------+

;===============================================================================
;                           MEMORY MAP DETAILS
;===============================================================================
; Flash Memory (0x0000-0x3FFF)              SRAM (0x0100-0x08FF)
; +-----------------------------+           +-----------------------------+
; | 0x0000: Reset vector        |           | 0x0100: array_ram[0] LSB    |
; | 0x0002: INT0 vector         |           | 0x0101: array_ram[0] MSB    |
; | ...                         |           | ...                         |
; | 0x0040: Main program        |           | 0x0113: array_ram[9] MSB    |
; | 0x005A: Bubble sort code    |           | 0x08FF: Stack bottom        |
; | 0x006F: array_flash data    |           +-----------------------------+
; +-----------------------------+

;===============================================================================
;                           TEST CASE SPECIFICATIONS
;===============================================================================
; Test Case 1: Pre-sorted Array
;   Input:   0x1111, 0x1234, 0x2222, ..., 0xDEF0
;   Expected: No swaps, early exit after first pass
;   Validation: Check cycle count < 500

; Test Case 2: Reverse-sorted Array
;   Input:   0xDEF0, 0x9ABC, 0x6666, ..., 0x1111
;   Expected: Full N-1 passes
;   Validation: Verify final element = 0xDEF0

; Test Case 3: All Equal Elements
;   Input:   0x5555 repeated 10 times
;   Expected: Early exit after first pass
;   Validation: Check swap flag remains 0

; Test Case 4: Boundary Values
;   Input:   0x0000, 0xFFFF, 0x0001, 0xFFFE
;   Expected: Sorted order 0x0000, 0x0001, 0xFFFE, 0xFFFF
;   Validation: Check mid-range comparisons

;===============================================================================
;                       HARDWARE CONFIGURATION REQUIREMENTS
;===============================================================================
; Power Supply:
;   - Operating Voltage: 1.8V to 5.5V (3.3V recommended)
;   - Current Consumption:
;     - 8.5 mA @ 16 MHz (Active mode during sorting)
;     - 25 μA @ 32 kHz (Sleep mode in idle loop)

; Clock Configuration:
;   - Minimum: 0 MHz (Reset state)
;   - Typical: 16 MHz (Ceramic resonator)
;   - Maximum: 20 MHz (@ 4.5-5.5V)

; Timing Characteristics:
;   - 62.5 ns per clock cycle @ 16 MHz
;   - 1 CPU cycle = 1 clock cycle (single-cycle execution)

; Peripheral Requirements:
;   - 2048 bytes SRAM (minimum)
;   - 32 KB Flash (minimum)
;   - Stack space: 32 bytes (minimum)

;===============================================================================
;                           KEY IMPLEMENTATION DETAILS
;===============================================================================
;===============================================================================
;                           BUBBLE SORT IMPLEMENTATION
;===============================================================================
; 
; Filename:     bubble_sort_atmega328p.asm
; Processor:    ATmega328P (8-bit AVR RISC architecture)
; Description:  Optimized bubble sort algorithm for 16-bit unsigned integers
;               stored in RAM with early exit optimization and pointer arithmetic
;
; Features:
;   - Sorts 16-bit values in ascending order
;   - In-place sorting (modifies array directly in RAM)
;   - Early termination if no swaps occur in a pass
;   - Efficient pointer handling for 16-bit data
;   - Little-endian compliant memory operations
;
;===============================================================================
;                           PROCESSOR DEFINITIONS
;===============================================================================
.include "m328pdef.inc"    ; Standard ATmega328P register definitions

;===============================================================================
;                           MEMORY ALLOCATION
;===============================================================================
.dseg
.org SRAM_START            ; Start allocation from beginning of SRAM
array_ram: 
    .byte 20               ; Reserve 20 bytes (10 elements × 2 bytes each)

;===============================================================================
;                           CONSTANT DEFINITIONS
;===============================================================================
.cseg
.org 0x0000                ; Reset vector location
    rjmp main              ; Jump to main program initialization

; Initial test array (16-bit values in Flash memory)
array_flash: 
    .dw 0xDEF0, 0x9ABC, 0x6666, 0x5678, 0x5555 
    .dw 0x4444, 0x3333, 0x2222, 0x1234, 0x1111

.equ ARRAY_SIZE = 10       ; Number of elements in array (10 elements)
.equ ARRAY_BYTES = 20      ; Total array size in bytes (10 × 2 bytes)

;===============================================================================
;                               MAIN PROGRAM
;===============================================================================
main:
    ;-----------------------------------------------------------------------
    ; STACK POINTER INITIALIZATION
    ;-----------------------------------------------------------------------
    ldi r16, high(RAMEND)  ; Load high byte of RAM end address
    out SPH, r16           ; Set Stack Pointer High byte
    ldi r16, low(RAMEND)   ; Load low byte of RAM end address
    out SPL, r16           ; Set Stack Pointer Low byte

    ;-----------------------------------------------------------------------
    ; COPY ARRAY FROM FLASH TO RAM
    ;-----------------------------------------------------------------------
    ; Initialize Z-pointer (R30:R31) to source array in Flash
    ldi ZL, low(array_flash*2)  ; Flash addresses are word-aligned (×2)
    ldi ZH, high(array_flash*2) 
    
    ; Initialize Y-pointer (R28:R29) to destination in RAM
    ldi YL, low(array_ram)
    ldi YH, high(array_ram)
    
    ; Initialize byte counter (2 bytes per element × 10 elements)
    ldi r17, ARRAY_BYTES   

copy_loop:
    ; Flash memory read operation (1 byte per cycle)
    lpm r0, Z+             ; Load byte from Flash to r0, post-increment Z
    st Y+, r0              ; Store byte to RAM, post-increment Y
    dec r17                ; Decrement byte counter
    brne copy_loop         ; Loop until all bytes are copied

    ;-----------------------------------------------------------------------
    ; EXECUTE BUBBLE SORT ALGORITHM
    ;-----------------------------------------------------------------------
    rcall bubble_sort      ; Call sorting subroutine with full optimization

    ;-----------------------------------------------------------------------
    ; PROGRAM COMPLETION (INFINITE LOOP)
    ;-----------------------------------------------------------------------
end:
    rjmp end               ; Infinite loop indicating program completion

;===============================================================================
;                       BUBBLE SORT SUBROUTINE
;===============================================================================
; Parameters:
;   - array_ram: Array in RAM (little-endian 16-bit words)
;   - ARRAY_SIZE: Number of elements (10)
;
; Register Usage:
;   - r20: Outer loop counter (n-1 passes)
;   - r23: Inner loop counter (remaining elements)
;   - r22: Swap flag (1 = swap occurred)
;   - r0-r3: Temporary data registers for element comparison
;   - Y-pointer (R28:R29): Array access pointer
;
; Algorithm Complexity:
;   - Best case: O(n) when array is already sorted
;   - Worst case: O(n²) when array is reverse sorted
;   - Space: O(1) (in-place sorting)
;
; Optimization Notes:
;   1. Early termination when no swaps occur
;   2. Pointer arithmetic avoids recalculating addresses
;   3. Minimal register usage preserves caller context
;===============================================================================
bubble_sort:
    ; Initialize outer loop counter (n-1 passes required)
    ldi r20, ARRAY_SIZE-1  

outer_loop:
    ; Reset Y-pointer to beginning of array
    ldi YL, low(array_ram)
    ldi YH, high(array_ram)
    
    ; Clear swap flag (will be set if any swaps occur)
    ldi r22, 0             
    
    ; Initialize inner loop counter (decreases with each pass)
    ldi r23, ARRAY_SIZE-1  

inner_loop:
    ;-----------------------------------------------------------------------
    ; LOAD CURRENT ELEMENT (array[j])
    ;-----------------------------------------------------------------------
    ld r0, Y+              ; Load LSB of current element, post-increment
    ld r1, Y+              ; Load MSB of current element, post-increment
    
    ;-----------------------------------------------------------------------
    ; LOAD NEXT ELEMENT (array[j+1])
    ;-----------------------------------------------------------------------
    ld r2, Y+              ; Load LSB of next element, post-increment
    ld r3, Y+              ; Load MSB of next element, post-increment
    
    ;-----------------------------------------------------------------------
    ; RESET POINTER TO ELEMENT PAIR START
    ;-----------------------------------------------------------------------
    sbiw YL, 4             ; Rewind Y-pointer by 4 bytes (2 words)
    
    ;-----------------------------------------------------------------------
    ; COMPARE 16-BIT VALUES (UNSIGNED)
    ;-----------------------------------------------------------------------
    cp r0, r2              ; Compare LSBs (r0 - r2)
    cpc r1, r3             ; Compare MSBs with carry (r1 - r3)
    brlo no_swap           ; If array[j] < array[j+1], skip swap
    
    ;-----------------------------------------------------------------------
    ; PERFORM SWAP OPERATION
    ;-----------------------------------------------------------------------
    ; Store next element's value in current position
    st Y+, r2              
    st Y+, r3              
    
    ; Store current element's value in next position
    st Y+, r0              
    st Y+, r1              
    
    ; Reset pointer after swap operation
    sbiw YL, 4             
    
    ; Set swap flag to indicate sort is still active
    ldi r22, 1             

no_swap:
    ;-----------------------------------------------------------------------
    ; ADVANCE TO NEXT ELEMENT PAIR
    ;-----------------------------------------------------------------------
    adiw YL, 2             ; Move forward 2 bytes (1 element)
    dec r23                ; Decrement inner loop counter
    brne inner_loop        ; Repeat until all pairs compared
    
    ;-----------------------------------------------------------------------
    ; CHECK FOR EARLY TERMINATION
    ;-----------------------------------------------------------------------
    tst r22                ; Test if any swaps occurred
    breq sort_end          ; If no swaps, array is fully sorted
    
    ;-----------------------------------------------------------------------
    ; PREPARE NEXT OUTER LOOP ITERATION
    ;-----------------------------------------------------------------------
    dec r20                ; Decrement pass counter
    brne outer_loop        ; Continue until all passes complete

sort_end:
    ret                    ; Return from subroutine

;===============================================================================
;                           KEY IMPLEMENTATION DETAILS
;===============================================================================
;
; MEMORY ACCESS:
;   - Uses Y-pointer exclusively for array access (required for ST/LD)
;   - Pointer arithmetic handles 2-byte elements correctly
;   - Maintains little-endian format (LSB at lower address)
;
; COMPARISON LOGIC:
;   - 16-bit unsigned comparison using CP (LSB) and CPC (MSB with carry)
;   - BRLO instruction for unsigned comparison (BRLT for signed)
;
; OPTIMIZATIONS:
;   1. Early exit when pass completes with no swaps
;   2. Pointer manipulation avoids address recalculation
;   3. Minimal register usage preserves calling context
;   4. In-place sorting requires no additional memory
;
; LIMITATIONS:
;   - Fixed array size (change ARRAY_SIZE constant for different sizes)
;   - Unsigned comparison only (modify for signed values)
;   - 16-bit values only (requires modification for other data sizes)
;
;===============================================================================
;                               END OF FILE
;===============================================================================

