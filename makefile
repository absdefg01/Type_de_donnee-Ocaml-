# AT91EB40A board with angel / Gdb Simulator makefile

PROG		= ex3

# Tools
#
TARGET_ALIAS	= arm-elf
CC 		= $(TARGET_ALIAS)-gcc
AS 		= $(TARGET_ALIAS)-as
AR 		= $(TARGET_ALIAS)-ar
LD 		= $(TARGET_ALIAS)-ld
COPY 		= $(TARGET_ALIAS)-objcopy
DUMP 		= $(TARGET_ALIAS)-objdump
RANLIB 		= $(TARGET_ALIAS)-ranlib

# Tools flags
#
ASFLAGS 	= -v -mcpu=arm7tdmi -gstabs
LDFLAGS		= -v -Ttext=0x2000 -nostartfiles
DUMP_LST	= -x -w -h -t
DUMP_DIS	= -D --prefix-addresses --show-raw-insn

ASM_SRC 	= $(PROG).s

OBJS16		= $(ASM_SRC:.s=.o16)
OBJS32		= $(ASM_SRC:.s=.o32) 

%.o32: %.s makefile
	$(AS)  $(ASFLAGS) $*.s -o $*.o32

%.o16: %.s makefile
	$(AS) -mthumb-interwork $(ASFLAGS) $*.s -o $*.o16

arm: $(OBJS32) makefile
	$(LD) $(LDFLAGS) -o $(PROG).elf $(OBJS32)
	$(DUMP) $(DUMP_LST) $(PROG).elf >$(PROG).lst
	$(DUMP) $(DUMP_DIS) $(PROG).elf >$(PROG).dis

thumb: $(OBJS16) makefile
	$(LD) $(LDFLAGS) -o $(PROG)-thumb.elf $(OBJS16)
	$(DUMP) $(DUMP_LST) $(PROG)-thumb.elf >$(PROG)-thumb.lst
	$(DUMP) $(DUMP_DIS) $(PROG)-thumb.elf >$(PROG)-thumb.dis

clean:
	${RM} a.out core *.o16 *.o32 $(PROG)*.elf $(PROG)*.lst $(PROG)*.dis
