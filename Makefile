PROJECT = enq
PROJECT_DESCRIPTION = Native implemented queues for Erlang
PROJECT_VERSION = 1.0.0

C_SRC_OUTPUT = priv/enq_nif
CFLAGS += --std=gnu99 -Wall -O3
LDFLAGS += -lrt

include erlang.mk
