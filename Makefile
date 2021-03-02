REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

UTILS_PATH := build_utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := bouncer_client_erlang

BUILD_IMAGE_NAME := build-erlang
BUILD_IMAGE_TAG := 1aa9b46b343bcf3bf0d4359ceca1a7ab6d577699

CALL_ANYWHERE := \
	submodules \
	all compile xref lint dialyze test cover \
	clean distclean \
	check_format format

CALL_W_CONTAINER := $(CALL_ANYWHERE)

.PHONY: $(CALL_W_CONTAINER) all

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

compile:
	$(REBAR) compile

xref:
	$(REBAR) xref

lint:
	elvis rock

check_format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w

dialyze:
	$(REBAR) dialyzer

clean:
	$(REBAR) cover -r
	$(REBAR) clean

distclean:
	$(REBAR) clean
	rm -rf _build

cover:
	$(REBAR) cover

# CALL_W_CONTAINER
test:
	$(REBAR) ct
