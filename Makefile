ROOT_TEST=_build/test/lib

ifndef suite
	SUITE_EXEC=
else
	SUITE_EXEC=-suite $(suite)_SUITE
endif

all: compile

compile:
	rebar3 compile

clean:
	rebar3 clean

eunit:
	rebar3 eunit

docs:
	rebar3 edoc

ct:
	mkdir -p log
	rebar3 ct --compile_only
	ct_run  -no_auto_compile \
			-name test@127.0.0.1 \
			-cover test/cover.conf \
			-logdir log \
			-dir $(ROOT_TEST)/unsplit/test $(SUITE_EXEC) \
			-ct_config test/test.config \
			-pa $(ROOT_TEST)/*/ebin \
			-erl_args \
			-kernel prevent_overlapping_partitions false \
			-config test/sys.config

