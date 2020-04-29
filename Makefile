ROOT_DIR=$(shell pwd)
EXAMPLES_PA=-pa _build/default/lib/ekcp/ebin/ -pa examples/
demo_port=
demo_ip=
demo_num=
ekcp_conf=

compile:
	rebar3 compile

shell:
	rebar3 shell

compile-examples:
	erlc -W -o $(ROOT_DIR)/examples/ $(ROOT_DIR)/examples/*.erl

tcp-server: compile compile-examples
	erl $(EXAMPLES_PA) -s tcp_demo start server -demo_port $(demo_port)

tcp-client: compile compile-examples
	erl $(EXAMPLES_PA) -s tcp_demo start client -demo_port $(demo_port) -demo_ip $(demo_ip) -demo_num $(demo_num)

kcp-server: compile compile-examples
	erl $(EXAMPLES_PA) -s ekcp_demo start server -demo_port $(demo_port) -ekcp_conf $(ekcp_conf)

kcp-client: compile compile-examples
	erl $(EXAMPLES_PA) -s ekcp_demo start client -demo_port $(demo_port) -demo_ip $(demo_ip) -demo_num $(demo_num) -ekcp_conf $(ekcp_conf)

.PHONY: compile
