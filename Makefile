PROJECT = nat
PROJECT_DESCRIPTION = implements NAT handling facilities
PROJECT_VERSION = 0.1.0

DEPS = inet_cidr inet_ext

dep_inet_cidr = git https://github.com/benoitc/inet_cidr 0.2.0
dep_inet_ext = git https://github.com/benoitc/inet_ext 0.3.2

DOC_DEPS = edown
EDOC_OPTS = {doclet, edown_doclet}, \
			{top_level_readme, {"./README.md", "http://github.com/benoitc/erlang-nat"}}


all:: deps app rel

doc: edoc

distclean:: distclean-edown

distclean-edown:
	rm -rf doc/*.md

app:: rebar.config

include erlang.mk
