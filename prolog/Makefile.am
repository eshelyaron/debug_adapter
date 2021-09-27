bin_PROGRAMS = swipl_debug_adapter
swipl_debug_adapter_SOURCES = $(addprefix $(srcdir)/debug_adapter/, main.pl server.pl tracer.pl protocol.pl client.pl ../../pack.pl)

swipl_debug_adapter$(EXEEXT):
	@$(PROLOG) -o $@ -c $(swipl_debug_adapter_SOURCES) -O
