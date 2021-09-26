bin_PROGRAMS = debug_adapter
debug_adapter_SOURCES = $(addprefix $(srcdir)/debug_adapter/, main.pl server.pl tracer.pl protocol.pl client.pl)

debug_adapter$(EXEEXT):
	@$(PROLOG) -o $@ -c $(debug_adapter_SOURCES) -O
