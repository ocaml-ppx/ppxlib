INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

# Default rule
default:
	jbuilder build --dev --auto-promote @cinaps @install

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

clean:
	rm -rf _build

.PHONY: default install uninstall reinstall clean
