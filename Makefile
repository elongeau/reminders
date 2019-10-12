copy-bin:
	@cp "$$(stack path --local-install-root --system-ghc --allow-different-user)/bin/reminder-exe" reminder-exe