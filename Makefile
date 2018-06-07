ghcid:
	ghcid \
		--command "stack ghci work-time:lib work-time:exe:work-time \
			--ghci-options=-fno-code"

.PHONY: ghcid