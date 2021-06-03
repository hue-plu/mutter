run-mutter:
  # FIXME: postgresql ssl setting
	PGSSLMODE=disable scripts/psqldef -U postgres mutter < schema.sql
	cd server && stack run mutter-exe

run-study:
  # FIXME: postgresql ssl setting
	PGSSLMODE=disable scripts/psqldef -U postgres mutter < schema.sql
	cd server && stack run study-exe

.PHONY: run-mutter
