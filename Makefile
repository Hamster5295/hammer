MILL = ./mill

MVN_DIR = .deps

lib:
	@$(MILL) _.publishM2Local --m2RepoPath=$(MVN_DIR)

sonatype:
	@$(MILL) _.publishSonatypeCentral

format:
	@$(MILL) _.reformat