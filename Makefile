.PHONY: default
default:
	@echo "no arguments -- call 'make hwXX' for some homework number like 'hw01'"

.stack-work:
	stack setup

##############
# RUNNING HW #
##############

.PHONY: eval
eval:
	@echo -------
	@echo RUNNING $(EVAL_PATH)
	@echo -------
	stack ghci --ghci-options '-e $(EVAL_PATH).main'

.PHONY: hw01
hw01: ; make eval EVAL_PATH=HW01

.PHONY: hw02
hw02: ; make eval EVAL_PATH=HW02

.PHONY: hw03
hw03: ; make eval EVAL_PATH=HW03

.PHONY: hw04
hw04: ; make eval EVAL_PATH=HW04

.PHONY: hw05
hw05: ; make eval EVAL_PATH=HW05

.PHONY: hw06
hw06: ; make eval EVAL_PATH=HW06

.PHONY: hw07
hw07: ; make eval EVAL_PATH=HW07

.PHONY: hw08
hw08: ; make eval EVAL_PATH=HW08

.PHONY: hw09
hw09: ; make eval EVAL_PATH=HW09

.PHONY: hw10
hw10: ; make eval EVAL_PATH=HW10

.PHONY: live
live: ; make eval EVAL_PATH=Live


#################
# GHCI HOMEWORK #
#################

.PHONY: interact
interact:
	@echo -------
	@echo LOADING $(EVAL_PATH)
	@echo -------
	stack ghci src/$(EVAL_PATH).hs

.PHONY: hw01-i
hw01-i: ; make interact EVAL_PATH=HW01

.PHONY: hw02-i
hw02-i: ; make interact EVAL_PATH=HW02

.PHONY: hw03-i
hw03-i: ; make interact EVAL_PATH=HW03

.PHONY: hw04-i
hw04-i: ; make interact EVAL_PATH=HW04

.PHONY: hw05-i
hw05-i: ; make interact EVAL_PATH=HW05

.PHONY: hw06-i
hw06-i: ; make interact EVAL_PATH=HW06

.PHONY: hw07-i
hw07-i: ; make interact EVAL_PATH=HW07

.PHONY: hw08-i
hw08-i: ; make interact EVAL_PATH=HW08

.PHONY: hw09-i
hw09-i: ; make interact EVAL_PATH=HW09

.PHONY: hw10-i
hw10-i: ; make interact EVAL_PATH=HW10

.PHONY: live-i
live-i: ; make interact EVAL_PATH=Live

##################
# GHCID HOMEWORK #
##################

.PHONY: dev
dev: .stack-work
	@echo -----------
	@echo INTERACTIVE $(EVAL_PATH)
	@echo -----------
	ghcid --test=$(EVAL_PATH).main --warnings

.PHONY: hw01-dev
hw01-dev: ; make dev EVAL_PATH=HW01

.PHONY: hw02-dev
hw02-dev: ; make dev EVAL_PATH=HW02

.PHONY: hw03-dev
hw03-dev: ; make dev EVAL_PATH=HW03

.PHONY: hw04-dev
hw04-dev: ; make dev EVAL_PATH=HW04

.PHONY: hw05-dev
hw05-dev: ; make dev EVAL_PATH=HW05

.PHONY: hw06-dev
hw06-dev: ; make dev EVAL_PATH=HW06

.PHONY: hw07-dev
hw07-dev: ; make dev EVAL_PATH=HW07

.PHONY: hw08-dev
hw08-dev: ; make dev EVAL_PATH=HW08

.PHONY: hw09-dev
hw09-dev: ; make dev EVAL_PATH=HW09

.PHONY: hw10-dev
hw10-dev: ; make dev EVAL_PATH=HW10

.PHONY: live-dev
live-dev: ; make dev EVAL_PATH=Live

###########
# PARSING #
###########

.PHONY: pl1
pl1: ; stack ghci --ghci-options '-e "Lang.Parse.action [\"l1\",\"$E\"]"'

.PHONY: pl2
pl2: ; stack ghci --ghci-options '-e "Lang.Parse.action [\"l2\",\"$E\"]"'

##################
# STAFF USE ONLY #
##################

.PHONY: sl01
sl01: ; make eval EVAL_PATH=Solutions.SL01

.PHONY: sl02
sl02: ; make eval EVAL_PATH=Solutions.SL02

.PHONY: sl03
sl03: ; make eval EVAL_PATH=Solutions.SL03

.PHONY: sl04
sl04: ; make eval EVAL_PATH=Solutions.SL04

.PHONY: sl05
sl05: ; make eval EVAL_PATH=Solutions.SL05

.PHONY: sl06
sl06: ; make eval EVAL_PATH=Solutions.SL06

.PHONY: sl07
sl07: ; make eval EVAL_PATH=Solutions.SL07

.PHONY: sl08
sl08: ; make eval EVAL_PATH=Solutions.SL08

.PHONY: sl09
sl09: ; make eval EVAL_PATH=Solutions.SL09

.PHONY: sl10
sl10: ; make eval EVAL_PATH=Solutions.SL10

RELEASE_FILES := \
	Makefile package.yaml README.md stack.yaml \
	src/Util/Testing.hs \
	src/HW01.hs \
	src/HW02.hs src/Lang/Trees.hs src/Lang/Trees/Data.hs src/Lang/Trees/Util.hs $(wildcard tests/hw02/**/*)

RELEASE_DIR := cs225-hw-2020-01

.PHONY: prepare
prepare:
	cd $(RELEASE_DIR) && git clean -fd
	$(foreach f,$(RELEASE_FILES), \
		mkdir -p $(dir $(RELEASE_DIR)/$f) ; \
		cp $f $(RELEASE_DIR)/$f ; \
	)
	cd $(RELEASE_DIR) && git status

.PHONY: release
release:
	make prepare
	cd cs225-hw-2020-01 && git add . && git commit -m "update" && git push
