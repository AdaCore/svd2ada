VPATH = IPs

all: cm4.svd cm4f.svd cm7.svd

cm4.svd: nvic.part scb.part systick.part mpu.part debug.part dwt_m4.part
	@echo "creating $@ from $+"
	@cat support/preamble > $@
	@for f in $+; do \
	  cat $$f >> $@; \
	done
	@cat support/trailing >> $@

cm4f.svd: nvic.part scb.part systick.part fpu.part mpu.part debug.part dwt_m4.part
	@echo "creating $@ from $+"
	@cat support/preamble > $@
	@for f in $+; do \
	  cat $$f >> $@; \
	done
	@cat support/trailing >> $@

cm7.svd: nvic.part scb.part systick.part fpu.part mpu.part pf.part cache.part debug.part dwt_m7.part
	@echo "creating $@ from $+"
	@cat support/preamble > $@
	@for f in $+; do \
	  cat $$f >> $@; \
	done
	@cat support/trailing >> $@

clean:
	rm -f *.svd
