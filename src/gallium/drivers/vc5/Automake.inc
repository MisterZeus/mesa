if HAVE_GALLIUM_VC5

TARGET_DRIVERS += vc5
TARGET_CPPFLAGS += -DGALLIUM_VC5
TARGET_LIB_DEPS += \
	$(top_builddir)/src/gallium/winsys/vc5/drm/libvc5drm.la \
	$(top_builddir)/src/gallium/drivers/vc5/libvc5.la

endif
