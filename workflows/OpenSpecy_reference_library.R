# Rebuild the official Open Specy reference-library artifacts.
#
# build_lib() owns source discovery, curated metadata joins, class completion,
# pruning, one-off filters, medoid/model creation, resumable checkpoints,
# complete legacy comparisons, assessments, and versioned output promotion.
# Override its environment-aware path defaults with OPENSPECY_LIBRARY_DATA,
# OPENSPECY_SOURCE_FILE, OPENSPECY_PROCESSED_DIR, or
# OPENSPECY_LIBRARY_OUTPUT when the official files are elsewhere.

library(OpenSpecy)

reference_library_build <- build_lib()
