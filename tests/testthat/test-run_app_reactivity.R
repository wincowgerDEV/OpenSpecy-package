test_that("the announce-priority Run observer cannot disturb reset/canonical/default ordering", {
  missing <- .openspecy_app_packages()[
    !vapply(.openspecy_app_packages(), requireNamespace, logical(1),
            quietly = TRUE)
  ]
  skip_if(length(missing), paste(
    "Missing Shiny app packages:", paste(missing, collapse = ", ")
  ))

  server_source <- paste(
    readLines(file.path(run_app(test_mode = TRUE), "server.R"), warn = FALSE),
    collapse = "\n"
  )
  extract_priority <- function(name) {
    pattern <- paste0(name, " <- ([0-9]+)L")
    match <- regmatches(server_source, regexpr(pattern, server_source))
    as.integer(sub(pattern, "\\1", match))
  }
  priority_announce <- extract_priority("RUN_GATE_PRIORITY_ANNOUNCE")
  priority_reset <- extract_priority("RUN_GATE_PRIORITY_RESET")
  priority_canonical <- extract_priority("RUN_GATE_PRIORITY_CANONICAL")
  priority_default <- extract_priority("RUN_GATE_PRIORITY_DEFAULT")

  # The regression this guards against: an earlier version of this app let a
  # DEFAULT-priority consumer read state a CANONICAL-priority observer was
  # responsible for populating first, before priority= made the order
  # explicit (spec 014). The new ANNOUNCE-priority observer (added to show
  # busy feedback immediately on click) must not reopen that class of bug.
  # This reproduces the shape with a minimal reactive graph, using the
  # actual priority values from server.R, plus the new observer, and proves
  # both: (1) execution order is exactly announce -> reset -> canonical ->
  # default, and (2) the default-priority consumer always sees the
  # canonical-priority producer's value, never a stale/missing one.
  expect_true(priority_announce > priority_reset)
  expect_true(priority_reset > priority_canonical)
  expect_true(priority_canonical > priority_default)

  # The announce observer's real body must stay message-only: it must not
  # reference any reactiveVal/state other observers depend on.
  announce_body <- regmatches(
    server_source,
    regexpr(
      "observeEvent\\(input\\$run_analysis, \\{[^}]*\\}, priority = RUN_GATE_PRIORITY_ANNOUNCE\\)",
      server_source
    )
  )
  expect_length(announce_body, 1L)
  expect_match(announce_body, "analysis_phase(", fixed = TRUE)
  for(forbidden in c("canonical_state", "analysis_dirty(", "analysis_needs_reset(",
                     "<<-", "reactiveVal(")) {
    expect_false(grepl(forbidden, announce_body, fixed = TRUE))
  }

  server <- function(input, output, session) {
    order_log <- character()
    canonical_value <- NULL
    consumer_saw <- character()

    shiny::observeEvent(input$run_analysis, {
      order_log <<- c(order_log, "announce")
    }, priority = priority_announce)

    shiny::observeEvent(input$run_analysis, {
      order_log <<- c(order_log, "reset")
    }, priority = priority_reset)

    shiny::observeEvent(input$run_analysis, {
      order_log <<- c(order_log, "canonical")
      canonical_value <<- "populated"
    }, priority = priority_canonical)

    shiny::observeEvent(input$run_analysis, {
      order_log <<- c(order_log, "default")
      consumer_saw <<- c(
        consumer_saw, if(is.null(canonical_value)) "STALE" else canonical_value
      )
    }, priority = priority_default)

    session$userData$get_order <- function() order_log
    session$userData$get_consumer_saw <- function() consumer_saw
  }

  shiny::testServer(server, {
    session$setInputs(run_analysis = 1)
    expect_identical(
      session$userData$get_order(), c("announce", "reset", "canonical", "default")
    )
    expect_identical(session$userData$get_consumer_saw(), "populated")

    session$setInputs(run_analysis = 2)
    expect_identical(
      session$userData$get_order(),
      c("announce", "reset", "canonical", "default",
        "announce", "reset", "canonical", "default")
    )
    expect_identical(
      session$userData$get_consumer_saw(), c("populated", "populated")
    )
  })
})
