library(testthat)
library(shiny)
library(data.table)

APP <- test_path("..", "..", "shiny")

test_that("global search returns filtered, clickable results", {
  shiny::testServer(APP, {
    session$setInputs(global_search = "Ifng", search_category = "all", search_limit = 20)
    x <- search_results()
    expect_s3_class(x, "data.table")
    expect_true(all(c("Category", "Name", "Identifier", "Detail", "Action") %in% names(x)))
    expect_true(nrow(x) >= 1)
    expect_true("Gene" %in% x$Category)
    expect_match(x[Category == "Gene", Action][1], "Shiny.setInputValue")

    session$setInputs(search_category = "disease")
    expect_true(all(search_results()$Category == "Disease"))
    session$setInputs(global_search = "")
    expect_equal(nrow(search_results()), 0)
  })
})

test_that("network builds nodes, detail table and toggles disease links", {
  shiny::testServer(APP, {
    session$setInputs(global_search = "Ifng", search_category = "gene", search_limit = 10)
    gid <- search_results()[Category == "Gene", Identifier][1]
    expect_true(!is.na(gid))
    session$setInputs(network_gene = gid, network_limit = 10, network_diseases = TRUE)
    nw <- network_data()
    expect_true(nrow(nw$phen) > 0)
    expect_equal(nrow(nw$phen_edges), nrow(nw$phen))
    expect_equal(nrow(nw$dis_edges), nrow(nw$dis_nodes))
    expect_true(inherits(output$p_network, "json"))
    expect_true(inherits(output$t_network, "json"))

    session$setInputs(network_diseases = FALSE)
    expect_equal(nrow(network_data()$dis_nodes), 0)
  })
})

test_that("comparison, enrichment and report tabs render", {
  shiny::testServer(APP, {
    session$setInputs(global_search = "Ifng", search_category = "gene", search_limit = 10)
    g1 <- search_results()[Category == "Gene", Identifier][1]
    session$setInputs(global_search = "Cog2", search_category = "gene", search_limit = 10)
    g2 <- search_results()[Category == "Gene", Identifier][1]
    expect_true(!is.na(g1) && !is.na(g2))

    session$setInputs(compare_genes = c(g1, g2), compare_level = "procedure")
    expect_true(inherits(output$p_compare, "json"))
    expect_true(inherits(output$t_compare, "json"))

    session$setInputs(enrich_gene = g1, enrich_threshold = "0.05")
    expect_true(inherits(output$p_enrich, "json"))
    expect_true(inherits(output$t_enrich, "json"))

    session$setInputs(report_gene = g1)
    r <- report_preview_data()
    expect_true(nrow(r$results) > 0)
    expect_true(nzchar(r$symbol))
    expect_false(is.null(output$report_preview))
  })
})
