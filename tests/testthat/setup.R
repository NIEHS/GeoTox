# Define DuckDB storage locations to prevent information messages from being
# printed when a connection is opened. See `?duckdb::duckdb_storage` for details.
# duckdb.home for DuckDB v1.5.5, duckdb.extension_directory for earlier versions.
tempdir <- withr::local_tempdir()
op <- options(duckdb.home = tempdir, duckdb.extension_directory = tempdir)

withr::defer(options(op), teardown_env())
