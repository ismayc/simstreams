# simstreams 0.1.0

* The test suite covers every line, and the `test-coverage` workflow fails
  below 100%. The per-process steps of `sim_template()` are the internal
  functions `init_worker()`, `attach_package()`, and `run_worker()`, so they
  can be tested in the current process as well as on the cluster.
* First release. `sim_template()`, `sim_template_serial()`, `make_set_up()`,
  `split_reps()`, `set_worker_rng()`, `bind_results()`, `benchmark_cores()`,
  and the book's example workers `pi_worker()`, `iqr_worker()`, and
  `regression_worker()`.
* The test suite pins the Chapter 3 results printed in the book
  (`3.141554` for pi and the twenty Cauchy scale estimates) and checks the
  package workers against the book's `sim_worker.R` listings.
