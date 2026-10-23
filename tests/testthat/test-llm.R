# Which language models the module agrees to call.

test_that("only local models, or vllm at an address in tdc.trusted_llm, are used", {
  withr::local_envvar(OLLAMA_BASE_URL = "")
  withr::local_options(metacheck.llm.model = "groq/some-model", tdc.trusted_llm = NULL)
  expect_false(tdc$.tdc_local_llm())

  withr::local_options(metacheck.llm.model = "ollama/qwen3.5:4b")
  expect_true(tdc$.tdc_local_llm())

  withr::local_options(metacheck.llm.model = "vllm/some-model",
                       metacheck.llm.vllm.base_url = "https://gateway.example.tue.nl/v1")
  expect_false(tdc$.tdc_local_llm())

  withr::local_options(tdc.trusted_llm = "https://gateway.example.tue.nl/")
  expect_true(tdc$.tdc_local_llm())

  withr::local_options(tdc.trusted_llm = "https://other.example.org/")
  expect_false(tdc$.tdc_local_llm())

  withr::local_envvar(OLLAMA_BASE_URL = "http://localhost:11434")
  withr::local_options(metacheck.llm.vllm.base_url = "http://localhost:11434/v1")
  expect_true(tdc$.tdc_local_llm())

  withr::local_options(metacheck.llm.vllm.base_url = "")
  expect_false(tdc$.tdc_local_llm())
})
