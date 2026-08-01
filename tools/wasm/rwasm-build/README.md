# Pinned r-wasm build driver

`Dockerfile` and `code.R` reproduce `r-wasm/actions/build-rwasm` at commit
`0f8493df20b6b47d3621f16be81218926a09dad1`. The default webR container is
pinned to the digest used by OpenSpecy's successful `da6fd42` wasm build.

The copy is kept here so the pre-push rehearsal and GitHub Actions execute the
same reviewable build driver instead of independently following floating tags.
The upstream files are distributed under the included MIT license.
