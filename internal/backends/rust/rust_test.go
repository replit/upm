package rust

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/replit/upm/internal/api"
)

func TestCrateInfo(t *testing.T) {
	info := RustBackend.Info(api.PkgName("serde"))
	// We don't want to check too many fields since they can be changed externally and break this test.
	require.Equal(t, "serde", info.Name)
}

func TestCrateSearch(t *testing.T) {
	results := RustBackend.Search("serde")
	// We don't want to check the results as they may change externally and break this test.
	require.NotEmpty(t, results)
}

func TestListSpecfile(t *testing.T) {
	contents, err := ioutil.ReadFile("testdata/Cargo.toml")
	require.NoError(t, err)

	pkgs := listSpecfileWithContents(contents)

	expectedPkgs := map[api.PkgName]api.PkgSpec{
		api.PkgName("rand"):       api.PkgSpec("https://github.com/rust-lang-nursery/rand"),
		api.PkgName("serde"):      api.PkgSpec("1.0.130"),
		api.PkgName("serde_json"): api.PkgSpec("1.0.68"),
		api.PkgName("sqlx"):       api.PkgSpec("0.5.7"),
	}

	require.Equal(t, expectedPkgs, pkgs)
}

func TestListLockfile(t *testing.T) {
	contents, err := ioutil.ReadFile("testdata/Cargo.lock")
	require.NoError(t, err)

	pkgs := listLockfileWithContents(contents)

	expectedPkgs := map[api.PkgName]api.PkgVersion{
		api.PkgName("ahash"):                        api.PkgVersion("0.7.4"),
		api.PkgName("atoi"):                         api.PkgVersion("0.4.0"),
		api.PkgName("autocfg"):                      api.PkgVersion("1.0.1"),
		api.PkgName("bitflags"):                     api.PkgVersion("1.3.2"),
		api.PkgName("block-buffer"):                 api.PkgVersion("0.9.0"),
		api.PkgName("bumpalo"):                      api.PkgVersion("3.7.1"),
		api.PkgName("byteorder"):                    api.PkgVersion("1.4.3"),
		api.PkgName("bytes"):                        api.PkgVersion("1.1.0"),
		api.PkgName("cfg-if"):                       api.PkgVersion("1.0.0"),
		api.PkgName("cpufeatures"):                  api.PkgVersion("0.2.1"),
		api.PkgName("crc"):                          api.PkgVersion("2.0.0"),
		api.PkgName("crc-catalog"):                  api.PkgVersion("1.1.1"),
		api.PkgName("crossbeam-channel"):            api.PkgVersion("0.5.1"),
		api.PkgName("crossbeam-queue"):              api.PkgVersion("0.3.2"),
		api.PkgName("crossbeam-utils"):              api.PkgVersion("0.8.5"),
		api.PkgName("digest"):                       api.PkgVersion("0.9.0"),
		api.PkgName("dotenv"):                       api.PkgVersion("0.15.0"),
		api.PkgName("either"):                       api.PkgVersion("1.6.1"),
		api.PkgName("form_urlencoded"):              api.PkgVersion("1.0.1"),
		api.PkgName("futures"):                      api.PkgVersion("0.3.17"),
		api.PkgName("futures-channel"):              api.PkgVersion("0.3.17"),
		api.PkgName("futures-core"):                 api.PkgVersion("0.3.17"),
		api.PkgName("futures-executor"):             api.PkgVersion("0.3.17"),
		api.PkgName("futures-intrusive"):            api.PkgVersion("0.4.0"),
		api.PkgName("futures-io"):                   api.PkgVersion("0.3.17"),
		api.PkgName("futures-macro"):                api.PkgVersion("0.3.17"),
		api.PkgName("futures-sink"):                 api.PkgVersion("0.3.17"),
		api.PkgName("futures-task"):                 api.PkgVersion("0.3.17"),
		api.PkgName("futures-util"):                 api.PkgVersion("0.3.17"),
		api.PkgName("generic-array"):                api.PkgVersion("0.14.4"),
		api.PkgName("getrandom"):                    api.PkgVersion("0.2.3"),
		api.PkgName("hashbrown"):                    api.PkgVersion("0.11.2"),
		api.PkgName("hashlink"):                     api.PkgVersion("0.7.0"),
		api.PkgName("heck"):                         api.PkgVersion("0.3.3"),
		api.PkgName("hex"):                          api.PkgVersion("0.4.3"),
		api.PkgName("idna"):                         api.PkgVersion("0.2.3"),
		api.PkgName("instant"):                      api.PkgVersion("0.1.11"),
		api.PkgName("itertools"):                    api.PkgVersion("0.10.1"),
		api.PkgName("itoa"):                         api.PkgVersion("0.4.8"),
		api.PkgName("js-sys"):                       api.PkgVersion("0.3.55"),
		api.PkgName("lazy_static"):                  api.PkgVersion("1.4.0"),
		api.PkgName("libc"):                         api.PkgVersion("0.2.103"),
		api.PkgName("lock_api"):                     api.PkgVersion("0.4.5"),
		api.PkgName("log"):                          api.PkgVersion("0.4.14"),
		api.PkgName("matches"):                      api.PkgVersion("0.1.9"),
		api.PkgName("memchr"):                       api.PkgVersion("2.4.1"),
		api.PkgName("minimal-lexical"):              api.PkgVersion("0.1.3"),
		api.PkgName("nom"):                          api.PkgVersion("7.0.0"),
		api.PkgName("num-traits"):                   api.PkgVersion("0.2.14"),
		api.PkgName("once_cell"):                    api.PkgVersion("1.8.0"),
		api.PkgName("opaque-debug"):                 api.PkgVersion("0.3.0"),
		api.PkgName("parking_lot"):                  api.PkgVersion("0.11.2"),
		api.PkgName("parking_lot_core"):             api.PkgVersion("0.8.5"),
		api.PkgName("percent-encoding"):             api.PkgVersion("2.1.0"),
		api.PkgName("pin-project-lite"):             api.PkgVersion("0.2.7"),
		api.PkgName("pin-utils"):                    api.PkgVersion("0.1.0"),
		api.PkgName("ppv-lite86"):                   api.PkgVersion("0.2.10"),
		api.PkgName("proc-macro-hack"):              api.PkgVersion("0.5.19"),
		api.PkgName("proc-macro-nested"):            api.PkgVersion("0.1.7"),
		api.PkgName("proc-macro2"):                  api.PkgVersion("1.0.29"),
		api.PkgName("quote"):                        api.PkgVersion("1.0.9"),
		api.PkgName("rand"):                         api.PkgVersion("0.8.5"),
		api.PkgName("rand_chacha"):                  api.PkgVersion("0.3.1"),
		api.PkgName("rand_core"):                    api.PkgVersion("0.6.4"),
		api.PkgName("redox_syscall"):                api.PkgVersion("0.2.10"),
		api.PkgName("rust-upm-test"):                api.PkgVersion("0.1.0"),
		api.PkgName("ryu"):                          api.PkgVersion("1.0.5"),
		api.PkgName("scopeguard"):                   api.PkgVersion("1.1.0"),
		api.PkgName("serde"):                        api.PkgVersion("1.0.130"),
		api.PkgName("serde_json"):                   api.PkgVersion("1.0.68"),
		api.PkgName("sha2"):                         api.PkgVersion("0.9.8"),
		api.PkgName("slab"):                         api.PkgVersion("0.4.4"),
		api.PkgName("smallvec"):                     api.PkgVersion("1.6.1"),
		api.PkgName("sqlformat"):                    api.PkgVersion("0.1.8"),
		api.PkgName("sqlx"):                         api.PkgVersion("0.5.7"),
		api.PkgName("sqlx-core"):                    api.PkgVersion("0.5.7"),
		api.PkgName("sqlx-macros"):                  api.PkgVersion("0.5.7"),
		api.PkgName("sqlx-rt"):                      api.PkgVersion("0.5.7"),
		api.PkgName("stringprep"):                   api.PkgVersion("0.1.2"),
		api.PkgName("syn"):                          api.PkgVersion("1.0.77"),
		api.PkgName("thiserror"):                    api.PkgVersion("1.0.29"),
		api.PkgName("thiserror-impl"):               api.PkgVersion("1.0.29"),
		api.PkgName("tinyvec"):                      api.PkgVersion("1.5.0"),
		api.PkgName("tinyvec_macros"):               api.PkgVersion("0.1.0"),
		api.PkgName("typenum"):                      api.PkgVersion("1.14.0"),
		api.PkgName("unicode-bidi"):                 api.PkgVersion("0.3.6"),
		api.PkgName("unicode-normalization"):        api.PkgVersion("0.1.19"),
		api.PkgName("unicode-segmentation"):         api.PkgVersion("1.8.0"),
		api.PkgName("unicode-xid"):                  api.PkgVersion("0.2.2"),
		api.PkgName("unicode_categories"):           api.PkgVersion("0.1.1"),
		api.PkgName("url"):                          api.PkgVersion("2.2.2"),
		api.PkgName("version_check"):                api.PkgVersion("0.9.3"),
		api.PkgName("wasi"):                         api.PkgVersion("0.10.2+wasi-snapshot-preview1"),
		api.PkgName("wasm-bindgen"):                 api.PkgVersion("0.2.78"),
		api.PkgName("wasm-bindgen-backend"):         api.PkgVersion("0.2.78"),
		api.PkgName("wasm-bindgen-macro"):           api.PkgVersion("0.2.78"),
		api.PkgName("wasm-bindgen-macro-support"):   api.PkgVersion("0.2.78"),
		api.PkgName("wasm-bindgen-shared"):          api.PkgVersion("0.2.78"),
		api.PkgName("web-sys"):                      api.PkgVersion("0.3.55"),
		api.PkgName("whoami"):                       api.PkgVersion("1.1.4"),
		api.PkgName("winapi"):                       api.PkgVersion("0.3.9"),
		api.PkgName("winapi-i686-pc-windows-gnu"):   api.PkgVersion("0.4.0"),
		api.PkgName("winapi-x86_64-pc-windows-gnu"): api.PkgVersion("0.4.0"),
	}

	require.Equal(t, expectedPkgs, pkgs)
}
