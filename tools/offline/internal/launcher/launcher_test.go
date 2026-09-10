package launcher

import (
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func writeRequiredSite(t *testing.T, root string) {
	t.Helper()
	for _, relative := range []string{
		"index.html",
		filepath.Join("app", "index.html"),
		filepath.Join("app", "pinned-wasm-library.json"),
		filepath.Join("app", "shinylive", "shinylive.js"),
		filepath.Join("pkgdown", "index.html"),
	} {
		path := filepath.Join(root, relative)
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte("content for "+relative), 0o644); err != nil {
			t.Fatal(err)
		}
	}
}

func TestResolveSiteRootHandlesSpaces(t *testing.T) {
	executable := filepath.Join(t.TempDir(), "OpenSpecy folder with spaces",
		"OpenSpecy Offline")
	got, err := ResolveSiteRoot(executable)
	if err != nil {
		t.Fatal(err)
	}
	want := filepath.Join(filepath.Dir(executable), "site")
	if got != want {
		t.Fatalf("ResolveSiteRoot() = %q, want %q", got, want)
	}
}

func TestValidateSiteRoot(t *testing.T) {
	root := t.TempDir()
	writeRequiredSite(t, root)
	if err := ValidateSiteRoot(root); err != nil {
		t.Fatal(err)
	}
	if err := os.Remove(filepath.Join(root, "app", "index.html")); err != nil {
		t.Fatal(err)
	}
	if err := ValidateSiteRoot(root); err == nil || !strings.Contains(err.Error(), "app") {
		t.Fatalf("expected missing app error, got %v", err)
	}
}

func TestHandlerServesOnlyLoopbackHosts(t *testing.T) {
	root := t.TempDir()
	writeRequiredSite(t, root)
	handler := NewHandler(root)

	request := httptest.NewRequest(http.MethodGet, "http://127.0.0.1/index.html", nil)
	request.Host = "127.0.0.1:43210"
	response := httptest.NewRecorder()
	handler.ServeHTTP(response, request)
	if response.Code != http.StatusOK {
		t.Fatalf("loopback response status = %d", response.Code)
	}
	body, _ := io.ReadAll(response.Result().Body)
	if !strings.Contains(string(body), "content for index.html") {
		t.Fatalf("unexpected response body %q", body)
	}

	request = httptest.NewRequest(http.MethodGet, "http://example.test/index.html", nil)
	request.Host = "example.test:43210"
	response = httptest.NewRecorder()
	handler.ServeHTTP(response, request)
	if response.Code != http.StatusMisdirectedRequest {
		t.Fatalf("non-loopback response status = %d", response.Code)
	}
}

func TestListenUsesAnAvailableIPv4LoopbackPort(t *testing.T) {
	listener, err := Listen()
	if err != nil {
		t.Fatal(err)
	}
	defer listener.Close()
	address := listener.Addr().String()
	if !strings.HasPrefix(address, "127.0.0.1:") || strings.HasSuffix(address, ":0") {
		t.Fatalf("unexpected listener address %q", address)
	}
}

func TestBrowserCommandsUseNativeOpeners(t *testing.T) {
	url := "http://127.0.0.1:43210/"
	cases := map[string]string{
		"windows": "rundll32.exe",
		"darwin":  "open",
		"linux":   "xdg-open",
	}
	for goos, want := range cases {
		commands := browserCommands(goos, url)
		if len(commands) == 0 || commands[0].name != want {
			t.Fatalf("%s first browser command = %#v, want %q", goos, commands, want)
		}
		if !strings.Contains(strings.Join(commands[0].args, " "), url) {
			t.Fatalf("%s browser command omits URL", goos)
		}
	}
}
