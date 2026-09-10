// Package launcher serves an exported OpenSpecy site from beside a native
// executable. It intentionally uses only the Go standard library.
package launcher

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"mime"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"time"
)

const loopbackAddress = "127.0.0.1:0"

// Options contains test/automation switches. Normal users need no arguments.
type Options struct {
	NoBrowser bool
	URLFile   string
}

// Main parses launcher arguments and serves until Ctrl+C or termination.
func Main(args []string) error {
	options, err := parseOptions(args)
	if err != nil {
		return err
	}
	executable, err := os.Executable()
	if err != nil {
		return fmt.Errorf("locate the launcher: %w", err)
	}
	return Run(executable, options)
}

func parseOptions(args []string) (Options, error) {
	var options Options
	flags := flag.NewFlagSet("OpenSpecy Offline", flag.ContinueOnError)
	flags.SetOutput(io.Discard)
	flags.BoolVar(&options.NoBrowser, "no-browser", false,
		"serve without opening the default browser (automation only)")
	flags.StringVar(&options.URLFile, "write-url", "",
		"write the selected local URL to a file (automation only)")
	if err := flags.Parse(args); err != nil {
		return Options{}, fmt.Errorf("invalid launcher option: %w", err)
	}
	if flags.NArg() != 0 {
		return Options{}, fmt.Errorf("unexpected argument %q", flags.Arg(0))
	}
	return options, nil
}

// ResolveSiteRoot locates the immutable site relative to the launcher. No
// current-working-directory assumption is made, so extracted paths with spaces
// and launching from shortcuts both work.
func ResolveSiteRoot(executable string) (string, error) {
	if strings.TrimSpace(executable) == "" {
		return "", errors.New("launcher path is empty")
	}
	absolute, err := filepath.Abs(executable)
	if err != nil {
		return "", fmt.Errorf("resolve launcher path: %w", err)
	}
	return filepath.Join(filepath.Dir(absolute), "site"), nil
}

// ValidateSiteRoot rejects partial bundles before opening a browser.
func ValidateSiteRoot(siteRoot string) error {
	required := []string{
		"index.html",
		filepath.Join("app", "index.html"),
		filepath.Join("app", "pinned-wasm-library.json"),
		filepath.Join("app", "shinylive", "shinylive.js"),
		filepath.Join("pkgdown", "index.html"),
	}
	for _, relative := range required {
		path := filepath.Join(siteRoot, relative)
		info, err := os.Stat(path)
		if err != nil {
			return fmt.Errorf("bundle is incomplete; missing %s: %w", relative, err)
		}
		if !info.Mode().IsRegular() || info.Size() == 0 {
			return fmt.Errorf("bundle is incomplete; %s is not a non-empty file", relative)
		}
	}
	return nil
}

// NewHandler returns a static handler with a strict Host guard. The listener is
// also IPv4-loopback-only; the guard prevents DNS-rebinding access to the site.
func NewHandler(siteRoot string) http.Handler {
	_ = mime.AddExtensionType(".wasm", "application/wasm")
	_ = mime.AddExtensionType(".mjs", "text/javascript; charset=utf-8")
	_ = mime.AddExtensionType(".js", "text/javascript; charset=utf-8")
	files := http.FileServer(http.Dir(siteRoot))
	return http.HandlerFunc(func(response http.ResponseWriter, request *http.Request) {
		host := request.Host
		if parsed, _, err := net.SplitHostPort(host); err == nil {
			host = parsed
		}
		if host != "127.0.0.1" {
			http.Error(response, "OpenSpecy Offline is available only on this computer.",
				http.StatusMisdirectedRequest)
			return
		}
		response.Header().Set("X-Content-Type-Options", "nosniff")
		response.Header().Set("Referrer-Policy", "no-referrer")
		files.ServeHTTP(response, request)
	})
}

// Listen opens an operating-system-selected TCP port only on IPv4 loopback.
func Listen() (net.Listener, error) {
	listener, err := net.Listen("tcp4", loopbackAddress)
	if err != nil {
		return nil, fmt.Errorf("start the loopback web server: %w", err)
	}
	return listener, nil
}

// Run starts the local server, opens the default browser, and blocks until the
// process is interrupted. Browser launch failure is non-fatal because the URL
// remains visible for manual opening.
func Run(executable string, options Options) error {
	siteRoot, err := ResolveSiteRoot(executable)
	if err != nil {
		return err
	}
	if err := ValidateSiteRoot(siteRoot); err != nil {
		return err
	}
	listener, err := Listen()
	if err != nil {
		return err
	}
	defer listener.Close()

	port := listener.Addr().(*net.TCPAddr).Port
	// Open the app route directly. The marketing landing page intentionally
	// offers an online video; bypassing it guarantees that normal offline launch
	// attempts no non-loopback request. The complete / and /pkgdown/ trees remain
	// available from the same local server.
	url := fmt.Sprintf("http://127.0.0.1:%d/app/", port)
	if options.URLFile != "" {
		if err := os.WriteFile(options.URLFile, []byte(url+"\n"), 0o600); err != nil {
			return fmt.Errorf("write selected URL: %w", err)
		}
	}

	server := &http.Server{
		Handler:           NewHandler(siteRoot),
		ReadHeaderTimeout: 10 * time.Second,
		IdleTimeout:       2 * time.Minute,
	}
	serveError := make(chan error, 1)
	go func() {
		err := server.Serve(listener)
		if err != nil && !errors.Is(err, http.ErrServerClosed) {
			serveError <- err
			return
		}
		serveError <- nil
	}()

	fmt.Println("OpenSpecy Offline is running at", url)
	fmt.Println("All app files are served from this folder on 127.0.0.1 only.")
	fmt.Println("Keep this window open; press Ctrl+C to stop OpenSpecy Offline.")
	if !options.NoBrowser {
		if err := openBrowser(runtime.GOOS, url); err != nil {
			fmt.Fprintln(os.Stderr, "Could not open the default browser automatically:", err)
			fmt.Fprintln(os.Stderr, "Open this address in a modern browser:", url)
		}
	}

	interrupt := make(chan os.Signal, 1)
	notifyInterrupt(interrupt)
	defer stopInterrupt(interrupt)
	select {
	case err := <-serveError:
		if err != nil {
			return fmt.Errorf("serve the offline app: %w", err)
		}
		return nil
	case <-interrupt:
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		if err := server.Shutdown(ctx); err != nil {
			return fmt.Errorf("stop the loopback web server: %w", err)
		}
		return <-serveError
	}
}

type browserCommand struct {
	name string
	args []string
}

func browserCommands(goos, url string) []browserCommand {
	switch goos {
	case "windows":
		return []browserCommand{
			{name: "rundll32.exe", args: []string{"url.dll,FileProtocolHandler", url}},
			{name: "cmd.exe", args: []string{"/c", "start", "", url}},
		}
	case "darwin":
		return []browserCommand{{name: "open", args: []string{url}}}
	default:
		return []browserCommand{
			{name: "xdg-open", args: []string{url}},
			{name: "gio", args: []string{"open", url}},
			{name: "sensible-browser", args: []string{url}},
		}
	}
}

func openBrowser(goos, url string) error {
	var failures []string
	for _, candidate := range browserCommands(goos, url) {
		path, err := exec.LookPath(candidate.name)
		if err != nil {
			failures = append(failures, candidate.name+": not found")
			continue
		}
		if err := exec.Command(path, candidate.args...).Start(); err == nil {
			return nil
		} else {
			failures = append(failures, candidate.name+": "+err.Error())
		}
	}
	return errors.New(strings.Join(failures, "; "))
}
