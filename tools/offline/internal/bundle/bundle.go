// Package bundle validates and packages an exact action-built OpenSpecy Pages
// tree with one native launcher. It intentionally uses only the Go standard
// library so the distribution workflow has a small, auditable build surface.
package bundle

import (
	"archive/zip"
	"bufio"
	"crypto/sha256"
	"encoding/binary"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	pathpkg "path"
	"path/filepath"
	"regexp"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"time"
)

const (
	sourceWorkflow = "Build and deploy Shinylive app"
	manifestName   = "offline-manifest.json"
	checksumsName  = "SHA256SUMS.txt"
	readmeName     = "README-OFFLINE.txt"
)

var (
	shaPattern = regexp.MustCompile(`^[0-9a-f]{40}$`)
	targets    = map[string]targetDefinition{
		"windows-amd64": {GOOS: "windows", GOARCH: "amd64", Launcher: "OpenSpecy Offline.exe"},
		"macos-amd64":   {GOOS: "darwin", GOARCH: "amd64", Launcher: "OpenSpecy Offline"},
		"macos-arm64":   {GOOS: "darwin", GOARCH: "arm64", Launcher: "OpenSpecy Offline"},
		"linux-amd64":   {GOOS: "linux", GOARCH: "amd64", Launcher: "OpenSpecy Offline"},
		"linux-arm64":   {GOOS: "linux", GOARCH: "arm64", Launcher: "OpenSpecy Offline"},
	}
	archiveEpoch = time.Date(1980, time.January, 1, 0, 0, 0, 0, time.UTC)
)

type targetDefinition struct {
	GOOS     string
	GOARCH   string
	Launcher string
}

// Config identifies one exact Pages artifact and target launcher.
type Config struct {
	ArtifactRoot     string
	Launcher         string
	Output           string
	PackageSHA       string
	Target           string
	SourceRepository string
	SourceRunID      string
}

// Result summarizes one fully verified archive.
type Result struct {
	Archive       string
	ArchiveSHA256 string
	FileCount     int
	SiteBytes     int64
}

type packageIdentity struct {
	Package struct {
		Name    string `json:"name"`
		Version string `json:"version"`
		Commit  string `json:"commit"`
	} `json:"package"`
	WasmBuild struct {
		Artifact       string `json:"artifact"`
		Immutable      bool   `json:"immutable"`
		BundledIntoApp bool  `json:"bundled_into_app"`
	} `json:"wasm_build"`
}

type offlineManifest struct {
	SchemaVersion int `json:"schema_version"`
	Package       struct {
		Name    string `json:"name"`
		Version string `json:"version"`
		Commit  string `json:"commit"`
	} `json:"package"`
	Target struct {
		Name     string `json:"name"`
		GOOS     string `json:"goos"`
		GOARCH   string `json:"goarch"`
		Launcher string `json:"launcher"`
	} `json:"target"`
	Source struct {
		Repository string `json:"repository"`
		Workflow   string `json:"workflow"`
		RunID      string `json:"run_id"`
		Artifact   string `json:"artifact"`
	} `json:"source"`
	Site struct {
		Directory  string `json:"directory"`
		EntryPoint string `json:"entry_point"`
		Files      int    `json:"files"`
		Bytes      int64  `json:"bytes"`
		TreeSHA256 string `json:"tree_sha256"`
	} `json:"site"`
	SourceManifests map[string]string `json:"source_manifest_sha256"`
	BuildToolchain  string            `json:"build_toolchain"`
}

type fileRecord struct {
	Path   string
	Size   int64
	SHA256 string
}

// Build validates identity, copies the _site tree byte-for-byte, adds support
// files, writes the ZIP, and verifies every archived checksum.
func Build(config Config) (Result, error) {
	definition, err := validateConfig(config)
	if err != nil {
		return Result{}, err
	}
	identity, manifestHashes, siteRoot, err := validateArtifact(config.ArtifactRoot, config.PackageSHA)
	if err != nil {
		return Result{}, err
	}
	if err := os.MkdirAll(filepath.Dir(config.Output), 0o755); err != nil {
		return Result{}, fmt.Errorf("create output directory: %w", err)
	}
	stage, err := os.MkdirTemp(filepath.Dir(config.Output), ".openspecy-offline-stage-")
	if err != nil {
		return Result{}, fmt.Errorf("create packaging directory: %w", err)
	}
	defer os.RemoveAll(stage)

	bundleRootName := "OpenSpecy-offline-" + config.PackageSHA
	bundleRoot := filepath.Join(stage, bundleRootName)
	destinationSite := filepath.Join(bundleRoot, "site")
	if err := copyTree(siteRoot, destinationSite); err != nil {
		return Result{}, fmt.Errorf("copy exact Pages site: %w", err)
	}
	launcherDestination := filepath.Join(bundleRoot, definition.Launcher)
	if err := copyFile(config.Launcher, launcherDestination, 0o755); err != nil {
		return Result{}, fmt.Errorf("copy native launcher: %w", err)
	}

	siteRecords, err := scanFiles(destinationSite)
	if err != nil {
		return Result{}, fmt.Errorf("inventory copied site: %w", err)
	}
	siteBytes := sumBytes(siteRecords)
	manifest := makeOfflineManifest(config, definition, identity, manifestHashes,
		siteRecords, siteBytes)
	manifestBytes, err := json.MarshalIndent(manifest, "", "  ")
	if err != nil {
		return Result{}, fmt.Errorf("encode offline manifest: %w", err)
	}
	manifestBytes = append(manifestBytes, '\n')
	if err := os.WriteFile(filepath.Join(bundleRoot, manifestName), manifestBytes, 0o644); err != nil {
		return Result{}, fmt.Errorf("write offline manifest: %w", err)
	}
	readme := offlineReadme(config, definition, identity.Package.Version)
	if err := os.WriteFile(filepath.Join(bundleRoot, readmeName), []byte(readme), 0o644); err != nil {
		return Result{}, fmt.Errorf("write offline README: %w", err)
	}

	checksumRecords, err := scanFiles(bundleRoot)
	if err != nil {
		return Result{}, fmt.Errorf("inventory bundle: %w", err)
	}
	if err := writeChecksums(filepath.Join(bundleRoot, checksumsName), checksumRecords); err != nil {
		return Result{}, err
	}
	if err := writeZip(config.Output, bundleRoot, bundleRootName); err != nil {
		return Result{}, err
	}
	if err := verifyArchive(config.Output, bundleRootName, config.PackageSHA,
		config.Target, definition.Launcher); err != nil {
		return Result{}, fmt.Errorf("verify completed archive: %w", err)
	}
	archiveHash, _, err := hashFile(config.Output)
	if err != nil {
		return Result{}, fmt.Errorf("hash completed archive: %w", err)
	}
	companion := fmt.Sprintf("%s  %s\n", archiveHash, filepath.Base(config.Output))
	if err := os.WriteFile(config.Output+".sha256", []byte(companion), 0o644); err != nil {
		return Result{}, fmt.Errorf("write archive checksum: %w", err)
	}
	if err := verifyArchiveCompanion(config.Output); err != nil {
		return Result{}, fmt.Errorf("verify archive checksum companion: %w", err)
	}
	return Result{
		Archive:       config.Output,
		ArchiveSHA256: archiveHash,
		FileCount:     len(siteRecords),
		SiteBytes:     siteBytes,
	}, nil
}

func validateConfig(config Config) (targetDefinition, error) {
	if !shaPattern.MatchString(config.PackageSHA) {
		return targetDefinition{}, errors.New("package SHA must be exactly 40 lowercase hexadecimal characters")
	}
	definition, ok := targets[config.Target]
	if !ok {
		return targetDefinition{}, fmt.Errorf("unsupported target %q", config.Target)
	}
	for label, value := range map[string]string{
		"artifact root": config.ArtifactRoot,
		"launcher": config.Launcher,
		"output": config.Output,
		"source repository": config.SourceRepository,
		"source run ID": config.SourceRunID,
	} {
		if strings.TrimSpace(value) == "" {
			return targetDefinition{}, fmt.Errorf("%s is required", label)
		}
	}
	if _, err := strconv.ParseUint(config.SourceRunID, 10, 64); err != nil {
		return targetDefinition{}, fmt.Errorf("source run ID must be numeric: %w", err)
	}
	expectedArchive := fmt.Sprintf("openspecy-offline-%s-%s.zip", config.Target, config.PackageSHA)
	if filepath.Base(config.Output) != expectedArchive {
		return targetDefinition{}, fmt.Errorf("archive must be named %s", expectedArchive)
	}
	info, err := os.Stat(config.Launcher)
	if err != nil {
		return targetDefinition{}, fmt.Errorf("inspect launcher: %w", err)
	}
	if !info.Mode().IsRegular() || info.Size() == 0 {
		return targetDefinition{}, errors.New("launcher must be a non-empty regular file")
	}
	if err := validateLauncherBinary(config.Launcher, definition); err != nil {
		return targetDefinition{}, err
	}
	return definition, nil
}

type launcherBinaryIdentity struct {
	format  string
	machine uint32
}

func validateLauncherBinary(filename string, definition targetDefinition) error {
	actual, err := inspectLauncherBinary(filename)
	if err != nil {
		return fmt.Errorf("inspect launcher binary: %w", err)
	}
	expected := launcherBinaryIdentity{}
	switch definition.GOOS + "/" + definition.GOARCH {
	case "windows/amd64":
		expected = launcherBinaryIdentity{format: "PE", machine: 0x8664}
	case "darwin/amd64":
		expected = launcherBinaryIdentity{format: "Mach-O", machine: 0x01000007}
	case "darwin/arm64":
		expected = launcherBinaryIdentity{format: "Mach-O", machine: 0x0100000c}
	case "linux/amd64":
		expected = launcherBinaryIdentity{format: "ELF", machine: 0x3e}
	case "linux/arm64":
		expected = launcherBinaryIdentity{format: "ELF", machine: 0xb7}
	default:
		return fmt.Errorf("no launcher binary identity is defined for %s/%s",
			definition.GOOS, definition.GOARCH)
	}
	if actual != expected {
		return fmt.Errorf(
			"launcher binary is %s machine %#x; target requires %s machine %#x",
			actual.format, actual.machine, expected.format, expected.machine)
	}
	return nil
}

func inspectLauncherBinary(filename string) (launcherBinaryIdentity, error) {
	file, err := os.Open(filename)
	if err != nil {
		return launcherBinaryIdentity{}, err
	}
	defer file.Close()
	header := make([]byte, 64)
	if _, err := io.ReadFull(file, header); err != nil {
		return launcherBinaryIdentity{}, errors.New("launcher header is truncated")
	}

	switch {
	case string(header[:4]) == "\x7fELF":
		if header[4] != 2 {
			return launcherBinaryIdentity{}, errors.New("launcher ELF is not 64-bit")
		}
		var order binary.ByteOrder
		switch header[5] {
		case 1:
			order = binary.LittleEndian
		case 2:
			order = binary.BigEndian
		default:
			return launcherBinaryIdentity{}, errors.New("launcher ELF has an invalid byte order")
		}
		return launcherBinaryIdentity{
			format: "ELF", machine: uint32(order.Uint16(header[18:20])),
		}, nil
	case string(header[:4]) == "\xcf\xfa\xed\xfe":
		return launcherBinaryIdentity{
			format: "Mach-O", machine: binary.LittleEndian.Uint32(header[4:8]),
		}, nil
	case string(header[:4]) == "\xfe\xed\xfa\xcf":
		return launcherBinaryIdentity{
			format: "Mach-O", machine: binary.BigEndian.Uint32(header[4:8]),
		}, nil
	case string(header[:2]) == "MZ":
		peOffset := int64(binary.LittleEndian.Uint32(header[0x3c:0x40]))
		info, err := file.Stat()
		if err != nil {
			return launcherBinaryIdentity{}, err
		}
		if peOffset < 64 || peOffset+6 > info.Size() {
			return launcherBinaryIdentity{}, errors.New("launcher PE header offset is invalid")
		}
		peHeader := make([]byte, 6)
		if _, err := file.ReadAt(peHeader, peOffset); err != nil {
			return launcherBinaryIdentity{}, errors.New("launcher PE header is truncated")
		}
		if string(peHeader[:4]) != "PE\x00\x00" {
			return launcherBinaryIdentity{}, errors.New("launcher PE signature is invalid")
		}
		return launcherBinaryIdentity{
			format: "PE", machine: uint32(binary.LittleEndian.Uint16(peHeader[4:6])),
		}, nil
	default:
		return launcherBinaryIdentity{}, errors.New("launcher is not a supported PE, Mach-O, or ELF binary")
	}
}

func validateArtifact(root, packageSHA string) (packageIdentity, map[string]string, string, error) {
	paths := map[string]string{
		"wasm_app_manifest": filepath.Join(root, "_wasm", "wasm-app-manifest.json"),
		"resolved_wasm_packages": filepath.Join(root, "_wasm", "pinned", "metadata", "resolved-wasm-packages.json"),
		"pinned_wasm_library": filepath.Join(root, "_site", "app", "pinned-wasm-library.json"),
	}
	identities := make(map[string]packageIdentity, len(paths))
	hashes := make(map[string]string, len(paths))
	for label, path := range paths {
		var identity packageIdentity
		if err := readJSON(path, &identity); err != nil {
			return packageIdentity{}, nil, "", fmt.Errorf("read %s: %w", label, err)
		}
		if identity.Package.Name != "OpenSpecy" || identity.Package.Version == "" ||
			identity.Package.Commit != packageSHA {
			return packageIdentity{}, nil, "", fmt.Errorf(
				"%s package identity does not match OpenSpecy commit %s", label, packageSHA)
		}
		hash, _, err := hashFile(path)
		if err != nil {
			return packageIdentity{}, nil, "", err
		}
		identities[label] = identity
		hashes[label] = hash
	}
	app := identities["wasm_app_manifest"]
	expectedArtifact := "openspecy-wasm-" + packageSHA
	if app.WasmBuild.Artifact != expectedArtifact || !app.WasmBuild.Immutable ||
		!app.WasmBuild.BundledIntoApp {
		return packageIdentity{}, nil, "", fmt.Errorf(
			"wasm app manifest is not the immutable bundled artifact %s", expectedArtifact)
	}
	for label, identity := range identities {
		if identity.Package.Version != app.Package.Version {
			return packageIdentity{}, nil, "", fmt.Errorf(
				"%s package version %s differs from app version %s",
				label, identity.Package.Version, app.Package.Version)
		}
	}

	siteRoot := filepath.Join(root, "_site")
	required := map[string]string{
		"index.html": "data-openspecy-embed",
		filepath.Join("app", "index.html"): "runExportedApp",
		filepath.Join("pkgdown", "index.html"): "Generated by pkgdown",
		filepath.Join("app", "shinylive", "shinylive.js"): "",
		filepath.Join("app", "shinylive", "webr", "packages", "metadata.rds"): "",
		filepath.Join("app", "shinylive", "webr", "packages", "OpenSpecyPinned", "library.data.gz"): "",
		filepath.Join("app", "shinylive", "webr", "packages", "OpenSpecyPinned", "library.js.metadata"): "",
	}
	for relative, marker := range required {
		path := filepath.Join(siteRoot, relative)
		info, err := os.Stat(path)
		if err != nil || !info.Mode().IsRegular() || info.Size() == 0 {
			return packageIdentity{}, nil, "", fmt.Errorf("Pages artifact is missing non-empty %s", relative)
		}
		if marker != "" {
			content, err := os.ReadFile(path)
			if err != nil || !strings.Contains(string(content), marker) {
				return packageIdentity{}, nil, "", fmt.Errorf("Pages route %s is missing marker %q", relative, marker)
			}
		}
	}
	return app, hashes, siteRoot, nil
}

func readJSON(path string, destination any) error {
	file, err := os.Open(path)
	if err != nil {
		return err
	}
	defer file.Close()
	if err := json.NewDecoder(file).Decode(destination); err != nil {
		return err
	}
	return nil
}

func copyTree(source, destination string) error {
	seen := make(map[string]string)
	return filepath.WalkDir(source, func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		relative, err := filepath.Rel(source, path)
		if err != nil {
			return err
		}
		if relative == "." {
			return os.MkdirAll(destination, 0o755)
		}
		portableRelative := filepath.ToSlash(relative)
		if err := validatePortableRelativePath(portableRelative); err != nil {
			return fmt.Errorf("unsafe Pages artifact path %q: %w", relative, err)
		}
		key := portablePathKey(portableRelative)
		if previous, exists := seen[key]; exists {
			if collapsiblePkgdownRedirects(source, previous, portableRelative) {
				return nil
			}
			return fmt.Errorf("Pages artifact paths %q and %q collide on a portable filesystem",
				previous, portableRelative)
		}
		seen[key] = portableRelative
		if entry.Type()&os.ModeSymlink != 0 {
			return fmt.Errorf("symbolic links are not allowed in the Pages artifact: %s", relative)
		}
		target := filepath.Join(destination, relative)
		if entry.IsDir() {
			return os.MkdirAll(target, 0o755)
		}
		if !entry.Type().IsRegular() {
			return fmt.Errorf("unsupported Pages artifact entry: %s", relative)
		}
		return copyFile(path, target, 0o644)
	})
}

// pkgdown can emit case-only aliases for two different documentation topics
// (for example OpenSpecy.html and openspecy.html). Both are redirect stubs,
// but no portable archive can represent both names safely. Keep the first
// lexical alias only when both colliding files are verified pkgdown redirects;
// every other collision remains a hard error.
func collapsiblePkgdownRedirects(root, first, second string) bool {
	firstSlash := filepath.ToSlash(first)
	secondSlash := filepath.ToSlash(second)
	if firstSlash == secondSlash || !strings.EqualFold(firstSlash, secondSlash) ||
		strings.ToLower(filepath.ToSlash(filepath.Dir(firstSlash))) != "pkgdown/reference" ||
		strings.ToLower(filepath.Ext(firstSlash)) != ".html" {
		return false
	}
	for _, relative := range []string{first, second} {
		content, err := os.ReadFile(filepath.Join(root, filepath.FromSlash(relative)))
		if err != nil {
			return false
		}
		lower := strings.ToLower(string(content))
		if !strings.Contains(lower, `<meta http-equiv="refresh"`) ||
			!strings.Contains(lower, `<link rel="canonical"`) {
			return false
		}
	}
	return true
}

func copyFile(source, destination string, mode os.FileMode) error {
	input, err := os.Open(source)
	if err != nil {
		return err
	}
	defer input.Close()
	if err := os.MkdirAll(filepath.Dir(destination), 0o755); err != nil {
		return err
	}
	output, err := os.OpenFile(destination, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, mode)
	if err != nil {
		return err
	}
	_, copyErr := io.Copy(output, input)
	closeErr := output.Close()
	if copyErr != nil {
		return copyErr
	}
	return closeErr
}

func scanFiles(root string) ([]fileRecord, error) {
	var records []fileRecord
	seen := make(map[string]string)
	err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if entry.IsDir() {
			return nil
		}
		if !entry.Type().IsRegular() {
			return fmt.Errorf("cannot checksum non-regular file %s", path)
		}
		relative, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		portableRelative := filepath.ToSlash(relative)
		if err := validatePortableRelativePath(portableRelative); err != nil {
			return fmt.Errorf("unsafe checksum path %q: %w", relative, err)
		}
		key := portablePathKey(portableRelative)
		if previous, exists := seen[key]; exists {
			return fmt.Errorf("checksum paths %q and %q collide on a portable filesystem",
				previous, portableRelative)
		}
		seen[key] = portableRelative
		hash, size, err := hashFile(path)
		if err != nil {
			return err
		}
		records = append(records, fileRecord{
			Path: portableRelative, Size: size, SHA256: hash,
		})
		return nil
	})
	sort.Slice(records, func(left, right int) bool { return records[left].Path < records[right].Path })
	return records, err
}

func hashFile(path string) (string, int64, error) {
	file, err := os.Open(path)
	if err != nil {
		return "", 0, err
	}
	defer file.Close()
	hash := sha256.New()
	size, err := io.Copy(hash, file)
	if err != nil {
		return "", 0, err
	}
	return hex.EncodeToString(hash.Sum(nil)), size, nil
}

func sumBytes(records []fileRecord) int64 {
	var total int64
	for _, record := range records {
		total += record.Size
	}
	return total
}

func treeHash(records []fileRecord) string {
	hash := sha256.New()
	for _, record := range records {
		fmt.Fprintf(hash, "%s\x00%d\x00%s\n", record.Path, record.Size, record.SHA256)
	}
	return hex.EncodeToString(hash.Sum(nil))
}

func makeOfflineManifest(config Config, definition targetDefinition,
	identity packageIdentity, sourceHashes map[string]string,
	siteRecords []fileRecord, siteBytes int64) offlineManifest {
	var manifest offlineManifest
	manifest.SchemaVersion = 1
	manifest.Package.Name = identity.Package.Name
	manifest.Package.Version = identity.Package.Version
	manifest.Package.Commit = identity.Package.Commit
	manifest.Target.Name = config.Target
	manifest.Target.GOOS = definition.GOOS
	manifest.Target.GOARCH = definition.GOARCH
	manifest.Target.Launcher = definition.Launcher
	manifest.Source.Repository = config.SourceRepository
	manifest.Source.Workflow = sourceWorkflow
	manifest.Source.RunID = config.SourceRunID
	manifest.Source.Artifact = "openspecy-pages-" + config.PackageSHA
	manifest.Site.Directory = "site/"
	manifest.Site.EntryPoint = "site/app/"
	manifest.Site.Files = len(siteRecords)
	manifest.Site.Bytes = siteBytes
	manifest.Site.TreeSHA256 = treeHash(siteRecords)
	manifest.SourceManifests = sourceHashes
	manifest.BuildToolchain = runtime.Version()
	return manifest
}

func offlineReadme(config Config, definition targetDefinition, version string) string {
	launch := definition.Launcher
	launchInstruction := "Double-click \"" + launch + "\"."
	platformNote := ""
	switch definition.GOOS {
	case "darwin":
		launchInstruction = "Open Terminal in this folder and run: ./OpenSpecy\\ Offline"
		platformNote = "\nThis launcher is unsigned. If macOS blocks the first launch, use Privacy & Security > Open Anyway, then launch it again.\n"
	case "linux":
		launchInstruction = "Double-click \"OpenSpecy Offline\" or run it from this folder as: ./OpenSpecy\\ Offline"
		platformNote = "\nIf your file manager asks, choose Run. The executable permission is stored in this ZIP.\n"
	case "windows":
		platformNote = "\nThis launcher is unsigned. Windows may show a reputation prompt; review the SHA and choose Run anyway only for the expected GitHub Actions artifact.\n"
	}
	return fmt.Sprintf(`OpenSpecy Offline %s
Commit: %s
Target: %s (%s/%s)
Source artifact: openspecy-pages-%s
Source run: https://github.com/%s/actions/runs/%s

WHAT YOU NEED
A modern browser with WebAssembly and service-worker support. You do not need
R, Python, Node, an installer, an additional file download, or an internet
connection.

START
1. Keep this entire extracted folder together; do not move only the launcher.
2. %s
3. Keep the launcher window open while using OpenSpecy. It selects an available
   port on 127.0.0.1, opens /app/ in your default browser, and serves only files
   in this bundle.

STOP
Press Ctrl+C in the launcher window, or close that window.

PRIVACY AND OFFLINE USE
The app and its WebAssembly packages are in site/. The launcher listens only on
127.0.0.1, so other computers cannot connect. Normal app startup makes no
internet request. Links to external websites require a connection only if you
deliberately click them. Browser extensions and browser telemetry are controlled
by your browser, not OpenSpecy.

WHY NOT OPEN site/app/index.html DIRECTLY?
Browsers restrict workers, modules, and WebAssembly resources loaded through
file://. The included loopback server supplies the required http:// origin.

VERIFY
offline-manifest.json records the exact package commit, Pages artifact, target,
site tree digest, and source-manifest digests. SHA256SUMS.txt contains a SHA-256
for every bundled file except itself. The GitHub Actions download also includes
a checksum for the target ZIP.
%s`, version, config.PackageSHA, config.Target, definition.GOOS,
		definition.GOARCH, config.PackageSHA, config.SourceRepository,
		config.SourceRunID, launchInstruction, platformNote)
}

func writeChecksums(path string, records []fileRecord) error {
	file, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
	if err != nil {
		return fmt.Errorf("create bundle checksums: %w", err)
	}
	for _, record := range records {
		if err := validatePortableRelativePath(record.Path); err != nil {
			file.Close()
			return fmt.Errorf("invalid checksum path %q: %w", record.Path, err)
		}
		if _, err := fmt.Fprintf(file, "%s  %s\n", record.SHA256, record.Path); err != nil {
			file.Close()
			return fmt.Errorf("write bundle checksums: %w", err)
		}
	}
	if err := file.Close(); err != nil {
		return fmt.Errorf("close bundle checksums: %w", err)
	}
	return nil
}

func writeZip(outputPath, bundleRoot, bundleRootName string) error {
	if err := validatePortableRelativePath(bundleRootName); err != nil {
		return fmt.Errorf("invalid bundle root %q: %w", bundleRootName, err)
	}
	temporary := outputPath + ".partial"
	_ = os.Remove(temporary)
	output, err := os.OpenFile(temporary, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
	if err != nil {
		return fmt.Errorf("create archive: %w", err)
	}
	writer := zip.NewWriter(output)
	seen := make(map[string]string)
	walkErr := filepath.WalkDir(bundleRoot, func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		relative, err := filepath.Rel(bundleRoot, path)
		if err != nil {
			return err
		}
		portableRelative := filepath.ToSlash(relative)
		if relative != "." {
			if err := validatePortableRelativePath(portableRelative); err != nil {
				return fmt.Errorf("invalid archive source path %q: %w", relative, err)
			}
		}
		name := bundleRootName
		if relative != "." {
			name += "/" + portableRelative
		}
		if err := validatePortableRelativePath(name); err != nil {
			return fmt.Errorf("invalid archive entry %q: %w", name, err)
		}
		key := portablePathKey(name)
		if previous, exists := seen[key]; exists {
			return fmt.Errorf("archive entries %q and %q collide on a portable filesystem",
				previous, name)
		}
		seen[key] = name
		info, err := entry.Info()
		if err != nil {
			return err
		}
		header, err := zip.FileInfoHeader(info)
		if err != nil {
			return err
		}
		header.Name = name
		header.Modified = archiveEpoch
		if entry.IsDir() {
			header.Name += "/"
			header.SetMode(0o755 | os.ModeDir)
			_, err = writer.CreateHeader(header)
			return err
		}
		header.SetMode(info.Mode().Perm())
		header.Method = compressionMethod(path)
		archiveFile, err := writer.CreateHeader(header)
		if err != nil {
			return err
		}
		source, err := os.Open(path)
		if err != nil {
			return err
		}
		_, copyErr := io.Copy(archiveFile, source)
		closeErr := source.Close()
		if copyErr != nil {
			return copyErr
		}
		return closeErr
	})
	closeZipErr := writer.Close()
	closeFileErr := output.Close()
	if walkErr != nil {
		_ = os.Remove(temporary)
		return fmt.Errorf("write archive entries: %w", walkErr)
	}
	if closeZipErr != nil {
		_ = os.Remove(temporary)
		return fmt.Errorf("finish archive: %w", closeZipErr)
	}
	if closeFileErr != nil {
		_ = os.Remove(temporary)
		return fmt.Errorf("close archive: %w", closeFileErr)
	}
	if err := os.Rename(temporary, outputPath); err != nil {
		_ = os.Remove(temporary)
		return fmt.Errorf("publish archive: %w", err)
	}
	return nil
}

func compressionMethod(path string) uint16 {
	switch strings.ToLower(filepath.Ext(path)) {
	case ".gz", ".zip", ".png", ".jpg", ".jpeg", ".webp", ".woff", ".woff2", ".wasm", ".rds":
		return zip.Store
	default:
		return zip.Deflate
	}
}

func verifyArchive(path, root, packageSHA, target, launcher string) error {
	reader, err := zip.OpenReader(path)
	if err != nil {
		return err
	}
	defer reader.Close()
	prefix := root + "/"
	entries := make(map[string]*zip.File, len(reader.File))
	seen := make(map[string]string, len(reader.File))
	for _, entry := range reader.File {
		name := strings.TrimSuffix(entry.Name, "/")
		if name == "" || strings.HasSuffix(name, "/") {
			return fmt.Errorf("unsafe or unexpected archive entry %q", entry.Name)
		}
		if err := validatePortableRelativePath(name); err != nil {
			return fmt.Errorf("unsafe or unexpected archive entry %q: %w", entry.Name, err)
		}
		if name != root && !strings.HasPrefix(name, prefix) {
			return fmt.Errorf("unsafe or unexpected archive entry %q", entry.Name)
		}
		key := portablePathKey(name)
		if previous, exists := seen[key]; exists {
			return fmt.Errorf("duplicate or colliding archive entries %q and %q", previous, entry.Name)
		}
		seen[key] = entry.Name
		entries[name] = entry
	}
	required := []string{
		prefix + launcher,
		prefix + readmeName,
		prefix + manifestName,
		prefix + checksumsName,
		prefix + "site/index.html",
		prefix + "site/app/index.html",
		prefix + "site/pkgdown/index.html",
	}
	for _, name := range required {
		if entries[name] == nil {
			return fmt.Errorf("archive is missing %s", name)
		}
	}
	if !strings.HasPrefix(target, "windows-") && entries[prefix+launcher].Mode().Perm()&0o111 == 0 {
		return errors.New("Unix launcher is not executable in the ZIP")
	}
	manifestBytes, err := readZipFile(entries[prefix+manifestName])
	if err != nil {
		return err
	}
	var manifest offlineManifest
	if err := json.Unmarshal(manifestBytes, &manifest); err != nil {
		return fmt.Errorf("decode archived manifest: %w", err)
	}
	if manifest.Package.Commit != packageSHA || manifest.Target.Name != target ||
		manifest.Site.EntryPoint != "site/app/" {
		return errors.New("archived manifest identity does not match requested bundle")
	}
	checksumsBytes, err := readZipFile(entries[prefix+checksumsName])
	if err != nil {
		return err
	}
	expected, err := parseChecksums(checksumsBytes)
	if err != nil {
		return err
	}
	for relative, expectedHash := range expected {
		entry := entries[prefix+relative]
		if entry == nil || entry.FileInfo().IsDir() {
			return fmt.Errorf("checksum references missing archive file %s", relative)
		}
		actualHash, err := hashZipFile(entry)
		if err != nil {
			return err
		}
		if actualHash != expectedHash {
			return fmt.Errorf("checksum mismatch for %s", relative)
		}
	}
	for name, entry := range entries {
		if entry.FileInfo().IsDir() || name == prefix+checksumsName {
			continue
		}
		relative := strings.TrimPrefix(name, prefix)
		if _, ok := expected[relative]; !ok {
			return fmt.Errorf("archive file %s is missing from SHA256SUMS.txt", relative)
		}
	}
	return nil
}

func parseChecksums(content []byte) (map[string]string, error) {
	result := make(map[string]string)
	seen := make(map[string]string)
	scanner := bufio.NewScanner(strings.NewReader(string(content)))
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) < 67 || line[64:66] != "  " {
			return nil, fmt.Errorf("invalid checksum line %q", line)
		}
		hash := line[:64]
		if _, err := hex.DecodeString(hash); err != nil {
			return nil, fmt.Errorf("invalid checksum %q", hash)
		}
		name := line[66:]
		if err := validatePortableRelativePath(name); err != nil {
			return nil, fmt.Errorf("invalid checksum path %q: %w", name, err)
		}
		key := portablePathKey(name)
		if previous, exists := seen[key]; exists {
			return nil, fmt.Errorf("duplicate or colliding checksum paths %q and %q", previous, name)
		}
		seen[key] = name
		result[name] = hash
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return result, nil
}

func validatePortableRelativePath(name string) error {
	if name == "" {
		return errors.New("path is empty")
	}
	if strings.Contains(name, "\\") {
		return errors.New("backslashes are not allowed")
	}
	for _, character := range name {
		if character < 0x20 || character == 0x7f {
			return errors.New("control characters are not allowed")
		}
	}
	if pathpkg.IsAbs(name) {
		return errors.New("absolute paths are not allowed")
	}
	if len(name) >= 2 && ((name[0] >= 'A' && name[0] <= 'Z') ||
		(name[0] >= 'a' && name[0] <= 'z')) && name[1] == ':' {
		return errors.New("Windows drive paths are not allowed")
	}
	if cleaned := pathpkg.Clean(name); cleaned != name || cleaned == "." ||
		cleaned == ".." || strings.HasPrefix(cleaned, "../") {
		return errors.New("path is not clean and relative")
	}
	for _, component := range strings.Split(name, "/") {
		if strings.ContainsAny(component, `<>:"|?*`) {
			return errors.New("path contains characters that are unsafe on Windows")
		}
		if strings.HasSuffix(component, ".") || strings.HasSuffix(component, " ") {
			return errors.New("path components cannot end with a dot or space")
		}
		base := strings.ToLower(strings.SplitN(component, ".", 2)[0])
		if base == "con" || base == "prn" || base == "aux" || base == "nul" ||
			(len(base) == 4 && (strings.HasPrefix(base, "com") ||
				strings.HasPrefix(base, "lpt")) && base[3] >= '1' && base[3] <= '9') {
			return errors.New("path contains a reserved Windows device name")
		}
	}
	return nil
}

func portablePathKey(name string) string {
	return strings.ToLower(name)
}

func verifyArchiveCompanion(archivePath string) error {
	content, err := os.ReadFile(archivePath + ".sha256")
	if err != nil {
		return err
	}
	expected, err := parseChecksums(content)
	if err != nil {
		return err
	}
	if len(expected) != 1 {
		return fmt.Errorf("checksum companion must contain exactly one entry; found %d", len(expected))
	}
	archiveName := filepath.Base(archivePath)
	expectedHash, exists := expected[archiveName]
	if !exists {
		return fmt.Errorf("checksum companion does not name %s", archiveName)
	}
	actualHash, _, err := hashFile(archivePath)
	if err != nil {
		return err
	}
	if actualHash != expectedHash {
		return errors.New("archive checksum companion does not match the ZIP")
	}
	return nil
}

func readZipFile(file *zip.File) ([]byte, error) {
	reader, err := file.Open()
	if err != nil {
		return nil, err
	}
	defer reader.Close()
	return io.ReadAll(reader)
}

func hashZipFile(file *zip.File) (string, error) {
	reader, err := file.Open()
	if err != nil {
		return "", err
	}
	defer reader.Close()
	hash := sha256.New()
	if _, err := io.Copy(hash, reader); err != nil {
		return "", err
	}
	return hex.EncodeToString(hash.Sum(nil)), nil
}
