package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/wincowgerDEV/OpenSpecy-package/tools/offline/internal/bundle"
)

func main() {
	var config bundle.Config
	flag.StringVar(&config.ArtifactRoot, "artifact-root", "", "downloaded Pages artifact directory")
	flag.StringVar(&config.Launcher, "launcher", "", "cross-compiled launcher path")
	flag.StringVar(&config.Output, "output", "", "output ZIP path")
	flag.StringVar(&config.PackageSHA, "package-sha", "", "exact 40-character package commit")
	flag.StringVar(&config.Target, "target", "", "supported OS/architecture target")
	flag.StringVar(&config.SourceRepository, "source-repository", "", "owner/repository that built the Pages artifact")
	flag.StringVar(&config.SourceRunID, "source-run-id", "", "successful Pages workflow run ID")
	flag.Parse()
	if flag.NArg() != 0 {
		fmt.Fprintln(os.Stderr, "package-offline: unexpected positional arguments")
		os.Exit(2)
	}
	result, err := bundle.Build(config)
	if err != nil {
		fmt.Fprintln(os.Stderr, "package-offline:", err)
		os.Exit(1)
	}
	fmt.Printf("Created %s (%d files, %d site bytes, sha256 %s)\n",
		result.Archive, result.FileCount, result.SiteBytes, result.ArchiveSHA256)
}
