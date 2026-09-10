package main

import (
	"fmt"
	"os"

	"github.com/wincowgerDEV/OpenSpecy-package/tools/offline/internal/launcher"
)

func main() {
	if err := launcher.Main(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, "OpenSpecy Offline:", err)
		os.Exit(1)
	}
}
